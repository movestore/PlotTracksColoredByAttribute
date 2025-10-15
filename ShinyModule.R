library(shiny)
library(move2)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(pals)
library(colourpicker)
library(shinycssloaders)
library(htmlwidgets)
library(webshot2)
library(zip)
library(shinybusy)
library(grDevices)

my_data <- readRDS("./data/raw/input3_move2loc_LatLon.rds")
# my_data <- mt_as_move2(readRDS("./data/raw/input2_whitefgeese.rds"))

########### helpers

## helper 1: attribute type
continuous_attr <- function(vals, threshold = 12) {
  is_num <- is.numeric(vals) || inherits(vals, "units")
  if (!is_num) return(FALSE)  # categorical
  n_unique <- length(unique(stats::na.omit(as.numeric(vals))))
  n_unique > threshold        # continuous
}

## helper 2: making segments
make_segments <- function(tracks, attr_name, threshold = 12) {
  if (nrow(tracks) < 2) {
    return(sf::st_sf(track = character(0),
                     value = numeric(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  segs <- mt_segments(tracks)
  id   <- as.character(mt_track_id(tracks))
  vals <- sf::st_drop_geometry(tracks)[[attr_name]]  #Extracts attribute
  
  same_track_next <- c(id[-length(id)] == id[-1], FALSE)
  if (!any(same_track_next)) {
    return(sf::st_sf(track = character(0),
                     value = numeric(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  
  ##continuous: 
  if (continuous_attr(vals, threshold = threshold)) {
    v <- as.numeric(vals)
    seg_val <- (v[same_track_next] + v[which(same_track_next) + 1]) / 2
  } else {
    seg_val <- as.character(vals[same_track_next])  # treat as categorical
  }
  seg_track <- id[which(same_track_next)]
  sf::st_sf(track = seg_track, value = seg_val, geometry = segs[same_track_next])
}

## helper 3: selecting base map
base_map_fun <- function(map, basemap) {
  if (identical(basemap, "TopoMap")) {
    addProviderTiles(map, "Esri.WorldTopoMap")
  } else if (identical(basemap, "Aerial")) {
    addProviderTiles(map, "Esri.WorldImagery")
  } else {
    addTiles(map)
  }
}

## helper 4: legend for categorical attributes
add_cat_legend <- function(map, title, labels, colors, position = "topright") {
  stopifnot(length(labels) == length(colors))
  rows <- paste0(
    mapply(function(col, lab) {
      sprintf(
        "<div style='display:flex;align-items:center;margin:2px 0;'>
           <span style='display:inline-block;width:14px;height:14px;background:%s;
                        border:1px solid rgba(0,0,0,0.25);margin-right:6px;'></span>
           <span>%s</span>
         </div>",
        col, htmltools::htmlEscape(lab)
      )
    }, colors, labels),
    collapse = ""
  )
  box <- sprintf(
    "<div style='background:transparent;padding:6px 8px;border-radius:1px;font-size:11px;'>
       <div style='font-weight:600;margin-bottom:4px;'>%s</div>%s
     </div>",
    htmltools::htmlEscape(title), rows
  )
  leaflet::addControl(map, html = box, position = position)
}

# helper 5: generate HCL colors
color_generator <- function(pal, n, step = NULL) {
  if (n <= 0) return(character(0))
  m <- length(pal)
  
  # If palette not long enough generate n colors
  if (m == 0 || n > m) {
    golden <- 137.50776405003785
    hues   <- ((0:(n - 1)) * golden) %% 360
    return(hcl(h = hues, c = 65, l = 60))
  }
  
  # Otherwise avoid adjacent colors
  if (is.null(step)) step <- max(3L, as.integer(round(m / 4)))
  step <- max(1L, as.integer(step))
  idx  <- ((0:(n - 1)) * step) %% m + 1L
  pal[idx]
}

####### UI 
ui <- fluidPage(
  titlePanel("Tracks colored by attribute"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Animals"),
                 checkboxGroupInput("animals", NULL, choices = NULL),
                 fluidRow(
                   column(6, actionButton("select_all_animals", "Select All Animals", class = "btn-sm")),
                   column(6, actionButton("unselect_animals", "Unselect All Animals", class = "btn-sm"))
                 ),
                 
                 h4("Display"),
                 radioButtons("panel_mode", NULL,
                              choices = c("Single panel","Multipanel"),
                              selected = "Single panel", inline = TRUE),
                 
                 h4("Base map"),
                 radioButtons("basemap", NULL,
                              choices  = c("OpenStreetMap", "TopoMap", "Aerial"),
                              selected = "OpenStreetMap", inline = TRUE),
                 hr(),
                 
                 h4("Attribute"),
                 selectInput("attr", NULL, choices = NULL),
                 div(id = "attr-type-msg", tags$small(textOutput("attr_info"), style = "color:darkblue;")),
                 h6("Note: Numeric attributes with fewer than 12 unique values are considered as categorical."),
                 
                 h4("Colors"),
                 uiOutput("ui_color_controls"),
                 
                 hr(),
                 h4("Style"),
                 fluidRow(
                   column(6, numericInput("linesize_att", "Line width", 3, min = 1, max = 10, step = 1)),
                   column(6, sliderInput("linealpha_att", "Transparency", min = 0, max = 1, value = 0.9, step = 0.05))
                 ),
                 
                 # Option to attach color columns to the data returned to other parts of the app
                 h6("Optional: include the map colors in your data for next use in other apps."),
                 checkboxInput("attach_colors", "Add columns: color (hex) and color_legend", value = FALSE ),
                 
                 hr(),
                 actionButton("apply_btn", "Apply Changes", class = "btn-primary btn-block"),
                 hr(),
                 
                 h4("Download:"),
                 fluidRow(
                   column(6, downloadButton("save_html","Download as HTML", class = "btn-sm")),
                   column(6, downloadButton("save_png", "Save Map as PNG", class = "btn-sm"))
                 ),
                 
                 #### TEST-start
                 hr(),
                 downloadButton("dl_colors_csv", "Download colors CSV (test)")
                 #### TEST  — end
    ),
    mainPanel(uiOutput("maps_ui"))
  )
)

### server 
server <- function(input, output, session) {
  
  # Locked so that only change on clicking on button
  locked_settings <- reactiveVal(NULL)
  locked_mv       <- reactiveVal(NULL)
  locked_attach   <- reactiveVal(FALSE)
  
  # keep tracks with at least 2 
  mv_all <- reactive({
    my_data %>%
      arrange(mt_track_id(), mt_time()) %>%
      { .[!duplicated(data.frame(id = mt_track_id(.), t = mt_time(.))), ] } %>%
      group_by(track = mt_track_id()) %>%
      filter(n() >= 2) %>%
      ungroup()
  })
  
  observeEvent(input$select_all_animals, {
    ids <- as.character(unique(mt_track_id(mv_all())))
    updateCheckboxGroupInput(session, "animals", selected = ids)
  })
  observeEvent(input$unselect_animals, {
    updateCheckboxGroupInput(session, "animals", selected = character(0))
  })
  observe({
    ids <- as.character(unique(mt_track_id(mv_all())))
    updateCheckboxGroupInput(session, "animals", choices = ids, selected = ids)
  })
  observe({
    dd <- sf::st_drop_geometry(mv_all()) |> as.data.frame()
    keep <- colSums(!is.na(dd)) > 0
    keep <- keep & !sapply(dd, inherits, what = "POSIXt")
    keep <- keep & (sapply(dd, class) != "Date")
    if (!any(keep)) keep[["track"]] <- TRUE
    choices <- names(dd)[keep]
    updateSelectInput(session, "attr", choices = choices, selected = choices[1])
  })
  
  # live selection
  mv_sel <- reactive({
    mv <- mv_all()
    sel <- input$animals
    if (is.null(sel) || length(sel) == 0) return(mv[0, ])
    mv[as.character(mt_track_id(mv)) %in% sel, ] %>%
      arrange(mt_track_id(), mt_time())
  })
  
  # first time shows map 
  observe({
    mv <- mv_sel()
    if (!is.null(input$attr) && nrow(mv) > 0 &&
        is.null(locked_mv()) && is.null(locked_settings())) {
      locked_mv(mv)
      locked_settings(list(
        animals   = input$animals,
        panel_mode= input$panel_mode,
        basemap   = input$basemap,
        attr      = input$attr,
        linesize  = input$linesize_att,
        linealpha = input$linealpha_att,
        col_low   = input$col_low,
        col_high  = input$col_high,
        cat_pal   = input$cat_pal
      ))
      locked_attach(isTRUE(input$attach_colors)) 
    }
  })
  
  # update locked state when Apply button is clicked
  observeEvent(input$apply_btn, {
    locked_mv(mv_sel())
    locked_settings(list(
      animals   = input$animals,
      panel_mode= input$panel_mode,
      basemap   = input$basemap,
      attr      = input$attr,
      linesize  = input$linesize_att,
      linealpha = input$linealpha_att,
      col_low   = input$col_low,
      col_high  = input$col_high,
      cat_pal   = input$cat_pal
    ))
    locked_attach(isTRUE(input$attach_colors))
  }, ignoreInit = TRUE)
  
  attribute_type_live <- reactive({
    req(input$attr)
    mv <- mv_sel()
    if (is.null(mv) || nrow(mv) == 0) return(list(empty = TRUE, is_cont = TRUE))
    vals <- sf::st_drop_geometry(mv)[[input$attr]]
    list(empty = FALSE, is_cont = continuous_attr(vals, threshold = 12))
  })
  
  output$attr_info <- renderText({
    at <- attribute_type_live()
    if (at$is_cont) "Selected attribute is Continuous" else "Selected attribute is Categorical"
  })
  
  output$ui_color_controls <- renderUI({
    at <- attribute_type_live()
    if (isTRUE(at$empty)) return(helpText("Select animals to choose colors."))
    if (isTRUE(at$is_cont)) {
      tagList(
        fluidRow(
          column(6, colourpicker::colourInput("col_low",  "Low",
                                              if (is.null(isolate(input$col_low)))  "yellow" else isolate(input$col_low))),
          column(6, colourpicker::colourInput("col_high", "High",
                                              if (is.null(isolate(input$col_high))) "blue"   else isolate(input$col_high)))
        )
      )
    } else {
      selectInput("cat_pal", "Palette",
                  choices  = c("Set2","Set3","Dark2","Paired","Accent","Glasbey"),
                  selected = if (is.null(isolate(input$cat_pal))) "Dark2" else isolate(input$cat_pal))
    }
  })
  
  # segments for first locked selection
  segs_all <- reactive({
    s  <- locked_settings(); mv <- locked_mv(); req(s, mv, s$attr)
    segs <- make_segments(mv, s$attr, threshold = 12)
    validate(need(nrow(segs) > 0, "No segments for selected animals."))
    segs
  })
  
  # palette for locked selection
  pal_info <- reactive({
    s <- locked_settings()
    segs <- segs_all()
    req(s, segs)
    vals <- segs$value
    is_cont <- continuous_attr(vals, threshold = 12)
    
    if (is_cont) {
      low  <- if (is.null(s$col_low))  "yellow" else s$col_low
      high <- if (is.null(s$col_high)) "blue"   else s$col_high
      
      # Use full locked dataset for domain so nothing is NA due to range
      all_vals <- as.numeric(sf::st_drop_geometry(locked_mv())[[s$attr]])
      rng      <- range(all_vals, na.rm = TRUE)
      
      pal  <- colorNumeric(colorRampPalette(c(low, high))(256), domain = rng, na.color = NA)
      list(pal = pal, legend_vals = rng, is_cont = TRUE)
    } else {
      # categorical
      levs  <- sort(unique(stats::na.omit(as.character(vals))))
      n     <- length(levs)
      pname <- if (is.null(s$cat_pal)) "Dark2" else s$cat_pal
      
      if (tolower(pname) == "glasbey") {
        base <- pals::glasbey(32)
      } else {
        maxn <- RColorBrewer::brewer.pal.info[pname, "maxcolors"]
        base <- RColorBrewer::brewer.pal(maxn, pname)
      }
      
      if (n <= length(base)) {
        cols <- base[seq_len(n)]
      } else {
        cols <- color_generator(base, n)
      }
      
      pal <- colorFactor(cols, domain = levs, na.color = NA)
      list(pal = pal, legend_vals = levs, cols = cols, is_cont = FALSE)
    }
  })
  
  ### add color column and color_legend_attr to data
  mv_with_colors <- reactive({
    s <- locked_settings()
    mv <- locked_mv()
    p  <- pal_info()
    req(s, mv, p)
    
    vals <- sf::st_drop_geometry(mv)[[s$attr]]
    hex  <- if (p$is_cont) p$pal(as.numeric(vals)) else p$pal(as.character(vals))
    
    mv$color <- as.character(hex)
    cname <- paste0("color_legend_", s$attr)
    mv[[cname]] <- vals
    mv
  })
  
  # If the checkbox is ticked, it includes the two new columns.
  mv_current <- reactive({
    if (isTRUE(locked_attach())) mv_with_colors() else locked_mv()
  })
  
  # build a leaflet map 
  leaflet_map <- function(track_id = NULL) {
    s <- locked_settings()
    segs <- segs_all()
    pinfo <- pal_info()
    req(s, segs, pinfo)
    
    # subset for multipanel
    if (!is.null(track_id)) {
      segs <- segs[segs$track == track_id, , drop = FALSE]
      validate(need(nrow(segs) > 0, "No data for this animal."))
    }
    
    color_selection <- if (pinfo$is_cont) {
      ~pinfo$pal(as.numeric(value))
    } else {
      ~pinfo$pal(as.character(value))
    }
    
    bb <- as.vector(sf::st_bbox(segs))
    m <- leaflet(options = leafletOptions(minZoom = 2, preferCanvas = TRUE)) %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4])
    m <- base_map_fun(m, s$basemap)
    
    m <- m %>%
      addScaleBar(position = "topleft") %>%
      addPolylines(data = segs, weight = s$linesize, opacity = s$linealpha, color  = color_selection, smoothFactor = 1)
    
    # add legend 
    if (pinfo$is_cont) {
      m <- m %>% addLegend("topright", pal = pinfo$pal, values = pinfo$legend_vals, title = s$attr, opacity = 1, className = "tiny-legend")
    } else {
      m <- add_cat_legend(m, title  = s$attr, labels = pinfo$legend_vals, colors = pinfo$cols, position = "topright")
    }
    
    m
  }
  
  #### layout 
  output$maps_ui <- renderUI({
    s <- locked_settings()
    if (is.null(s)) return(div("Loading…"))
    ids <- s$animals
    if (is.null(ids) || length(ids) == 0)
      return(div(style="color:red; font-weight:700; padding:10px;",
                 "Please select one or more animals."))
    if (identical(s$panel_mode, "Single panel")) {
      return(withSpinner(leafletOutput("map_single", height = "85vh"), type = 4, color = "blue", size = 0.9))
    }
    width <- 6
    cols <- lapply(seq_along(ids), function(i) {
      content <- tagList(
        tags$h5(paste("Animal:", ids[i]),
                style = "text-align: center; margin-top: 5px; margin-bottom: 5px;"),
        withSpinner(leafletOutput(paste0("map_", ids[i]), height = "45vh"), type = 4, color = "blue", size = 0.9)
      )
      column(width, content)
    })
    rows <- lapply(split(cols, ceiling(seq_along(cols) / 2)), function(chunk) do.call(fluidRow, chunk))
    tagList(rows)
  })
  
  ## single panel
  output$map_single <- renderLeaflet({
    validate(need(!is.null(locked_settings()) && !is.null(locked_mv()), "Loading…"))
    leaflet_map()
  })
  
  observe({
    s <- locked_settings()
    req(s, identical(s$panel_mode, "Multipanel"))
    ids <- s$animals; if (is.null(ids) || length(ids) == 0) return()
    
    lapply(ids, function(id_i){
      local({
        id_loc <- id_i
        output[[paste0("map_", id_loc)]] <- renderLeaflet({
          validate(need(!is.null(locked_settings()) && !is.null(locked_mv()), "Loading…"))
          leaflet_map(track_id = id_loc)
        })
      })
    })
  })
  
  ###### downloads part ######
  
  # download map as html
  output$save_html <- downloadHandler(
    filename = function() {
      s <- locked_settings(); req(s)
      if (identical(s$panel_mode, "Multipanel")) paste0("Plots_HTML_", Sys.Date(), ".zip")
      else                                       paste0("Plots_HTML_", Sys.Date(), ".html")
    },
    content = function(file) {
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Saving HTML…")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      
      s <- locked_settings(); req(s)
      
      if (!identical(s$panel_mode, "Multipanel")) {
        htmlwidgets::saveWidget(leaflet_map(), file = file, selfcontained = TRUE)
        return(invisible())
      }
      
      ids <- s$animals; req(length(ids) > 0)
      td <- tempfile("tracks_html_"); dir.create(td)
      for (id in ids) {
        out <- file.path(td, paste0(id, "_", Sys.Date(), ".html"))
        htmlwidgets::saveWidget(leaflet_map(track_id = id), file = out, selfcontained = TRUE, libdir = NULL)
      }
      files <- list.files(td, pattern = "\\.html$", recursive = FALSE)
      zip::zipr(zipfile = file, files = files, root = td)
    }
  )
  
  # download map as png
  output$save_png <- downloadHandler(
    filename = function() {
      s <- locked_settings(); req(s)
      if (identical(s$panel_mode, "Multipanel")) paste0("Plots_PNG_", Sys.Date(), ".zip")
      else                                       paste0("Plots_PNG_", Sys.Date(), ".png")
    },
    content = function(file) {
      shinybusy::show_modal_spinner(spin = "fading-circle", text = "Saving PNG…")
      on.exit(shinybusy::remove_modal_spinner(), add = TRUE)
      
      s <- locked_settings(); req(s)
      
      # single
      if (!identical(s$panel_mode, "Multipanel")) {
        tf  <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(leaflet_map(), tf, selfcontained = TRUE)
        url <- if (.Platform$OS.type == "windows")
          paste0("file:///", gsub("\\\\", "/", normalizePath(tf))) else tf
        webshot2::webshot(url, file, vwidth = 1400, vheight = 900, delay = 1)
        return(invisible())
      }
      
      # multi
      ids <- s$animals; req(length(ids) > 0)
      td <- tempfile("tracks_png_"); dir.create(td)
      for (id in ids) {
        tf  <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(leaflet_map(track_id = id), tf, selfcontained = TRUE)
        url <- if (.Platform$OS.type == "windows")
          paste0("file:///", gsub("\\\\", "/", normalizePath(tf))) else tf
        out <- file.path(td, paste0(id, "_", Sys.Date(), ".png"))
        webshot2::webshot(url, out, vwidth = 1400, vheight = 900, delay = 1)
      }
      files <- list.files(td, recursive = FALSE)
      zip::zipr(zipfile = file, files = files, root = td)
    }
  )
  
  #### TEST  — CSV of color columns ####
  output$dl_colors_csv <- downloadHandler(
    filename = function() paste0("colors_", Sys.Date(), ".csv"),
    content  = function(file) {
      s  <- locked_settings(); req(s)
      mv <- mv_with_colors();  req(mv)
      cname <- paste0("color_legend_", s$attr)
      df <- data.frame(
        track_id  = as.character(mt_track_id(mv)),
        color_hex = as.character(mv$color),
        stringsAsFactors = FALSE
      )
      df[[cname]] <- sf::st_drop_geometry(mv)[[cname]]
      df <- df[, c("track_id", "color_hex", cname)]
      df <- unique(df)
      utils::write.csv(df, file, row.names = FALSE)
    }
  )
  #### TEST end ####
}

shinyApp(ui, server)
