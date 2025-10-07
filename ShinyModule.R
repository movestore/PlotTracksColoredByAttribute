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

my_data <- readRDS("./data/raw/input2_move2loc_LatLon.rds")
#my_data <- mt_as_move2(readRDS("./data/raw/input2_whitefgeese.rds"))

########### helpers

##helper 1: attribute type
attr_is_continuous <- function(vals, threshold = 12) {
  is_num <- is.numeric(vals) || inherits(vals, "units")
  if (!is_num) return(FALSE)  #categorical
  n_unique <- length(unique(stats::na.omit(as.numeric(vals))))
  n_unique > threshold        #  continuous
}

##helper2: making segments
make_segments <- function(tracks, attr_name, threshold = 12) {
  if (nrow(tracks) < 2) {
    return(sf::st_sf(track = character(0),
                     value = numeric(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  segs <- mt_segments(tracks)
  id   <- as.character(mt_track_id(tracks))
  vals <- sf::st_drop_geometry(tracks)[[attr_name]]
  
  same_track_next <- c(id[-length(id)] == id[-1], FALSE)
  if (!any(same_track_next)) {
    return(sf::st_sf(track = character(0),
                     value = numeric(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  
  if (attr_is_continuous(vals, threshold = threshold)) {
    v <- as.numeric(vals)
    seg_val <- (v[same_track_next] + v[which(same_track_next) + 1]) / 2
  } else {
    seg_val <- as.character(vals[same_track_next])  # treat as categorical
  }
  seg_track <- id[which(same_track_next)]
  sf::st_sf(track = seg_track, value = seg_val, geometry = segs[same_track_next])
}

##helper 3: selecting base map
base_map_fun <- function(map, basemap) {
  if (identical(basemap, "TopoMap")) {
    addProviderTiles(map, "Esri.WorldTopoMap")
  } else if (identical(basemap, "Aerial")) {
    addProviderTiles(map, "Esri.WorldImagery")
  } else {
    addTiles(map)
  }
}



####### UI 
ui <- fluidPage(
  titlePanel("Tracks colored by attribute"),
  tags$style(HTML("
  .tiny-legend { font-size: 11px !important; line-height: 1.1; }
  
  .tiny-legend i {
    width: 14px !important;
    height: 14px !important;
    display: inline-block !important;
    margin-right: 6px !important;
    opacity: 1 !important;               
    border: 1px solid rgba(0,0,0,0.25);   
  }
")),
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
                 h6("Note: Numeric attributes with fewer than 12 unique values are consider as categorical."),
                 h4("Colors"),
                 uiOutput("ui_color_controls"),
                 hr(),
                 h4("Style"),
                 fluidRow(
                   column(6, numericInput("linesize_att", "Line width", 3, min = 1, max = 10, step = 1)),
                   column(6, sliderInput("linealpha_att", "Transparency", min = 0, max = 1, value = 0.9, step = 0.05))
                 ),
                 hr(),
                 actionButton("apply_btn", "Apply Changes", class = "btn-primary btn-block"),
                 hr(),
                 h4("Download:"),
                 fluidRow(
                   column(6, downloadButton("save_html","Download as HTML", class = "btn-sm")),
                   column(6, downloadButton("save_png", "Save Map as PNG", class = "btn-sm"))
                 )
    ),
    mainPanel(uiOutput("maps_ui"))
  )
)

### server 
server <- function(input, output, session) {
  
  # Locked so that only change on clicking on button
  locked_settings <- reactiveVal(NULL)
  locked_mv       <- reactiveVal(NULL)
  
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
  }, ignoreInit = TRUE)
  
  attribute_type_live <- reactive({
    req(input$attr)
    mv <- mv_sel()
    if (is.null(mv) || nrow(mv) == 0) return(list(empty = TRUE, is_cont = TRUE))
    vals <- sf::st_drop_geometry(mv)[[input$attr]]
    list(empty = FALSE, is_cont = attr_is_continuous(vals, threshold = 12))
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
          column(6, colourInput("col_low",  "Low",
                                if (is.null(isolate(input$col_low)))  "yellow" else isolate(input$col_low))),
          column(6, colourInput("col_high", "High",
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
    is_cont <- attr_is_continuous(vals, threshold = 12)
    
    if (is_cont) {
      low  <- if (is.null(s$col_low))  "yellow" else s$col_low
      high <- if (is.null(s$col_high)) "blue"   else s$col_high
      rng  <- range(as.numeric(vals), na.rm = TRUE)
      pal  <- colorNumeric(colorRampPalette(c(low, high))(256), domain = rng, na.color = NA)
      list(pal = pal, legend_vals = rng, is_cont = TRUE)
    } else {
      # categorical
      levs <- sort(unique(stats::na.omit(as.character(vals))))
      pname <- if (is.null(s$cat_pal)) "Dark2" else s$cat_pal
      
      cols <- if (tolower(pname) == "glasbey") {
        pals::glasbey(length(levs))
      } else {
        maxn <- RColorBrewer::brewer.pal.info[pname, "maxcolors"]
        base <- RColorBrewer::brewer.pal(maxn, pname)
        if (length(levs) <= maxn) {
          base[seq_len(length(levs))]
        } else {
          colorRampPalette(base)(length(levs))
        }
      }
      
      pal <- colorFactor(cols, domain = levs, na.color = NA)
      list(pal = pal, legend_vals = levs, is_cont = FALSE)
    }
  })
  
  # build a leaflet map 
  leaflet_map <- function(track_id = NULL) {
    s <- locked_settings()
    segs <- segs_all()
    pinfo <- pal_info()
    req(s, segs, pinfo)
    
    # filter by track for multipanel
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
    m %>%
      addScaleBar(position = "topleft") %>%
      addPolylines(data = segs,
                   weight = s$linesize,
                   opacity = s$linealpha,
                   color  = color_selection,
                   smoothFactor = 1) %>%
      addLegend("topright", pal = pinfo$pal, values = pinfo$legend_vals,
                title = s$attr, opacity = 1, className = "tiny-legend")
  }
  
  #### layout 
  output$maps_ui <- renderUI({
    s <- locked_settings()
    if (is.null(s)) return(div("Pleas Wait. It is Loading…"))
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
  
  ### downloads
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
        htmlwidgets::saveWidget(leaflet_map(), tf, selfcontained = TRUE, libdir = NULL)
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
        htmlwidgets::saveWidget(leaflet_map(track_id = id), tf, selfcontained = TRUE, libdir = NULL)
        url <- if (.Platform$OS.type == "windows")
          paste0("file:///", gsub("\\\\", "/", normalizePath(tf))) else tf
        out <- file.path(td, paste0(id, "_", Sys.Date(), ".png"))
        webshot2::webshot(url, out, vwidth = 1400, vheight = 900, delay = 1)
      }
      files <- list.files(td, recursive = FALSE)
      zip::zipr(zipfile = file, files = files, root = td)
    }
  )
}

shinyApp(ui, server)
