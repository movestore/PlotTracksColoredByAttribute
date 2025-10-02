library(shiny)
library(move2)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(pals)
library(colourpicker)
library(shinycssloaders)  


#my_data <- readRDS("./data/raw/input2_move2loc_LatLon.rds")
my_data <- mt_as_move2(readRDS("./data/raw/input2_whitefgeese.rds"))

####### helpers #######

# helper 1: make segments
make_segments <- function(tracks, attr_name) {
  if (nrow(tracks) < 2) {
    return(sf::st_sf(value = numeric(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  segs <- mt_segments(tracks)
  id   <- as.character(mt_track_id(tracks))
  vals <- sf::st_drop_geometry(tracks)[[attr_name]]
  same_track_next <- c(id[-length(id)] == id[-1], FALSE)
  if (!any(same_track_next)) {
    return(sf::st_sf(value = numeric(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  seg_val <- if (is.numeric(vals) || inherits(vals, "units")) {
    (as.numeric(vals[same_track_next]) + as.numeric(vals[which(same_track_next) + 1])) / 2
  } else {
    as.character(vals[same_track_next])
  }
  sf::st_sf(value = seg_val, geometry = segs[same_track_next])
}

# helper 2: basemap
base_map_fun <- function(map, basemap) {
  if (identical(basemap, "TopoMap")) {
    addProviderTiles(map, "Esri.WorldTopoMap")
  } else if (identical(basemap, "Aerial")) {
    addProviderTiles(map, "Esri.WorldImagery")
  } else {
    addTiles(map) 
  }
}

####### UI #######
ui <- fluidPage(
  titlePanel("Tracks colored by attribute"),
  tags$style(HTML("
      .tiny-legend { font-size: 11px !important; line-height: 0.9; }
      .tiny-legend .leaflet-control { padding: 5px 5px; }
      .tiny-legend .legend-title { margin-bottom: 2px; }
  ")),
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("Animals"),
                 checkboxGroupInput("animals", NULL, choices = NULL),
                 fluidRow(
                   column(6,actionButton("select_all_animals", "Select All Animals",  class = "btn-sm") ),
                   column(6,actionButton("unselect_animals",  "Unselect All Animals", class = "btn-sm"))
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
                 h4("Colors"),
                 uiOutput("ui_color_controls"),
                 hr(),
                 h4("Style"),
                 fluidRow(
                   column(6, numericInput("linesize_att", "Line width", 3, min = 1, max = 10, step = 1)),
                   column(6, sliderInput("linealpha_att", "Transparency", min = 0, max = 1, value = 0.9, step = 0.05))
                 ),
                 hr(),
                 actionButton("apply_btn", "Apply Changes", class = "btn-primary btn-block")
    ),
    mainPanel(
      uiOutput("maps_ui")
    )
  )
)

####### server #######
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
  
  # update locked
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
  
  # attribute type (live)
  attribute_type_live <- reactive({
    req(input$attr)
    mv <- mv_sel()
    if (is.null(mv) || nrow(mv) == 0) {
      return(list(empty = TRUE, is_cont = TRUE))
    }
    vals <- sf::st_drop_geometry(mv)[[input$attr]]
    n_unique <- length(unique(stats::na.omit(vals)))
    is_cont <- is.numeric(vals) || inherits(vals, "units") || n_unique > 10
    list(empty = FALSE, is_cont = is_cont)
  })
  
  # Show message 
  output$attr_info <- renderText({
    at <- attribute_type_live()
    if (isTRUE(at$empty)) return("Please select one or more animals.")
    if (at$is_cont) "Selected attribute is Continuous" else "Selected attribute is Categorical"
  })
  
  # Show color controls 
  output$ui_color_controls <- renderUI({
    at <- attribute_type_live()
    if (isTRUE(at$empty)) {
      return(helpText("Select animals to choose colors."))
    }
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
                  choices = c("Set2","Set3","Dark2","Paired","Accent","Glasbey"),
                  selected = if (is.null(isolate(input$cat_pal))) "Set2" else isolate(input$cat_pal))
    }
  })
  
  #leaflet map 
  leaflet_map <- function(mv_subset) {
    s <- locked_settings(); req(s)
    req(mv_subset, nrow(mv_subset) > 0, s$attr)
    
    segs <- make_segments(mv_subset, s$attr)
    validate(need(nrow(segs) > 0, "No line segments for selected animals."))
    
    vals_locked <- sf::st_drop_geometry(mv_subset)[[s$attr]]
    n_unique <- length(unique(stats::na.omit(vals_locked)))
    is_cont_locked <- is.numeric(vals_locked) || inherits(vals_locked, "units") || n_unique > 10
    
    if (is_cont_locked) {
      low  <- if (is.null(s$col_low))  "yellow" else s$col_low
      high <- if (is.null(s$col_high)) "blue"   else s$col_high
      rng  <- range(as.numeric(segs$value), na.rm = TRUE)
      pal  <- colorNumeric(colorRampPalette(c(low, high))(256),
                           domain = rng, na.color = NA)
      color_selection <- ~pal(as.numeric(value))
      legend_vals <- rng
    } else {
      levs <- levels(factor(segs$value))
      pname <- if (is.null(s$cat_pal)) "Set2" else s$cat_pal
      cols <- if (tolower(pname) == "glasbey") {
        pals::glasbey(length(levs))
      } else {
        maxn <- RColorBrewer::brewer.pal.info[pname, "maxcolors"]
        base <- RColorBrewer::brewer.pal(min(maxn, max(3, length(levs))), pname)
        base[seq_len(length(levs))]
      }
      pal <- colorFactor(cols, domain = levs, na.color = NA)
      color_selection <- ~pal(as.character(value))
      legend_vals <- levs
    }
    
    bb <- as.vector(sf::st_bbox(segs))
    m <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4])
    m <- base_map_fun(m, s$basemap)
    m %>%
      addScaleBar(position = "topleft") %>%
      addPolylines(data = segs,
                   weight = s$linesize,
                   opacity = s$linealpha,
                   color  = color_selection) %>%
      addLegend("bottomright", pal = pal, values = legend_vals,
                title = s$attr, opacity = 1, className = "tiny-legend")
  }
  
  
  # Layout 
  output$maps_ui <- renderUI({
    s <- locked_settings()
    if (is.null(s)) return(div("Pleas Wait. It is Loading…"))
    ids <- s$animals
    if (is.null(ids) || length(ids) == 0)
      return(div(style="color:red; font-weight:700; padding:10px;",
                 "Please select one or more animals."))
    if (identical(s$panel_mode, "Single panel")) {
      #return(leafletOutput("map_single", height = "85vh"))
      return(  withSpinner( leafletOutput("map_single", height = "85vh"), type = 4, color = "blue",  size = 0.9  )  )
    }
    width <- 6
    cols <- lapply(seq_along(ids), function(i) {
      content <- tagList(
        tags$h5(paste("Animal:", ids[i]),
                style = "text-align: center; margin-top: 5px; margin-bottom: 5px;"),
        #leafletOutput(paste0("map_", ids[i]), height = "45vh")
        withSpinner(leafletOutput(paste0("map_", ids[i]), height = "45vh"), type = 4,  color = "blue",  size = 0.9  )
      )
      column(width, content)
    })
    rows <- lapply(split(cols, ceiling(seq_along(cols) / 2)), function(chunk) {
      do.call(fluidRow, chunk)
    })
    tagList(rows)
  })
  
  # Render maps 
  output$map_single <- renderLeaflet({
    validate(need(!is.null(locked_settings()) && !is.null(locked_mv()), "Loading…"))
    leaflet_map(locked_mv())   
  })
  
  observe({
    s <- locked_settings()
    req(s, identical(s$panel_mode, "Multipanel"))
    ids <- s$animals; if (is.null(ids) || length(ids) == 0) return()
    mv_all_locked <- locked_mv(); req(mv_all_locked)
    
    lapply(ids, function(id_i){
      local({
        id_loc <- id_i
        output[[paste0("map_", id_loc)]] <- renderLeaflet({
          validate(need(!is.null(locked_settings()) && !is.null(locked_mv()), "Loading…"))
          mv_id <- mv_all_locked[as.character(mt_track_id(mv_all_locked)) == id_loc, ]
          validate(need(nrow(mv_id) > 0, "No data for this animal."))
          leaflet_map(mv_id)  
        })
      })
    })
  })
}

shinyApp(ui, server)
