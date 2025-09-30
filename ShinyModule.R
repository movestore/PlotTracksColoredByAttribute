library(shiny)
library(move2)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(pals)
library(colourpicker)

my_data <- readRDS("./data/raw/input2_move2loc_LatLon.rds")

######### helpers:

### helper 1 : making segments #####
make_segments <- function(tracks, attr_name) {
  if (nrow(tracks) < 2) {
    return(sf::st_sf(value = numeric(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  
  # segments
  segs <- mt_segments(tracks)
  
  id   <- as.character(mt_track_id(tracks))
  vals <- sf::st_drop_geometry(tracks)[[attr_name]]
  
  # keep only pairs
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
  
  sf::st_sf(
    value    = seg_val,
    geometry = segs[same_track_next]
  )
}

## helper 2: line format #####
line_type <- function(x) {
  switch(x,
         "solid"    = NULL,      
         "dashed"   = "10,10",
         "dotted"   = "2,8",
         "dotdash"  = "10,6,2,6",
         "longdash" = "20,10",
         "twodash"  = "15,8,5,8",
         NULL
  )
}



#####  UI 
ui <- fluidPage(
  titlePanel("Tracks colored by attribute"),
  
  tags$style(HTML("
      .tiny-legend { font-size: 11px !important; line-height: 0.9; }
      .tiny-legend .leaflet-control { padding: 5px 5px; }
      .tiny-legend .legend-title { margin-bottom: 2px; }
    ")),
  
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Animals"),
                 fluidRow(
                   column(8,checkboxGroupInput("animals", NULL, choices = NULL)),        
                   column(4, actionButton("select_all_animals", "Select All",  class = "btn-sm"),actionButton("unselect_animals",  "Unselect All", class = "btn-sm"))
                 ),
                 
                 hr(),
                 
                 h4("Display"),
                 radioButtons("panel_mode", NULL,
                              choices = c("Single panel","Multipanel"),
                              selected = "Single panel", inline = TRUE),
                 
                 h4("Base map"),
                 radioButtons(
                   "basemap", NULL,
                   choices  = c("OpenStreetMap", "TopoMap", "Aerial"),
                   selected = "OpenStreetMap",
                   inline   = TRUE
                 ),
                 
                 
                 hr(),
                 h4("Attribute"),
                 selectInput("attr", NULL, choices = NULL),
                 div(id = "attr-type-msg", tags$small(textOutput("attr_info"), style = "color:darkblue;")),
                 h4("Colors"),
                 uiOutput("ui_color_controls"),   
                 hr(),
                 
                 h4("Style"),
                 fluidRow(
                   column(4, selectInput("linetype_att","Line type",choices = c("solid","dashed","dotted","dotdash","longdash","twodash"), selected = "solid" )),
                   column(4, numericInput("linesize_att", "Line width", 3, min = 1, max = 10, step = 1)), 
                   column(4, sliderInput("linealpha_att", "Line alpha", min = 0, max = 1, value = 0.9, step = 0.05)) 
                   
                 ),
                 
    ),
    mainPanel(
      #leafletOutput("map", height = "85vh")
      uiOutput("maps_ui")
    )
  )
)

########## server######

server <- function(input, output, session) {
  
  
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
  
  mv_sel <- reactive({
    #req(input$animals)
    mv <- mv_all()
    sel <- input$animals
    
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Please select one or more animals.", type = "warning", duration = 3)
      return(mv[0, ])  
    }
    
    mv[as.character(mt_track_id(mv)) %in% sel, ] %>%
      arrange(mt_track_id(), mt_time())
  })
  
  
  
  
  ### Attribute type
  max_level <- 10
  
  attribute_type <- reactive({
    req(input$attr)
    
    mv <- mv_sel()
    # If no animals selected
    if (is.null(mv) || nrow(mv) == 0) {
      return(list(n_unique = 0, is_cont = TRUE, empty = TRUE))
    }
    vals <- sf::st_drop_geometry(mv)[[input$attr]]
    n_unique <- length(unique(stats::na.omit(vals)))
    is_cont <- is.numeric(vals) || inherits(vals, "units") || n_unique > max_level
    list(n_unique = n_unique, is_cont = is_cont , empty = FALSE)
  })
  
  output$attr_info <- renderText({
    req(attribute_type())
    at <- attribute_type()
    
    if (isTRUE(at$empty)) return("Please select one or more animals.")
    if (at$is_cont) "Selected attribute is Continuous" else "Selected attribute is Categorical"
  })
  
  
  # color selection
  output$ui_color_controls <- renderUI({
    at <- attribute_type()
    if (isTRUE(at$is_cont)) {
      tagList(
        fluidRow(
          column(6, colourInput("col_low",  "Low",  "yellow")),
          column(6, colourInput("col_high", "High", "blue"))
        )
      )
    } else {
      selectInput("cat_pal", "Palette",
                  choices = c("Set2","Set3","Dark2","Paired","Accent","Glasbey"),
                  selected = "Set2")
    }
  })
  
  
  
  # segments 
  #  single-map
  leaflet_map <- function(single_indiv){
    
    segs <- make_segments(single_indiv, input$attr)
    validate(need(nrow(segs) > 0, "No line segments for selected animals."))
    
    at <- attribute_type() 
    
    if (isTRUE(at$is_cont)) {
      low  <- if (is.null(input$col_low))  "yellow" else input$col_low
      high <- if (is.null(input$col_high)) "blue" else input$col_high
      pal  <- colorNumeric(colorRampPalette(c(low, high))(256),
                           domain = as.numeric(segs$value), na.color = NA)
      legend_vals <- range(as.numeric(segs$value), na.rm = TRUE)
      color_selection   <- ~pal(as.numeric(value))
      
    } else {
      levs <- levels(factor(segs$value))
      pname <- if (is.null(input$cat_pal)) "Set2" else input$cat_pal
      cols <- if (tolower(pname) == "glasbey") {
        pals::glasbey(length(levs))
      } else {
        maxn <- RColorBrewer::brewer.pal.info[pname, "maxcolors"]
        base <- RColorBrewer::brewer.pal(min(maxn, max(3, length(levs))), pname)
        base[seq_len(length(levs))]
      }
      
      pal <- colorFactor(cols, domain = levs, na.color = NA)
      legend_vals <- levs
      color_selection   <- ~pal(as.character(value))
    }
    
    bb <- as.vector(sf::st_bbox(segs))
    
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addTiles() %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4]) %>%
      
      # addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      # addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      # addTiles(group = "OpenStreetMap") %>%
      # addScaleBar(position = "topleft") %>%
      # addLayersControl(
      #   baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),
      #   options = layersControlOptions(collapsed = FALSE)
      # ) %>%
      
      {
        if (identical(input$basemap, "TopoMap")) {
          addProviderTiles(., "Esri.WorldTopoMap")
        } else if (identical(input$basemap, "Aerial")) {
          addProviderTiles(., "Esri.WorldImagery")
        } else {
          addTiles(.)
        }
      } %>%
      addScaleBar(position = "topleft") %>%
      
    
      addPolylines(data = segs,
                   weight = input$linesize_att,
                   opacity = input$linealpha_att,
                   dashArray = line_type(input$linetype_att),
                   color = color_selection) %>%
      addLegend("bottomright", pal = pal, values = legend_vals,
                title = input$attr, opacity = 1, className = "tiny-legend")
  }
  
  # dynamic panel 
  output$maps_ui <- renderUI({
    ids <- input$animals
    if (is.null(ids) || length(ids) == 0)
      return(div(style="color:red; font-weight:700; padding:10px;",
                 "Please select one or more animals."))
    
    if (identical(input$panel_mode, "Single panel")) {
      return(leafletOutput("map_single", height = "85vh"))
    }
    
    
  # Multipanel setting
  n <- length(ids)
  width <- 6  
  
  cols <- lapply(seq_along(ids), function(i) {
    column(width, leafletOutput(paste0("map_", ids[i]), height = "45vh"))
  })
  
  rows <- lapply(split(cols, ceiling(seq_along(cols) / 2)), function(chunk) {
    do.call(fluidRow, chunk)
  })
  
  tagList(rows)
  
  })
  
  # Single panel
  output$map_single <- renderLeaflet({
    mv <- mv_sel()
    validate(need(nrow(mv) > 0, "Please select one or more animals."))
    leaflet_map(mv)
  })
  
  # Multipanel
  observe({
    req(input$panel_mode == "Multipanel")
    ids <- input$animals
    mv  <- mv_sel()
    if (is.null(ids) || length(ids) == 0 || nrow(mv) == 0) return()
    
    lapply(ids, function(id_i){
      local({
        id_loc <- id_i
        output[[paste0("map_", id_loc)]] <- renderLeaflet({
          mv_id <- mv[as.character(mt_track_id(mv)) == id_loc, ]
          validate(need(nrow(mv_id) > 0, "No data for this animal."))
          leaflet_map(mv_id)
        })
      })
    })
  })
}

shinyApp(ui, server)
