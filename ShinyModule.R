library(shiny)
library(move2)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(pals)
library(colourpicker)

my_data <- mt_as_move2(readRDS("./data/raw/input2_whitefgeese.rds"))

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
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Animals"),
                 fluidRow(
                   column(8,checkboxGroupInput("animals", NULL, choices = NULL)),        
                   column(4, actionButton("select_all_animals", "Select All",  class = "btn-sm"),actionButton("unselect_animals",  "Unselect All", class = "btn-sm"))
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
      leafletOutput("map", height = "85vh")
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
      return(mv[0, ])  # return an empty move2 with same columns
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
    
    
    dd   <- sf::st_drop_geometry(mv_sel()) |> as.data.frame()
    vals <- dd[[input$attr]]
    n_unique <- length(unique(stats::na.omit(vals)))
    is_cont <- is.numeric(vals) || inherits(vals, "units") || n_unique > max_level
    list(n_unique = n_unique, is_cont = is_cont)
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
  seg_and_pal <- reactive({
    req(input$attr)
    mv   <- mv_sel()
    validate(need(nrow(mv) > 0, "Please select one or more animals."))
    
    segs <- make_segments(mv, input$attr)
    validate(need(nrow(segs) > 0, "No line segments for selected animals."))
    
    at <- attribute_type()
    if (isTRUE(at$is_cont)) {
      low  <- if (is.null(input$col_low))  "yellow" else input$col_low
      high <- if (is.null(input$col_high)) "blue" else input$col_high
      color_range  <- range(as.numeric(segs$value), na.rm = TRUE)
      pal  <- colorNumeric(colorRampPalette(c(low, high))(256),
                           domain = as.numeric(segs$value), na.color = NA)
      list(segs = segs, pal = pal, cont = TRUE,  legend_vals = color_range )
    } else {
      levs <- levels(factor(segs$value))
      pname <- if (is.null(input$cat_pal)) "Set2" else input$cat_pal
      cols <- if (tolower(pname) == "glasbey") {
        pals::glasbey(length(levs))
      } else {
        maxn <- RColorBrewer::brewer.pal.info[pname, "maxcolors"]
        RColorBrewer::brewer.pal(min(maxn, max(3, length(levs))), pname)[seq_along(levs)]
      }
      pal <- colorFactor(cols, domain = levs, na.color = NA)
      list(segs = segs, pal = pal, cont = FALSE, legend_vals = levs)
    }
  })
  
  
  
  
  
  output$map <- renderLeaflet({
    ac <- seg_and_pal()
    segs <- ac$segs
    
    bb <- as.vector(sf::st_bbox(segs))
    m <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      addTiles() %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4]) %>%
      addPolylines(
        data   = segs,
        weight = input$linesize_att,
        opacity = input$linealpha_att,
        dashArray= line_type(input$linetype_att),
        color  = if (ac$cont) ~ac$pal(as.numeric(value)) else ~ac$pal(as.character(value))
      )
    
    m %>% addLegend("bottomright", pal = ac$pal, values = ac$legend_vals,
                    title = input$attr, opacity = 1)
  })
}

shinyApp(ui, server)
