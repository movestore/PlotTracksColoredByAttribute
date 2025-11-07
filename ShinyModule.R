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
library(htmltools)
library(colorspace)


my_data <- readRDS("./data/raw/input4_move2loc_LatLon.rds")
#my_data <- readRDS("./data/raw/input2_move2loc_LatLon.rds")

# transfer to WGS84 if needed
if (!sf::st_is_longlat(my_data)) {
  my_data <- sf::st_transform(my_data, 4326)
}

########### helpers ###########

# helper 1: Attribute type
continuous_attr <- function(vals, threshold = 12) {
  is_num <- is.numeric(vals) || inherits(vals, "units")
  if (!is_num) return(FALSE)  # categorical
  n_unique <- length(unique(stats::na.omit(as.numeric(vals))))
  n_unique > threshold
}

## helper2: making segments with one attributes
make_segments_1attr <- function(tracks, attr_name, threshold = 12) {
  if (nrow(tracks) < 2) {
    return(sf::st_sf(track = character(0),
                     value = character(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  segs <- mt_segments(tracks)
  id   <- as.character(mt_track_id(tracks))
  vals <- sf::st_drop_geometry(tracks)[[attr_name]]
  
  same_track_next <- c(id[-length(id)] == id[-1], FALSE)
  if (!any(same_track_next)) {
    return(sf::st_sf(track = character(0),
                     value = character(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  
  if (continuous_attr(vals, threshold)) {
    v <- as.numeric(if (inherits(vals, "units")) units::drop_units(vals) else vals)
    seg_val <- rowMeans(cbind(v[same_track_next], v[which(same_track_next) + 1]), na.rm = TRUE)
    seg_val[is.nan(seg_val)] <- NA_real_
  } else {
    seg_val <- as.character(vals[same_track_next])
  }
  
  seg_track <- id[which(same_track_next)]
  sf::st_sf(track = seg_track, value = seg_val, geometry = segs[same_track_next])
}

## helper3: making segments with two attributes
make_segments_2attr <- function(tracks, cat_name, cont_name) {
  if (nrow(tracks) < 2) {
    return(sf::st_sf(track = character(0),
                     cat   = character(0),
                     cont  = numeric(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  segs <- mt_segments(tracks)
  id   <- as.character(mt_track_id(tracks))
  dd   <- sf::st_drop_geometry(tracks)
  
  catv <- as.character(dd[[cat_name]])
  cv   <- if (inherits(dd[[cont_name]], "units")) units::drop_units(dd[[cont_name]]) else as.numeric(dd[[cont_name]])
  
  same_track_next <- c(id[-length(id)] == id[-1], FALSE)
  if (!any(same_track_next)) {
    return(sf::st_sf(track = character(0),
                     cat   = character(0),
                     cont  = numeric(0),
                     geometry = sf::st_sfc(crs = sf::st_crs(tracks))))
  }
  
  seg_cat  <- catv[same_track_next]
  seg_cont <- rowMeans(cbind(cv[same_track_next], cv[which(same_track_next) + 1]), na.rm = TRUE)
  seg_cont[is.nan(seg_cont)] <- NA_real_
  
  seg_track <- id[which(same_track_next)]
  sf::st_sf(track = seg_track, cat = seg_cat, cont = seg_cont, geometry = segs[same_track_next])
}

## helper 4:  generate HCL colors
color_generator <- function(pal, n, step = NULL) {
  if (n <= 0) return(character(0))
  m <- length(pal)
  if (m == 0 || n > m) {
    golden <- 137.50776405003785
    hues   <- ((0:(n - 1)) * golden) %% 360
    return(hcl(h = hues, c = 65, l = 60))
  }
  if (is.null(step)) step <- max(3L, as.integer(round(m / 4)))
  step <- max(1L, as.integer(step))
  idx  <- ((0:(n - 1)) * step) %% m + 1L
  pal[idx]
}

## helper 5: legend for categorical attributes
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

# helper 6: shade a base color by weight- for cont in option2
shade_hex <- function(base_hex, w, light_to_dark = TRUE) {
  if (!length(base_hex)) return(character(0))
  if (light_to_dark) {
    colorspace::darken(base_hex, amount = w * 0.95)
  } else {
    colorspace::lighten(base_hex, amount = w * 0.95)
  }
}

# helper 7 : move track attr to events
as_event <- function(mv, attr_names) {
  if (is.null(attr_names) || !length(attr_names)) return(mv)
  nms <- unique(as.character(attr_names))
  trkattrb <- names(mt_track_data(mv))
  out <- mv
  for (nm in nms) if (!is.null(nm) && nm %in% trkattrb) out <- mt_as_event_attribute(out, nm)
  out
}


###############  UI  #################################
ui <- fluidPage(
  titlePanel("Plot Tracks Colored by Attributes"),
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Tracks"),
                 checkboxGroupInput("animals", NULL, choices = NULL),
                 fluidRow(
                   column(6, actionButton("select_all_animals", "Select All Animals", class = "btn-sm")),
                   column(6, actionButton("unselect_animals", "Unselect All Animals", class = "btn-sm"))
                 ),
                 hr(),
                 h4("Attribute"),
                 hr(),
                 radioButtons("attr_mode", NULL,
                              choices = c("Option 1: Color by 1 attribute", "Option 2: Color by 2 attributes"),
                              selected = "Option 1: Color by 1 attribute"),
                 
                 # Option 1
                 conditionalPanel(
                   condition = "input.attr_mode == 'Option 1: Color by 1 attribute'",
                   selectInput("attr_1", NULL, choices = NULL),
                   div(tags$small("Note: Numeric attributes with fewer than 12 unique values are considered as categorical.",
                                  style = "color: darkblue;")),
                   uiOutput("ui_color_controls_opt1")
                 ),
                 
                 # Option 2
                 conditionalPanel(
                   condition = "input.attr_mode == 'Option 2: Color by 2 attributes'",
                   fluidRow(
                     column(6, selectInput("cat_attr_2","Categorical Attribute", choices = NULL)),
                     column(6, selectInput("cat_pal_2", "Palette",
                                           choices  = c("Glasbey","Set2","Set3","Dark2","Paired","Accent"),
                                           selected = "Glasbey"))
                   ),
                   fluidRow(
                     column(6, selectInput("cont_attr_2", "Continuous Attribute", choices = NULL)),
                     column(6, selectInput("cont_pal_2", "Shade", choices = c("Light to Dark","Dark to Light"),
                                           selected = "Light to Dark"))
                   ),
                   div(tags$small("Note: Numeric attributes with fewer than 12 unique values are considered as categorical.",
                                  style = "color: darkblue;"))
                 ),
                 
                 hr(),
                 h4("Panel"),
                 radioButtons("panel_mode", NULL,
                              choices = c("Single panel","Multipanel"),
                              selected = "Single panel", inline = TRUE),
                 hr(),
                 h4("Style"),
                 fluidRow(
                   column(6, numericInput("linesize_att", "Line width", 3, min = 1, max = 10, step = 1)),
                   column(6, sliderInput("linealpha_att", "Transparency", min = 0, max = 1, value = 0.9, step = 0.05))
                 ),
                 
                 hr(),
                 checkboxInput("attach_colors", tags$strong("Add columns color hex and legend in the returned data"), value = FALSE),
                 
                 hr(),
                 actionButton("apply_btn", "Apply Changes", class = "btn-primary btn-block"),
                 hr(),
                 
                 h4("Download"),
                 fluidRow(
                   column(6, downloadButton("save_html","Save Map as HTML", class = "btn-sm")),
                   column(6, downloadButton("save_png", "Save Map as PNG", class = "btn-sm"))
                 ),
                 
                 #### csv test-start
                 hr(),
                 downloadButton("dl_colors_csv", "Download colors CSV (test)"),
                 hr(),
                 uiOutput("dl_colors_msg")
                 #### csv test-end
    ),
    
    mainPanel(uiOutput("maps_ui"))
  )
)

############################ server  ###################

server <- function(input, output, session) {
  
  locked_settings <- reactiveVal(NULL)
  locked_mv       <- reactiveVal(NULL)
  locked_attach   <- reactiveVal(FALSE)
  
  # keep tracks with at least 2 points
  mv_all <- reactive({
    my_data %>%
      arrange(mt_track_id(), mt_time()) %>%
      { .[!duplicated(data.frame(id = mt_track_id(.), t = mt_time(.))), ] } %>%
      group_by(track = mt_track_id()) %>%
      filter(n() >= 2) %>%
      ungroup()
  })
  
  # Track list
  observe({
    ids <- as.character(unique(mt_track_id(mv_all())))
    updateCheckboxGroupInput(session, "animals", choices = ids, selected = ids)
  })
  observeEvent(input$select_all_animals, {
    ids <- as.character(unique(mt_track_id(mv_all())))
    updateCheckboxGroupInput(session, "animals", selected = ids)
  })
  observeEvent(input$unselect_animals, {
    updateCheckboxGroupInput(session, "animals", selected = character(0))
  })
  
  # split attributes
  observe({
    mv <- mv_all()
    
    # event attr
    dd <- sf::st_drop_geometry(mv) |> as.data.frame()
    keep <- colSums(!is.na(dd)) > 0
    keep <- keep & !sapply(dd, inherits, what = "POSIXt")
    keep <- keep & (sapply(dd, class) != "Date")
    evnt_choices <- names(dd)[keep]
    
    # track attr
    trk_choices <- setdiff(names(mt_track_data(mv)), names(sf::st_drop_geometry(mv)))
    
    
    # Option 1: union (event + track)
    all_opt1 <- sort(unique(c(evnt_choices, trk_choices)))
    updateSelectInput(session, "attr_1", choices = all_opt1,
                      selected = if (length(all_opt1)) all_opt1[1] else NULL)
    
    # Option 2
    all_opt2 <- all_opt1
    
    # project all track attrs to events
    mv_tmp <- as_event(mv, all_opt2)
    dd2    <- sf::st_drop_geometry(mv_tmp)
    is_cont_col <- sapply(all_opt2, function(nm) continuous_attr(dd2[[nm]], threshold = 12))
    cat_cols  <- all_opt2[!is_cont_col]
    cont_cols <- all_opt2[ is_cont_col]
    
    updateSelectInput(session, "cat_attr_2",  choices = cat_cols,
                      selected = if (length(cat_cols))  cat_cols[1]  else NULL)
    updateSelectInput(session, "cont_attr_2", choices = cont_cols,
                      selected = if (length(cont_cols)) cont_cols[1] else NULL)
  })
  
  
  # Live selection of animals
  mv_sel <- reactive({
    mv <- mv_all()
    sel <- input$animals
    if (is.null(sel) || length(sel) == 0) return(mv[0, ])
    mv[as.character(mt_track_id(mv)) %in% sel, ] %>%
      arrange(mt_track_id(), mt_time())
  })
  
  #### Option 1 color controls
  attr_type_opt1 <- reactive({
    req(input$attr_1)
    mv <- mv_sel()
    if (nrow(mv) == 0) return(list(empty = TRUE, is_cont = TRUE))
    mv_use <- as_event(mv, input$attr_1)       
    vals   <- sf::st_drop_geometry(mv_use)[[input$attr_1]]
    list(empty = FALSE, is_cont = continuous_attr(vals, threshold = 12))
  })
  
  output$ui_color_controls_opt1 <- renderUI({
    at <- attr_type_opt1()
    if (isTRUE(at$empty)) return(helpText("Select animals to choose colors."))
    if (isTRUE(at$is_cont)) {
      tagList(
        h4("Colors"),
        fluidRow(
          column(6, colourpicker::colourInput("col_low_1",  "Low",
                                              if (is.null(isolate(input$col_low_1)))  "yellow" else isolate(input$col_low_1))),
          column(6, colourpicker::colourInput("col_high_1", "High",
                                              if (is.null(isolate(input$col_high_1))) "blue"   else isolate(input$col_high_1)))
        )
      )
    } else {
      tagList(
        h4("Colors"),
        selectInput("cat_pal_1", "Palette",
                    choices  = c("Glasbey","Set2","Set3","Dark2","Paired","Accent"),
                    selected = if (is.null(isolate(input$cat_pal_1))) "Glasbey" else isolate(input$cat_pal_1))
      )
    }
  })
  
  ######## first time shows map
  observe({
    mv <- mv_sel()
    if (!is.null(input$attr_1) &&
        nrow(mv) > 0 &&
        is.null(locked_mv()) && is.null(locked_settings())) {
      locked_mv(mv)
      locked_settings(list(
        animals     = input$animals,
        panel_mode  = input$panel_mode,
        attr_mode   = input$attr_mode,
        attr_1      = input$attr_1,
        col_low_1   = input$col_low_1,
        col_high_1  = input$col_high_1,
        cat_pal_1   = input$cat_pal_1,
        cat_attr_2  = input$cat_attr_2,
        cont_attr_2 = input$cont_attr_2,
        cat_pal_2   = input$cat_pal_2,
        cont_pal_2  = input$cont_pal_2,
        linesize    = input$linesize_att,
        linealpha   = input$linealpha_att
      ))
      locked_attach(isTRUE(input$attach_colors))
    }
  })
  
  ##### update locked state when Apply button is clicked
  observeEvent(input$apply_btn, {
    locked_mv(mv_sel())
    locked_settings(list(
      animals     = input$animals,
      panel_mode  = input$panel_mode,
      attr_mode   = input$attr_mode,
      attr_1      = input$attr_1,
      col_low_1   = input$col_low_1,
      col_high_1  = input$col_high_1,
      cat_pal_1   = input$cat_pal_1,
      cat_attr_2  = input$cat_attr_2,
      cont_attr_2 = input$cont_attr_2,
      cat_pal_2   = input$cat_pal_2,
      cont_pal_2  = input$cont_pal_2,
      linesize    = input$linesize_att,
      linealpha   = input$linealpha_att
    ))
    locked_attach(isTRUE(input$attach_colors))
  }, ignoreInit = TRUE)
  
  # current attribute
  mv_attr1 <- reactive({
    s  <- locked_settings()
    mv <- locked_mv()
    req(s, mv)
    as_event(mv, s$attr_1)
  })
  
  #  Build segs + palette per mode
  segs_and_pal <- reactive({
    s  <- locked_settings()
    mv <- locked_mv()
    req(s, mv)
    
    if (identical(s$attr_mode, "Option 1: Color by 1 attribute")) {
      req(s$attr_1)
      mv0  <- mv_attr1()
      segs <- make_segments_1attr(mv0, s$attr_1, threshold = 12)
      validate(need(nrow(segs) > 0, "No segments for selected animals."))
      
      vals <- segs$value
      is_cont <- continuous_attr(vals, threshold = 12)
      
      if (is_cont) { # continuous
        low  <- if (is.null(s$col_low_1))  "yellow" else s$col_low_1
        high <- if (is.null(s$col_high_1)) "blue"   else s$col_high_1
        all_vals <- as.numeric(sf::st_drop_geometry(mv0)[[s$attr_1]])
        all_vals <- all_vals[is.finite(all_vals)]
        rng      <- if (length(all_vals)) range(all_vals) else c(0,1)
        pal <- colorNumeric(colorRampPalette(c(low, high))(256), domain = rng, na.color = NA)
        list(mode = 1, segs = segs, is_cont = TRUE, pal = pal, legend_vals = rng, title = s$attr_1)
      } else {  # categorical
        levs  <- sort(unique(stats::na.omit(as.character(vals))))
        n     <- length(levs)
        pname <- if (is.null(s$cat_pal_1)) "Glasbey" else s$cat_pal_1
        base  <- if (tolower(pname) == "glasbey") pals::glasbey(max(32, n))
        else RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[pname,"maxcolors"], pname)
        cols  <- if (n <= length(base)) base[seq_len(n)] else color_generator(base, n)
        pal   <- colorFactor(cols, domain = levs, na.color = NA)
        list(mode = 1, segs = segs, is_cont = FALSE, pal = pal, legend_vals = levs, cols = cols, title = s$attr_1)
      }
      
    } else {
      # Option 2: Color by 2 attributes
      req(s$cat_attr_2, s$cont_attr_2)
      # project attributes to events
      mv02 <- as_event(mv, c(s$cat_attr_2, s$cont_attr_2))
      segs <- make_segments_2attr(mv02, s$cat_attr_2, s$cont_attr_2)
      validate(need(nrow(segs) > 0, "No segments for selected animals."))
      
      # categorical
      levs  <- sort(unique(stats::na.omit(as.character(segs$cat))))
      n     <- length(levs)
      pname <- if (is.null(s$cat_pal_2)) "Glasbey" else s$cat_pal_2
      base  <- if (tolower(pname) == "glasbey") pals::glasbey(max(32, n))
      else RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[pname,"maxcolors"], pname)
      cols_base <- if (n <= length(base)) base[seq_len(n)] else color_generator(base, n)
      names(cols_base) <- levs
      
      v_all <- segs$cont
      v_fin <- v_all[is.finite(v_all)]
      rng   <- if (length(v_fin)) range(v_fin) else c(0, 1)
      
      # shade categorical base by continuous value
      seg_cols <- rep("lightgray", nrow(segs))
      base_vec <- cols_base[as.character(segs$cat)]
      ok <- !is.na(base_vec) & is.finite(v_all)
      if (any(ok)) {
        w_ok <- if (diff(rng) == 0) rep(0.5, sum(ok)) else pmin(1, pmax(0, (v_all[ok] - rng[1]) / (rng[2] - rng[1])))
        seg_cols[ok] <- shade_hex(
          base_hex      = base_vec[ok],
          w             = w_ok,
          light_to_dark = identical(s$cont_pal_2, "Light to Dark")
        )
      }
      
      list(mode = 2, segs = segs,
           seg_cols = seg_cols,
           cat_legend = cols_base,
           cont_range = rng,
           title_cat = s$cat_attr_2,
           title_cont = paste0(s$cont_attr_2, " (", s$cont_pal_2, ")"))
    }
  })
  
  # add color hex column in data
  mv_with_colors <- reactive({
    s  <- locked_settings()
    mv <- locked_mv()
    sp <- segs_and_pal()
    req(s, mv, sp)
    
    if (sp$mode == 1) {
    
      mv_use <- as_event(mv, s$attr_1)
      vals   <- sf::st_drop_geometry(mv_use)[[s$attr_1]]
      hex    <- if (sp$is_cont) sp$pal(as.numeric(vals)) else sp$pal(as.character(vals))
      mv$color <- as.character(hex)
      cname <- paste0("color_legend_", s$attr_1)
      mv[[cname]] <- vals
      return(mv)
    } else {
      
      mv02 <- as_event(mv, c(s$cat_attr_2, s$cont_attr_2))
      cat_vals  <- sf::st_drop_geometry(mv02)[[s$cat_attr_2]]
      cont_vals <- as.numeric(sf::st_drop_geometry(mv02)[[s$cont_attr_2]])
      base_vec  <- sp$cat_legend[as.character(cat_vals)]
      rng       <- sp$cont_range
      
      w <- if (isTRUE(is.finite(diff(rng))) && diff(rng) != 0) {
        pmin(1, pmax(0, (cont_vals - rng[1]) / (rng[2] - rng[1])))
      } else rep(0.5, length(cont_vals))
      
      hex <- rep("lightgray", length(base_vec))
      ok  <- !is.na(base_vec) & is.finite(cont_vals)
      if (any(ok)) {
        hex[ok] <- shade_hex(
          base_hex      = base_vec[ok],
          w             = w[ok],
          light_to_dark = identical(s$cont_pal_2, "Light to Dark")
        )
      }
      
      mv$color <- as.character(hex)
      
      combo_colname <- paste0(s$cat_attr_2, "-", s$cont_attr_2)
      cat_str  <- ifelse(is.na(cat_vals), "NA", as.character(cat_vals))
      cont_str <- ifelse(is.finite(cont_vals), sprintf('%g', cont_vals), "NA")
      mv[[combo_colname]] <- paste0(cat_str, "-", cont_str)
      return(mv)
    }
  })
  
  return_mv_with_colors <- eventReactive(input$apply_btn, {
    if (isTRUE(locked_attach())) mv_with_colors() else NULL
  }, ignoreInit = TRUE)
  
  observeEvent(return_mv_with_colors(), {
    session$userData$mv_next <- return_mv_with_colors
  })
  
  #### Leaflet map   ##########
  leaflet_map <- function(track_id = NULL) {
    s  <- locked_settings()
    sp <- segs_and_pal()
    req(s, sp)
    
    # subset for multipanel
    if (!is.null(track_id)) {
      if (sp$mode == 1) {
        segs <- sp$segs[sp$segs$track == track_id, , drop = FALSE]
      } else {
        segs <- sp$segs[sp$segs$track == track_id, , drop = FALSE]
      }
      validate(need(nrow(segs) > 0, "No data for this animal."))
    } else {
      segs <- sp$segs
    }
    
    # light gray for NA
    if (sp$mode == 1) {
      if (sp$is_cont) {
        dseg <- segs %>% mutate(.val = as.numeric(value),
                                .col = if_else(is.finite(.val), sp$pal(.val), "lightgray"))
      } else {
        dseg <- segs %>% mutate(.val = as.character(value),
                                .col = if_else(is.na(.val), "lightgray", sp$pal(.val)))
      }
    } else {
      dseg <- segs
      pcols <- sp$seg_cols
      if (!is.null(track_id)) {
        idx <- sp$segs$track == track_id
        pcols <- pcols[idx]
      }
      dseg$.col <- pcols
    }
    
    bb <- as.vector(sf::st_bbox(dseg))
    m <- leaflet(options = leafletOptions(minZoom = 2, preferCanvas = TRUE)) %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4]) %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("TopoMap") %>%
      hideGroup("Aerial") %>%
      addScaleBar(position = "topleft") %>%
      addPolylines(data = dseg,
                   weight = s$linesize, opacity = s$linealpha,
                   color  = ~.col, smoothFactor = 1)
    
    # Legends
    if (sp$mode == 1) {
      if (sp$is_cont) { # continuous
        mv_legend <- mv_attr1()  # legend source
        vals <- as.numeric(sf::st_drop_geometry(mv_legend)[[sp$title]])
        vals <- vals[is.finite(vals)]
        if (!length(vals)) vals <- sp$legend_vals
        
        mn <- min(vals); mx <- max(vals)
        
        # 3 ticks between min and max
        ticks_all <- pretty(c(mn, mx), n = 5)
        inner <- ticks_all[ticks_all > mn & ticks_all < mx]
        if (length(inner) >= 3) {
          idx <- round(seq(1, length(inner), length.out = 3))
          inner3 <- inner[idx]
        } else {
          inner3 <- seq(mn, mx, length.out = 5)[2:4]
        }
        t1 <- inner3[1]; t2 <- inner3[2]; t3 <- inner3[3]
        
        # unit in title if present
        orig_vals <- sf::st_drop_geometry(mv_legend)[[sp$title]]
        unit_str  <- if (inherits(orig_vals, "units")) units::deparse_unit(orig_vals) else NULL
        title_txt <- if (!is.null(unit_str) && nzchar(unit_str)) paste0(sp$title, " (", unit_str, ")") else sp$title
        
        grad <- tags$div(
          style = "background:rgba(255,255,255,0.85);padding:6px 8px;border-radius:4px;font-size:11px;",
          tags$div(htmlEscape(title_txt), style="font-weight:600;margin-bottom:4px;"),
          tags$div(style = paste0(
            "width:220px;height:12px;background:linear-gradient(to right,",
            sp$pal(mn), ",", sp$pal(mx),
            ");border:1px solid rgba(0,0,0,0.25);margin-bottom:6px;"
          )),
          tags$div(style="display:flex;justify-content:space-between;width:220px;opacity:0.9;",
                   tags$span(sprintf('%g', mn)),
                   tags$span(sprintf('%g', t1)),
                   tags$span(sprintf('%g', t2)),
                   tags$span(sprintf('%g', t3)),
                   tags$span(sprintf('%g', mx))),
          tags$div(style="display:flex;justify-content:space-between;width:220px;opacity:0.7;",
                   tags$span("min"), tags$span(""), tags$span(""), tags$span(""), tags$span("max")),
          tags$div(style="margin-top:6px;display:flex;align-items:center;gap:6px;opacity:0.85;",
                   tags$span(style="display:inline-block;width:12px;height:12px;background:#BDBDBD;border:1px solid rgba(0,0,0,0.25);"),
                   tags$span("no data (NA)"))
        )
        m <- leaflet::addControl(m, html = as.character(grad), position = "topright")
      } else {
        m <- add_cat_legend(m, title = sp$title, labels = sp$legend_vals, colors = sp$cols, position = "topright")
      }
    } else {
      # Option 2
      m <- add_cat_legend(m, title = sp$title_cat, labels = names(sp$cat_legend),
                          colors = unname(sp$cat_legend), position = "topright")
      
      cont_legend <- sprintf(
        "<div style='background:transparent;padding:4px 6px 6px 6px;font-size:12px;opacity:.85;'>
           <div style='font-weight:700;'>%s</div>
           <div style='margin-top:6px;display:flex;align-items:center;gap:6px;'>
             <span style='display:inline-block;width:12px;height:12px;background:#BDBDBD;
                          border:1px solid rgba(0,0,0,0.25);'></span>
             <span>no data (NA)</span>
           </div>
         </div>", htmlEscape(sp$title_cont)
      )
      m <- leaflet::addControl(m, html = cont_legend, position = "topright")
    }
    
    m
  }
  
  ######### Layout ##########
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
  
  #  Downloads part
  
  #### html Downloads
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
  
  #### PNG Downloads
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
      
      if (!identical(s$panel_mode, "Multipanel")) {
        tf  <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(leaflet_map(), tf, selfcontained = TRUE)
        url <- if (.Platform$OS.type == "windows")
          paste0("file:///", gsub("\\\\", "/", normalizePath(tf))) else tf
        webshot2::webshot(url, file, vwidth = 1400, vheight = 900, delay = 1)
        return(invisible())
      }
      
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
  
  #### TEST  — CSV of color columns ##############
  output$dl_colors_csv <- downloadHandler(
    filename = function() paste0("colors_", Sys.Date(), ".csv"),
    content  = function(file) {
      
      if (is.null(return_mv_with_colors())) {
        output$dl_colors_msg <- renderUI(
          div(
            style = "color:red; font-weight:600; margin-top:6px;",
            HTML('Please check <em>“Add columns color hex and legend in the returned data”</em> and click <u>Apply Changes</u> before downloading.')
          )
        )
        stop("Download blocked: colors not attached (checkbox not applied).")
      }
      
      output$dl_colors_msg <- renderUI(NULL)
      
      s  <- locked_settings(); req(s)
      mv <- return_mv_with_colors(); req(!is.null(mv))
      
      df <- data.frame(
        track_id  = as.character(mt_track_id(mv)),
        color_hex = as.character(mv$color),
        stringsAsFactors = FALSE
      )
      
      if (identical(s$attr_mode, "Option 1: Color by 1 attribute")) {
        cname <- paste0("color_legend_", s$attr_1)
        if (!cname %in% names(sf::st_drop_geometry(mv))) {
          stop(sprintf("Missing legend column '%s'. Re-apply with 'Add columns...' checked.", cname))
        }
        df[[cname]] <- sf::st_drop_geometry(mv)[[cname]]
        
      } else {
        combo_col <- paste0(s$cat_attr_2, "-", s$cont_attr_2)
        dd <- sf::st_drop_geometry(mv)
        if (!combo_col %in% names(dd)) {
          stop(sprintf(
            "Missing legend column '%s'. Re-apply with 'Add columns...' checked.",
            combo_col
          ))
        }
        df[[combo_col]] <- dd[[combo_col]]
      }
      
      df <- unique(df)
      utils::write.csv(df, file, row.names = FALSE)
    }
  )
  #### TEST end #####################
}

shinyApp(ui, server)
