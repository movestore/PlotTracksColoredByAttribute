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
library(units)

`%||%` <- function(x, y) if (is.null(x)) y else x

########### helpers ###########
# helper 1: Attribute type
continuous_attr <- function(vals, threshold = 12) {
  is_num <- is.numeric(vals) || inherits(vals, "units")
  if (!is_num) return(FALSE)
  n_unique <- length(unique(stats::na.omit(as.numeric(vals))))
  n_unique > threshold
}

## helper2: making segments with one attribute
make_segments_1attr <- function(tracks, attr_name, threshold = 12) {
  if (nrow(tracks) < 2) {
    return(sf::st_sf(
      track_id = character(0),
      value = character(0),
      geometry = sf::st_sfc(crs = sf::st_crs(tracks))
    ))
  }
  
  segs <- mt_segments(tracks)
  id   <- as.character(mt_track_id(tracks))
  vals <- sf::st_drop_geometry(tracks)[[attr_name]]
  
  same_track_next <- c(id[-length(id)] == id[-1], FALSE)
  if (!any(same_track_next)) {
    return(sf::st_sf(
      track_id = character(0),
      value = character(0),
      geometry = sf::st_sfc(crs = sf::st_crs(tracks))
    ))
  }
  
  if (continuous_attr(vals, threshold)) {
    v <- as.numeric(if (inherits(vals, "units")) units::drop_units(vals) else vals)
    seg_val <- rowMeans(cbind(v[same_track_next], v[which(same_track_next) + 1]), na.rm = TRUE)
    seg_val[is.nan(seg_val)] <- NA_real_
  } else {
    seg_val <- as.character(vals[same_track_next])
  }
  
  seg_track <- id[which(same_track_next)]
  sf::st_sf(track_id = seg_track, value = seg_val, geometry = segs[same_track_next])
}

## helper3: making segments with two attributes
make_segments_2attr <- function(tracks, cat_name, cont_name) {
  if (nrow(tracks) < 2) {
    return(sf::st_sf(
      track_id = character(0),
      cat = character(0),
      cont = numeric(0),
      geometry = sf::st_sfc(crs = sf::st_crs(tracks))
    ))
  }
  
  segs <- mt_segments(tracks)
  id   <- as.character(mt_track_id(tracks))
  dd   <- sf::st_drop_geometry(tracks)
  
  catv <- as.character(dd[[cat_name]])
  cv   <- if (inherits(dd[[cont_name]], "units")) units::drop_units(dd[[cont_name]]) else as.numeric(dd[[cont_name]])
  
  same_track_next <- c(id[-length(id)] == id[-1], FALSE)
  if (!any(same_track_next)) {
    return(sf::st_sf(
      track_id = character(0),
      cat = character(0),
      cont = numeric(0),
      geometry = sf::st_sfc(crs = sf::st_crs(tracks))
    ))
  }
  
  seg_cat  <- catv[same_track_next]
  seg_cont <- rowMeans(cbind(cv[same_track_next], cv[which(same_track_next) + 1]), na.rm = TRUE)
  seg_cont[is.nan(seg_cont)] <- NA_real_
  
  seg_track <- id[which(same_track_next)]
  sf::st_sf(track_id = seg_track, cat = seg_cat, cont = seg_cont, geometry = segs[same_track_next])
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
add_cat_legend <- function(map, title, labels, colors, position = "topright", group = "Categorical_Legend") {
  stopifnot(length(labels) == length(colors))
  leaflet::addLegend(
    map,
    position = position,
    colors = colors,
    labels = labels,
    title = title,
    opacity = 1,
    group = group
  )
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
  for (nm in nms) {
    if (!is.null(nm) && nm %in% trkattrb) out <- mt_as_event_attribute(out, nm)
  }
  out
}

# helper 8: get available attribute names directly from event + track data
get_attr_choices <- function(mv) {
  event_df <- sf::st_drop_geometry(mv) |> as.data.frame()
  event_choices <- character(0)
  
  if (ncol(event_df) > 0) {
    keep_ev <- colSums(!is.na(event_df)) > 0
    keep_ev <- keep_ev & !vapply(event_df, inherits, logical(1), what = "POSIXt")
    keep_ev <- keep_ev & !vapply(event_df, inherits, logical(1), what = "Date")
    event_choices <- names(event_df)[keep_ev]
  }
  
  td <- mt_track_data(mv)
  track_choices <- character(0)
  
  if (!is.null(td) && ncol(td) > 0) {
    keep_td <- colSums(!is.na(td)) > 0
    keep_td <- keep_td & !vapply(td, inherits, logical(1), what = "POSIXt")
    keep_td <- keep_td & !vapply(td, inherits, logical(1), what = "Date")
    track_choices <- setdiff(names(td)[keep_td], event_choices)
  }
  
  sort(unique(c(event_choices, track_choices)))
}

# helper 9: split attributes into all,cat,cont without converting all track attrs
split_attr_choices <- function(mv, threshold = 12) {
  all_names <- get_attr_choices(mv)
  if (!length(all_names)) {
    return(list(all = character(0), cat = character(0), cont = character(0)))
  }
  
  event_df <- sf::st_drop_geometry(mv) |> as.data.frame()
  td <- mt_track_data(mv)
  
  is_cont <- vapply(all_names, function(nm) {
    if (nm %in% names(event_df)) {
      continuous_attr(event_df[[nm]], threshold = threshold)
    } else if (!is.null(td) && nm %in% names(td)) {
      continuous_attr(td[[nm]], threshold = threshold)
    } else {
      FALSE
    }
  }, logical(1))
  
  list( all  = all_names, cat  = all_names[!is_cont], cont = all_names[ is_cont])
}

# helper 10: get one attribute from its original source
get_attr_values <- function(mv, attr_name) {
  if (is.null(attr_name) || !length(attr_name)) return(NULL)
  
  event_df <- sf::st_drop_geometry(mv)
  if (attr_name %in% names(event_df)) return(event_df[[attr_name]])
  
  td <- mt_track_data(mv)
  if (!is.null(td) && attr_name %in% names(td)) return(td[[attr_name]])
  
  NULL
}

############### UI #################################

shinyModuleUserInterface <- function(id, label = NULL) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Plot Tracks Colored by Attributes"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("Tracks"),
        uiOutput(ns("animals_ui")),
        fluidRow(
          column(6, actionButton(ns("select_all_animals"), "Select All Tracks", class = "btn-sm")),
          column(6, actionButton(ns("unselect_animals"), "Unselect All Tracks", class = "btn-sm"))
        ),
        ###json file #####
        tags$div(style = "display:none;", textInput(ns("animals_json"), NULL, "")),
        tags$div(style = "display:none;", textInput(ns("attr_1_json"), NULL, "")),
        tags$div(style = "display:none;", textInput(ns("cat_attr_2_json"), NULL, "")),
        tags$div(style = "display:none;", textInput(ns("cont_attr_2_json"), NULL, "")),
        ######################
        hr(),
        h4("Attribute"),
        hr(),
        radioButtons(
          ns("attr_mode"), NULL,
          choices = c("Option 1: Color by 1 attribute", "Option 2: Color by 2 attributes"),
          selected = "Option 1: Color by 1 attribute"
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Option 1: Color by 1 attribute'", ns("attr_mode")),
          uiOutput(ns("attr_1_ui")),
          div(tags$small(
            "Note: Numeric attributes with fewer than 12 unique values are considered as categorical.",
            style = "color: darkblue;"
          )),
          uiOutput(ns("ui_color_controls_opt1"))
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Option 2: Color by 2 attributes'", ns("attr_mode")),
          fluidRow(
            column(6, uiOutput(ns("cat_attr_2_ui"))),
            column(6, selectInput(
              ns("cat_pal_2"), "Palette",
              choices = c("Glasbey", "Set2", "Set3", "Dark2", "Paired", "Accent"),
              selected = "Glasbey"
            ))
          ),
          fluidRow(
            column(6, uiOutput(ns("cont_attr_2_ui"))),
            column(6, selectInput(
              ns("cont_pal_2"), "Shade",
              choices = c("Light to Dark", "Dark to Light"),
              selected = "Light to Dark"
            ))
          ),
          div(tags$small(
            "Note: Numeric attributes with fewer than 12 unique values are considered as categorical.",
            style = "color: darkblue;"
          ))
        ),
        
        hr(),
        h4("Panel"),
        radioButtons(
          ns("panel_mode"), NULL,
          choices = c("Single panel", "Multipanel"),
          selected = "Single panel",
          inline = TRUE
        ),
        hr(),
        h4("Style"),
        fluidRow(
          column(6, numericInput(ns("linesize_att"), "Line width", 3, min = 1, max = 10, step = 1)),
          column(6, sliderInput(ns("linealpha_att"), "Transparency", min = 0, max = 1, value = 0.9, step = 0.05))
        ),
        
        hr(),
        checkboxInput(ns("attach_colors"), tags$strong("Add columns color hex and legend in the returned data"), value = FALSE),
        
        hr(),
        actionButton(ns("apply_btn"), "Apply Changes", class = "btn-primary btn-block"),
        hr(),
        
        h4("Download"),
        fluidRow(
          column(6, downloadButton(ns("save_html"), "Save Map as HTML", class = "btn-sm")),
          column(6, downloadButton(ns("save_png"), "Save Map as PNG", class = "btn-sm"))
        )
      ),
      mainPanel(uiOutput(ns("maps_ui")))
    )
  )
}

############################ server ###################

shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  
  # base data for UI
  base_data <- reactive({
    mv <- data
    if (!sf::st_is_longlat(mv)) mv <- sf::st_transform(mv, 4326)
    
    ev <- sf::st_drop_geometry(mv)
    keep_ev <- names(ev)[colSums(!is.na(ev)) > 0]
    if (length(keep_ev)) mv <- mv[, keep_ev, drop = FALSE]
    
    td <- mt_track_data(mv)
    if (!is.null(td) && ncol(td) > 0) {
      keep_td <- names(td)[colSums(!is.na(td)) > 0]
      if (length(keep_td)) {
        mv <- do.call(select_track_data, c(list(mv), as.list(keep_td)))
      }
    }
    mv
  })
  
  current <- reactiveVal(NULL)
  
  
  if (!is.null(data) && nrow(data) > 0) {
    current(data)
  }
  
  locked_settings <- reactiveVal(NULL)
  locked_mv       <- reactiveVal(NULL)
  locked_attach   <- reactiveVal(FALSE)
  init_done       <- reactiveVal(FALSE)
  
  mv_all <- reactive({
    base_data() %>%
      arrange(mt_track_id(), mt_time()) %>%
      { .[!duplicated(data.frame(id = mt_track_id(.), t = mt_time(.))), ] } %>%
      group_by(track_id = mt_track_id()) %>%
      filter(n() >= 2) %>%
      ungroup()
  })
  ########
  applied_animals <- reactiveVal(NULL)
  init_applied <- reactiveVal(FALSE)
  
  observeEvent(input$animals, {
    req(!is.null(input$animals))
    applied_animals(as.character(input$animals))
    init_applied(TRUE)
  }, ignoreInit = FALSE)
  ####
  
  #attribute choices and classification
  attr_info_all <- reactive({ split_attr_choices(mv_all(), threshold = 12)  })
  
  attr_choices_all <- reactive({attr_info_all()$all })
  
  cat_cont_choices <- reactive({list( cat  = attr_info_all()$cat, cont = attr_info_all()$cont )  })
  #############
  
  # dynamic UI
  output$animals_ui <- renderUI({
    ids <- as.character(unique(mt_track_id(mv_all())))
    restored_sel <- isolate(input$animals)
    sel <- if (!is.null(restored_sel)) intersect(restored_sel, ids) else ids
    if (!length(sel)) sel <- character(0)
    
    checkboxGroupInput(ns("animals"), NULL, choices = ids, selected = sel)
  })
  ##################
  output$attr_1_ui <- renderUI({
    ch <- attr_choices_all()
    restored <- isolate(input$attr_1)
    sel <- if (!is.null(restored) && restored %in% ch) restored else if (length(ch)) ch[1] else NULL
    
    selectInput(ns("attr_1"), NULL, choices = ch, selected = sel)
  })
  
  output$cat_attr_2_ui <- renderUI({
    ch <- cat_cont_choices()$cat
    restored <- isolate(input$cat_attr_2)
    sel <- if (!is.null(restored) && restored %in% ch) restored else if (length(ch)) ch[1] else NULL
    
    selectInput(ns("cat_attr_2"), "Categorical Attribute", choices = ch, selected = sel)
  })
  
  output$cont_attr_2_ui <- renderUI({
    ch <- cat_cont_choices()$cont
    restored <- isolate(input$cont_attr_2)
    sel <- if (!is.null(restored) && restored %in% ch) restored else if (length(ch)) ch[1] else NULL
    
    selectInput(ns("cont_attr_2"), "Continuous Attribute", choices = ch, selected = sel)
  })
  
  observeEvent(input$select_all_animals, {
    ids <- as.character(unique(mt_track_id(mv_all())))
    updateCheckboxGroupInput(session, "animals", selected = ids)
  }, ignoreInit = TRUE)
  
  observeEvent(input$unselect_animals, {
    updateCheckboxGroupInput(session, "animals", selected = character(0))
  }, ignoreInit = TRUE)
  
  mv_sel <- reactive({
    req(init_applied())
    
    mv <- mv_all()
    sel <- applied_animals()
    
    if (is.null(sel) || length(sel) == 0) return(mv[0, ])
    
    mv[as.character(mt_track_id(mv)) %in% sel, ] %>%
      arrange(mt_track_id(), mt_time())
  })
  
  #### Option 1 color controls
  attr_type_opt1 <- reactive({
    req(input$attr_1)
    mv <- mv_sel()
    if (nrow(mv) == 0) return(list(empty = TRUE, is_cont = TRUE))
    
    vals <- get_attr_values(mv, input$attr_1)
    if (is.null(vals)) return(list(empty = TRUE, is_cont = TRUE))
    
    list(empty = FALSE, is_cont = continuous_attr(vals, threshold = 12))
  })
  
  ############################
  collect_settings <- reactive({
    req(input$animals, input$attr_mode, input$panel_mode,
        input$linesize_att, input$linealpha_att)
    
    if (identical(input$attr_mode, "Option 1: Color by 1 attribute")) {
      req(input$attr_1)
      
      mv_tmp <- mv_sel()
      req(nrow(mv_tmp) > 0)
      
      vals_tmp <- get_attr_values(mv_tmp, input$attr_1)
      req(!is.null(vals_tmp))
      
      if (continuous_attr(vals_tmp, threshold = 12)) {
        req(input$col_low_1, input$col_high_1)
      } else {
        req(input$cat_pal_1)
      }
      
    } else {
      req(input$cat_attr_2, input$cont_attr_2, input$cat_pal_2, input$cont_pal_2)
    }
    
    list(
      animals     = applied_animals() %||% input$animals,
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
    )
  })
  ###############################
  # color controls for Option 1
  output$ui_color_controls_opt1 <- renderUI({
    at <- attr_type_opt1()
    if (isTRUE(at$empty)) return(helpText("Select animals to choose colors."))
    
    if (isTRUE(at$is_cont)) {
      tagList(
        h4("Colors"),
        fluidRow(
          column(6, colourpicker::colourInput(ns("col_low_1"), "Low", if (is.null(isolate(input$col_low_1))) "yellow" else isolate(input$col_low_1) )),
          column(6, colourpicker::colourInput(ns("col_high_1"), "High", if (is.null(isolate(input$col_high_1))) "blue" else isolate(input$col_high_1)  ))
        )
      )
    } else {
      tagList(
        h4("Colors"),
        selectInput(ns("cat_pal_1"), "Palette",choices  = c("Glasbey", "Set2", "Set3", "Dark2", "Paired", "Accent"),selected = if (is.null(isolate(input$cat_pal_1))) "Glasbey" else isolate(input$cat_pal_1) )
      )
    }
  })
  ######################
  #  initialize once from restored input values
  observe({
    if (isTRUE(init_done())) return()
    req(init_applied())
    
    mv <- mv_sel()
    if (nrow(mv) == 0) return()
    
    s <- collect_settings()
    
    locked_mv(mv)
    locked_settings(s)
    locked_attach(isTRUE(input$attach_colors))
    
    init_done(TRUE)
  })
  
  observeEvent(input$apply_btn, {
    if (is.null(input$animals) || length(input$animals) == 0) return()
    
    locked_mv(mv_sel())
    locked_settings(collect_settings())
    locked_attach(isTRUE(input$attach_colors))
  }, ignoreInit = TRUE)
  ##################################
  
  
  mv_attr1 <- reactive({
    s  <- locked_settings()
    mv <- locked_mv()
    req(s, mv)
    as_event(mv, s$attr_1)
  })
  
  #build segments and color palettes for the selected attribute
  segs_and_pal <- reactive({
    s  <- locked_settings()
    mv <- locked_mv()
    req(s, mv)
    
    #option 1:
    if (identical(s$attr_mode, "Option 1: Color by 1 attribute")) {
      req(s$attr_1)
      mv0  <- mv_attr1()
      segs <- make_segments_1attr(mv0, s$attr_1, threshold = 12)
      shiny::validate(shiny::need(nrow(segs) > 0, "No segments for selected animals."))
      
      vals <- segs$value
      is_cont <- continuous_attr(vals, threshold = 12)
      
      if (is_cont) {
        low  <- if (is.null(s$col_low_1))  "yellow" else s$col_low_1
        high <- if (is.null(s$col_high_1)) "blue"   else s$col_high_1
        
        mv_full <- as_event(data, s$attr_1)
        orig_vals <- sf::st_drop_geometry(mv_full)[[s$attr_1]]
        all_vals  <- if (inherits(orig_vals, "units")) units::drop_units(orig_vals) else orig_vals
        all_vals  <- as.numeric(all_vals)
        all_vals  <- all_vals[is.finite(all_vals)]
        rng       <- if (length(all_vals)) range(all_vals) else c(0, 1)
        
        pal <- colorNumeric(colorRampPalette(c(low, high))(256), domain = rng, na.color = NA)
        
        list(mode = 1, segs = segs, is_cont = TRUE, pal = pal, legend_vals = rng, title = s$attr_1)
      } else {
        levs  <- sort(unique(stats::na.omit(as.character(vals))))
        n     <- length(levs)
        pname <- if (is.null(s$cat_pal_1)) "Glasbey" else s$cat_pal_1
        base  <- if (tolower(pname) == "glasbey") pals::glasbey(max(32, n))
        else RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[pname, "maxcolors"], pname)
        cols  <- if (n <= length(base)) base[seq_len(n)] else color_generator(base, n)
        pal   <- colorFactor(cols, domain = levs, na.color = NA)
        list(mode = 1, segs = segs, is_cont = FALSE, pal = pal, legend_vals = levs, cols = cols, title = s$attr_1)
      }
      
    } else { #option2
      req(s$cat_attr_2, s$cont_attr_2)
      mv02 <- as_event(mv, c(s$cat_attr_2, s$cont_attr_2))
      segs <- make_segments_2attr(mv02, s$cat_attr_2, s$cont_attr_2)
      shiny::validate(shiny::need(nrow(segs) > 0, "No segments for selected animals."))
      
      levs  <- sort(unique(stats::na.omit(as.character(segs$cat))))
      n     <- length(levs)
      pname <- if (is.null(s$cat_pal_2)) "Glasbey" else s$cat_pal_2
      base  <- if (tolower(pname) == "glasbey") pals::glasbey(max(32, n))
      else RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[pname, "maxcolors"], pname)
      cols_base <- if (n <= length(base)) base[seq_len(n)] else color_generator(base, n)
      names(cols_base) <- levs
      
      v_all <- segs$cont
      v_fin <- v_all[is.finite(v_all)]
      rng   <- if (length(v_fin)) range(v_fin) else c(0, 1)
      
      seg_cols <- rep("lightgray", nrow(segs))
      base_vec <- cols_base[as.character(segs$cat)]
      ok <- !is.na(base_vec) & is.finite(v_all)
      if (any(ok)) {
        w_ok <- if (diff(rng) == 0) rep(0.5, sum(ok)) else pmin(1, pmax(0, (v_all[ok] - rng[1]) / (rng[2] - rng[1])))
        seg_cols[ok] <- shade_hex(base_hex = base_vec[ok], w= w_ok,light_to_dark = identical(s$cont_pal_2, "Light to Dark")  )
      }
      
      list(
        mode = 2, segs = segs,
        seg_cols = seg_cols,
        cat_legend = cols_base,
        cont_range = rng,
        title_cat = s$cat_attr_2,
        title_cont = paste0(s$cont_attr_2, " (", s$cont_pal_2, ")")
      )
    }
  })
  
  ##Add columns color hex and legend in the returned data
  mv_with_colors <- reactive({
    s  <- locked_settings()
    sp <- segs_and_pal()
    req(s, sp)
    
    mv <- data   
    
    # option 1
    if (sp$mode == 1) {
      mv_use <- as_event(mv, s$attr_1)
      vals0  <- sf::st_drop_geometry(mv_use)[[s$attr_1]]
      numv   <- if (inherits(vals0, "units")) units::drop_units(vals0) else vals0
      
      hex <- if (sp$is_cont) {
        sp$pal(as.numeric(numv))
      } else {
        sp$pal(as.character(vals0))
      }
      
      mv$color_hex <- as.character(hex)
      cname <- paste0("color_legend_", s$attr_1)
      mv[[cname]] <- vals0
      return(mv)
      
    } else { # option 2
      mv02 <- as_event(mv, c(s$cat_attr_2, s$cont_attr_2))
      dd   <- sf::st_drop_geometry(mv02)
      
      cat_vals  <- dd[[s$cat_attr_2]]
      cont_raw  <- dd[[s$cont_attr_2]]
      cont_vals <- if (inherits(cont_raw, "units")) units::drop_units(cont_raw) else as.numeric(cont_raw)
      
      base_vec <- sp$cat_legend[as.character(cat_vals)]
      rng      <- sp$cont_range
      
      w <- if (isTRUE(is.finite(diff(rng))) && diff(rng) != 0) {
        pmin(1, pmax(0, (cont_vals - rng[1]) / (rng[2] - rng[1])))
      } else {
        rep(0.5, length(cont_vals))
      }
      
      hex <- rep("lightgray", length(base_vec))
      ok  <- !is.na(base_vec) & is.finite(cont_vals)
      
      if (any(ok)) {
        hex[ok] <- shade_hex(
          base_hex      = base_vec[ok],
          w             = w[ok],
          light_to_dark = identical(s$cont_pal_2, "Light to Dark")
        )
      }
      
      mv$color_hex <- as.character(hex)
      
      combo_colname <- paste0("color_legend_", s$cat_attr_2, "-", s$cont_attr_2)
      cat_str  <- ifelse(is.na(cat_vals), "NA", as.character(cat_vals))
      cont_str <- ifelse(is.finite(cont_vals), sprintf("%g", cont_vals), "NA")
      mv[[combo_colname]] <- paste0(cat_str, "-", cont_str)
      
      return(mv)
    }
  })
  
  
  # Update the returned output data
  observe({
    req(locked_settings())
    
    if (isTRUE(locked_attach())) {
      current(mv_with_colors())   
    } else {
      current(data)               
    }
  })
  
  
  ##leaflet map
  leaflet_map <- function(track_id = NULL) {
    s  <- locked_settings()
    sp <- segs_and_pal()
    req(s, sp)
    
    if (!is.null(track_id)) {
      segs <- sp$segs[sp$segs$track_id == track_id, , drop = FALSE]
      shiny::validate(shiny::need(nrow(segs) > 0, "No data for this animal."))
    } else {
      segs <- sp$segs
    }
    
    #option1
    if (sp$mode == 1) {
      if (sp$is_cont) {
        dseg <- segs %>% mutate(.val = as.numeric(value),
                                .col = if_else(is.finite(.val), sp$pal(.val), "lightgray"))
      } else {
        dseg <- segs %>% mutate(.val = as.character(value), .col = if_else(is.na(.val), "lightgray", sp$pal(.val)))
      }
    } else {  #option2
      dseg <- segs
      pcols <- sp$seg_cols
      if (!is.null(track_id)) {
        idx <- sp$segs$track_id == track_id
        pcols <- pcols[idx]
      }
      dseg$.col <- pcols
    }
    
    bb <- as.vector(sf::st_bbox(dseg))
    cx <- (bb[1] + bb[3]) / 2
    cy <- (bb[2] + bb[4]) / 2
    
    overlay_legend <- if (sp$mode == 1) {
      if (sp$is_cont) "Continious_Legend" else "Categorical_Legend"
    } else {
      c("Categorical_Legend", "Continious_Legend")
    }
    
    m <- leaflet(options = leafletOptions(minZoom = 2, preferCanvas = TRUE)) %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4]) %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%
      addCircleMarkers(lng = cx, lat = cy,radius = 1, stroke = FALSE,opacity = 0, fillOpacity = 0,group = "Continious_Legend", options = pathOptions(interactive = FALSE)) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "TopoMap", "Aerial"),
        overlayGroups = overlay_legend,
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("TopoMap") %>%
      hideGroup("Aerial") %>%
      addScaleBar(position = "topleft") %>%
      addPolylines(data = dseg,weight = s$linesize, opacity = s$linealpha, color  = ~.col, smoothFactor = 1)
    
    if (sp$mode == 1) {
      if (sp$is_cont) {
        mv_legend <- mv_attr1()
        vals <- as.numeric(sf::st_drop_geometry(mv_legend)[[sp$title]])
        vals <- vals[is.finite(vals)]
        if (!length(vals)) vals <- sp$legend_vals
        
        mn <- min(vals); mx <- max(vals)
        ticks_all <- pretty(c(mn, mx), n = 5)
        inner <- ticks_all[ticks_all > mn & ticks_all < mx]
        if (length(inner) >= 3) {
          idx <- round(seq(1, length(inner), length.out = 3))
          inner3 <- inner[idx]
        } else {
          inner3 <- seq(mn, mx, length.out = 5)[2:4]
        }
        t1 <- inner3[1]; t2 <- inner3[2]; t3 <- inner3[3]
        
        orig_vals <- sf::st_drop_geometry(mv_legend)[[sp$title]]
        unit_str  <- if (inherits(orig_vals, "units")) units::deparse_unit(orig_vals) else NULL
        title_txt <- if (!is.null(unit_str) && nzchar(unit_str)) paste0(sp$title, " (", unit_str, ")") else sp$title
        
        grad <- tags$div(
          class = "continious-legend",
          style = "background:rgba(255,255,255,0.85);padding:6px 8px;border-radius:4px;font-size:11px;",
          tags$div(htmlEscape(title_txt), style = "font-weight:600;margin-bottom:4px;"),
          tags$div(style = paste0(
            "width:220px;height:12px;background:linear-gradient(to right,",
            sp$pal(mn), ",", sp$pal(mx),
            ");border:1px solid rgba(0,0,0,0.25);margin-bottom:6px;"
          )),
          tags$div(style = "display:flex;justify-content:space-between;width:220px;opacity:0.9;",
                   tags$span(sprintf("%g", mn)),
                   tags$span(sprintf("%g", t1)),
                   tags$span(sprintf("%g", t2)),
                   tags$span(sprintf("%g", t3)),
                   tags$span(sprintf("%g", mx))),
          tags$div(style = "display:flex;justify-content:space-between;width:220px;opacity:0.7;",
                   tags$span("min"), tags$span(""), tags$span(""), tags$span(""), tags$span("max")),
          tags$div(style = "margin-top:6px;display:flex;align-items:center;gap:6px;opacity:0.85;",
                   tags$span(style = "display:inline-block;width:12px;height:12px;background:#BDBDBD;border:1px solid rgba(0,0,0,0.25);"),
                   tags$span("no data (NA)"))
        )
        m <- leaflet::addControl(m, html = as.character(grad), position = "topright")
      } else {
        m <- add_cat_legend(m, title = sp$title, labels = sp$legend_vals, colors = sp$cols, position = "topright", group = "Categorical_Legend")
      }
    } else {
      m <- add_cat_legend(m, title = sp$title_cat,
                          labels = names(sp$cat_legend),
                          colors = unname(sp$cat_legend),
                          position = "topright", group = "Categorical_Legend")
      
      rng <- sp$cont_range
      mn <- rng[1]; mx <- rng[2]
      
      ticks_all <- pretty(c(mn, mx), n = 5)
      inner <- ticks_all[ticks_all > mn & ticks_all < mx]
      if (length(inner) >= 3) {
        idx <- round(seq(1, length(inner), length.out = 3))
        inner3 <- inner[idx]
      } else {
        inner3 <- seq(mn, mx, length.out = 5)[2:4]
      }
      t1 <- inner3[1]; t2 <- inner3[2]; t3 <- inner3[3]
      
      g1 <- if (identical(sp$title_cont, paste0(s$cont_attr_2, " (Light to Dark)"))) "white" else "black"
      g2 <- if (identical(sp$title_cont, paste0(s$cont_attr_2, " (Light to Dark)"))) "black" else "white"
      
      grad2 <- tags$div(
        class = "continious-legend",
        style = "background:rgba(255,255,255,0.85);padding:6px 8px;border-radius:4px;font-size:11px;",
        tags$div(htmltools::htmlEscape(sp$title_cont), style = "font-weight:600;margin-bottom:4px;"),
        tags$div(style = paste0(
          "width:220px;height:12px;background:linear-gradient(to right,",
          g1, ",", g2,
          ");border:1px solid rgba(0,0,0,0.25);margin-bottom:6px;"
        )),
        tags$div(style = "display:flex;justify-content:space-between;width:220px;opacity:0.9;",
                 tags$span(sprintf("%g", mn)),
                 tags$span(sprintf("%g", t1)),
                 tags$span(sprintf("%g", t2)),
                 tags$span(sprintf("%g", t3)),
                 tags$span(sprintf("%g", mx))),
        tags$div(style = "display:flex;justify-content:space-between;width:220px;opacity:0.7;",
                 tags$span("min"), tags$span(""), tags$span(""), tags$span(""), tags$span("max")),
        tags$div(style = "margin-top:6px;display:flex;align-items:center;gap:6px;opacity:0.85;",
                 tags$span(style = "display:inline-block;width:12px;height:12px;background:#BDBDBD;border:1px solid rgba(0,0,0,0.25);"),
                 tags$span("no data (NA)"))
      )
      m <- leaflet::addControl(m, html = as.character(grad2), position = "topright")
    }
    
    m <- htmlwidgets::onRender(m, "
      function(el){
        var map = this;
        function set(on){
          el.querySelectorAll('.continious-legend').forEach(function(n){ n.style.display = on ? '' : 'none'; });
        }
        set(true);
        map.on('overlayadd', function(e){ if(e.name === 'Continious_Legend') set(true); });
        map.on('overlayremove', function(e){ if(e.name === 'Continious_Legend') set(false); });
      }
    ")
    
    m
  }
  
  ##map display layout
  output$maps_ui <- renderUI({
    s <- locked_settings()
    if (is.null(s)) return(div("Loading…"))
    ids <- s$animals
    if (is.null(ids) || length(ids) == 0)
      return(div(style = "color:red; font-weight:700; padding:10px;",
                 "Please select one or more animals."))
    
    if (identical(s$panel_mode, "Single panel")) {
      return(withSpinner(leafletOutput(ns("map_single"), height = "85vh"), type = 4, color = "blue", size = 0.9))
    }
    
    width <- 6
    cols <- lapply(seq_along(ids), function(i) {
      content <- tagList(
        tags$h5(paste("Track:", ids[i]),
                style = "text-align: center; margin-top: 5px; margin-bottom: 5px;"),
        withSpinner(leafletOutput(ns(paste0("map_", ids[i])), height = "45vh"), type = 4, color = "blue", size = 0.9)
      )
      column(width, content)
    })
    rows <- lapply(split(cols, ceiling(seq_along(cols) / 2)), function(chunk) do.call(fluidRow, chunk))
    tagList(rows)
  })
  
  #single panel leaflet map
  output$map_single <- renderLeaflet({
    shiny::validate(shiny::need(!is.null(locked_settings()) && !is.null(locked_mv()), "Loading…"))
    leaflet_map()
  })
  
  observe({
    s <- locked_settings()
    req(s, identical(s$panel_mode, "Multipanel"))
    ids <- s$animals
    if (is.null(ids) || length(ids) == 0) return()
    lapply(ids, function(id_i) {
      local({
        id_loc <- id_i
        output[[paste0("map_", id_loc)]] <- renderLeaflet({
          shiny::validate(shiny::need(!is.null(locked_settings()) && !is.null(locked_mv()), "Loading…"))
          leaflet_map(track_id = id_loc)
        })
      })
    })
  })
  
  
  ################## JSON-friendly hidden inputs#########################
 
  observeEvent(input$animals, {
    updateTextInput(
      session,
      "animals_json",
      value = paste(input$animals %||% character(0), collapse = ",")
    )
  }, ignoreInit = TRUE)
  observe({ updateTextInput(session, "attr_1_json", value = input$attr_1 %||% "")  })
  observe({updateTextInput(session, "cat_attr_2_json", value = input$cat_attr_2 %||% "")})
  observe({ updateTextInput(session, "cont_attr_2_json", value = input$cont_attr_2 %||% "") })
  ############################################################################
  
  # Downloads
  ##save html
  save_leaflet_html <- function(widget, html_path, selfcontained = TRUE) {
    htmlwidgets::saveWidget(widget, file = html_path, selfcontained = selfcontained)
    html_path
  }
  
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
        save_leaflet_html(leaflet_map(), file, selfcontained = TRUE)
        return(invisible())
      }
      
      td <- tempfile("tracks_html_"); dir.create(td)
      for (id in s$animals) {
        out <- file.path(td, paste0(id, "_", Sys.Date(), ".html"))
        save_leaflet_html(leaflet_map(track_id = id), out, selfcontained = TRUE)
      }
      zip::zipr(zipfile = file, files = list.files(td, full.names = TRUE))
    }
  )
  
  
  ##save png
  save_leaflet_png <- function(widget, png_path, vwidth = 1400L, vheight = 900L, delay = 2) {
    html_file <- file.path(tempdir(), "leaflet_export.html")
    
    htmlwidgets::saveWidget(widget, file = html_file, selfcontained = TRUE)
    
    html_file <- normalizePath(html_file, winslash = "/", mustWork = TRUE)
    html_url  <- paste0("file:///", html_file)
    
    Sys.sleep(delay)
    
    webshot2::webshot(url = html_url,file = png_path, vwidth = vwidth,vheight = vheight,cliprect = "viewport" )
    
    png_path
  }
  
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
        save_leaflet_png(leaflet_map(), file)
        shiny::validate(shiny::need(file.exists(file), "PNG export failed."))
        return(invisible())
      }
      
      td <- tempfile("tracks_png_"); dir.create(td)
      for (id in s$animals) {
        out <- file.path(td, paste0(id, "_", Sys.Date(), ".png"))
        save_leaflet_png(leaflet_map(track_id = id), out)
      }
      zip::zipr(zipfile = file, files = list.files(td, full.names = TRUE))
    }
  )
  
  return(reactive({
    req(current())
    current()
  }))
}