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

# A numeric attribute with more than this many distinct values is treated as
# continuous, everywhere in the app. The NOTE shown in the sidebar is built from
# the same constant so the two cannot drift apart.
ATTR_CAT_THRESHOLD <- 12

########### helpers ###########
# helper 1: Attribute type
continuous_attr <- function(vals, threshold = ATTR_CAT_THRESHOLD) {
  is_num <- is.numeric(vals) || inherits(vals, "units")
  if (!is_num) return(FALSE)
  n_unique <- length(unique(stats::na.omit(as.numeric(vals))))
  n_unique > threshold
}

## helper 2: the sf skeleton returned when there is nothing to draw
empty_segs <- function(tracks, values) {
  do.call(sf::st_sf, c(list(track_id = character(0)), values,
                       list(geometry = sf::st_sfc(crs = sf::st_crs(tracks)))))
}

## helper 2a: TRUE at every event that starts a segment, i.e. whose successor
## belongs to the same track.
segment_starts <- function(tracks) {
  id <- as.character(mt_track_id(tracks))
  c(id[-length(id)] == id[-1], FALSE)
}

## helper 2b: the value each segment carries. A continuous attribute is averaged
## over the two events the segment joins; a categorical one takes the value of
## the event it starts at.
segment_values <- function(vals, starts, continuous) {
  if (!continuous) return(as.character(vals[starts]))
  v   <- as.numeric(if (inherits(vals, "units")) units::drop_units(vals) else vals)
  out <- rowMeans(cbind(v[starts], v[which(starts) + 1]), na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

## helper 3: making segments with one attribute
make_segments_1attr <- function(tracks, attr_name, threshold = ATTR_CAT_THRESHOLD) {
  shape <- list(value = character(0))
  if (nrow(tracks) < 2) return(empty_segs(tracks, shape))

  starts <- segment_starts(tracks)
  if (!any(starts)) return(empty_segs(tracks, shape))

  vals <- sf::st_drop_geometry(tracks)[[attr_name]]
  sf::st_sf(
    track_id = as.character(mt_track_id(tracks))[which(starts)],
    value    = segment_values(vals, starts, continuous_attr(vals, threshold)),
    geometry = mt_segments(tracks)[starts]
  )
}

## helper 3a: making segments with two attributes
make_segments_2attr <- function(tracks, cat_name, cont_name) {
  shape <- list(cat = character(0), cont = numeric(0))
  if (nrow(tracks) < 2) return(empty_segs(tracks, shape))

  starts <- segment_starts(tracks)
  if (!any(starts)) return(empty_segs(tracks, shape))

  dd <- sf::st_drop_geometry(tracks)
  sf::st_sf(
    track_id = as.character(mt_track_id(tracks))[which(starts)],
    cat      = segment_values(dd[[cat_name]],  starts, continuous = FALSE),
    cont     = segment_values(dd[[cont_name]], starts, continuous = TRUE),
    geometry = mt_segments(tracks)[starts]
  )
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
    if (!is.null(nm) && nm %in% trkattrb) out <- mt_as_event_attribute(out, dplyr::all_of(nm))
  }
  out
}

# helper 7b: track ids can contain "/" (and on Windows "\\"), which breaks the
# per-track download paths: the file silently lands nowhere and the zip comes up
# short one track. Replace only the path separators, so every id that already
# works - including ids with spaces - keeps exactly the file name it has today.
safe_file_id <- function(x) gsub("[/\\\\]", "_", as.character(x))

# helper 8: names of columns usable as colouring attributes. Drops all-NA
# columns, timestamps and geometry. st_drop_geometry() only removes the *active*
# geometry, so any further sfc column has to be excluded explicitly or it would
# be offered and coloured by its WKT string.
usable_attr_names <- function(df) {
  if (is.null(df) || !ncol(df)) return(character(0))
  keep <- colSums(!is.na(df)) > 0
  for (cls in c("POSIXt", "Date", "sfc")) {
    keep <- keep & !vapply(df, inherits, logical(1), what = cls)
  }
  names(df)[keep]
}

# helper 8a: available attribute names from event + track data
get_attr_choices <- function(mv) {
  event_choices <- usable_attr_names(as.data.frame(sf::st_drop_geometry(mv)))
  track_choices <- setdiff(usable_attr_names(mt_track_data(mv)), event_choices)
  sort(unique(c(event_choices, track_choices)))
}

# helper 9: split attributes into all/cat/cont without converting all track attrs.
# Reads the event and track tables once and reuses them for both the name filter
# and the continuous/categorical test.
split_attr_choices <- function(mv, threshold = ATTR_CAT_THRESHOLD) {
  event_df <- as.data.frame(sf::st_drop_geometry(mv))
  td       <- mt_track_data(mv)

  event_choices <- usable_attr_names(event_df)
  track_choices <- setdiff(usable_attr_names(td), event_choices)
  all_names     <- sort(unique(c(event_choices, track_choices)))

  if (!length(all_names)) {
    return(list(all = character(0), cat = character(0), cont = character(0)))
  }

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

# helper 11: colours for a set of categorical levels, named by level. Used for
# both colouring options, which is why the palette spans every level in the full
# data rather than only the ones currently on screen.
build_cat_palette <- function(levels, palette_name) {
  n     <- length(levels)
  pname <- palette_name %||% "Glasbey"
  base  <- if (tolower(pname) == "glasbey") pals::glasbey(max(32, n))
  else RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[pname, "maxcolors"], pname)
  cols  <- if (n <= length(base)) base[seq_len(n)] else color_generator(base, n)
  stats::setNames(cols, levels)
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

# helper 12: the continuous gradient legend drawn in the map's top-right corner.
# Shared by both colouring options; they differ only in the title, the range and
# the two colours the bar runs between.
cont_gradient_legend <- function(title, mn, mx, col_from, col_to) {
  ticks_all <- pretty(c(mn, mx), n = 5)
  inner     <- ticks_all[ticks_all > mn & ticks_all < mx]
  inner3    <- if (length(inner) >= 3) {
    inner[round(seq(1, length(inner), length.out = 3))]
  } else {
    seq(mn, mx, length.out = 5)[2:4]
  }

  tags$div(
    class = "continious-legend",
    style = "background:rgba(255,255,255,0.85);padding:6px 8px;border-radius:4px;font-size:11px;",
    tags$div(htmlEscape(title), style = "font-weight:600;margin-bottom:4px;"),
    tags$div(style = paste0(
      "width:220px;height:12px;background:linear-gradient(to right,",
      col_from, ",", col_to,
      ");border:1px solid rgba(0,0,0,0.25);margin-bottom:6px;"
    )),
    tags$div(style = "display:flex;justify-content:space-between;width:220px;opacity:0.9;",
             tags$span(sprintf("%g", mn)),
             tags$span(sprintf("%g", inner3[1])),
             tags$span(sprintf("%g", inner3[2])),
             tags$span(sprintf("%g", inner3[3])),
             tags$span(sprintf("%g", mx))),
    tags$div(style = "display:flex;justify-content:space-between;width:220px;opacity:0.7;",
             tags$span("min"), tags$span(""), tags$span(""), tags$span(""), tags$span("max")),
    tags$div(style = "margin-top:6px;display:flex;align-items:center;gap:6px;opacity:0.85;",
             tags$span(style = "display:inline-block;width:12px;height:12px;background:#BDBDBD;border:1px solid rgba(0,0,0,0.25);"),
             tags$span("no data (NA)"))
  )
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
          uiOutput(ns("ui_color_controls_opt1")),
          div(tags$small(
            paste0("NOTE: Numeric attributes with fewer than ", ATTR_CAT_THRESHOLD,
                   " unique values are considered as categorical."),
            style = "color: darkblue;"
          )),
          div(tags$small(
            "* For details on colors contained in each palette, see the documentation of this App.",
            style = "color: darkblue;"
          ))
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Option 2: Color by 2 attributes'", ns("attr_mode")),
          fluidRow(
            column(6, uiOutput(ns("cat_attr_2_ui"))),
            column(6, selectInput(
              ns("cat_pal_2"), "Palette*",
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
            paste0("NOTE: Numeric attributes with fewer than ", ATTR_CAT_THRESHOLD,
                   " unique values are considered as categorical."),
            style = "color: darkblue;"
          )),
          div(tags$small(
            "* For details on colors contained in each palette, see the documentation of this App.",
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
      { .[as.character(mt_track_id(.)) %in%
            names(which(table(as.character(mt_track_id(.))) >= 2)), ] }
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
  attr_info_all <- reactive({ split_attr_choices(mv_all(), threshold = ATTR_CAT_THRESHOLD)  })
  
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
  # One select for each attribute picker: keep the restored value when it is
  # still among the choices, otherwise fall back to the first one.
  attr_select_ui <- function(id, label, choices) {
    restored <- isolate(input[[id]])
    sel <- if (!is.null(restored) && restored %in% choices) restored else if (length(choices)) choices[1] else NULL
    selectInput(ns(id), label, choices = choices, selected = sel)
  }

  output$attr_1_ui <- renderUI({
    attr_select_ui("attr_1", NULL, attr_choices_all())
  })

  output$cat_attr_2_ui <- renderUI({
    attr_select_ui("cat_attr_2", "Categorical Attribute", cat_cont_choices()$cat)
  })

  output$cont_attr_2_ui <- renderUI({
    attr_select_ui("cont_attr_2", "Continuous Attribute", cat_cont_choices()$cont)
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
    
    list(empty = FALSE, is_cont = continuous_attr(vals, threshold = ATTR_CAT_THRESHOLD))
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
      
      if (continuous_attr(vals_tmp, threshold = ATTR_CAT_THRESHOLD)) {
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
          column(6, colourpicker::colourInput(ns("col_low_1"), "Low", isolate(input$col_low_1) %||% "yellow")),
          column(6, colourpicker::colourInput(ns("col_high_1"), "High", isolate(input$col_high_1) %||% "blue"))
        )
      )
    } else {
      tagList(
        h4("Colors"),
        selectInput(ns("cat_pal_1"), "Palette*",choices  = c("Glasbey", "Set2", "Set3", "Dark2", "Paired", "Accent"),selected = isolate(input$cat_pal_1) %||% "Glasbey")
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
      segs <- make_segments_1attr(mv0, s$attr_1, threshold = ATTR_CAT_THRESHOLD)
      shiny::validate(shiny::need(nrow(segs) > 0, "No segments for selected animals."))
      
      vals <- segs$value
      # Classify from the raw attribute -- the same source make_segments_1attr()
      # branches on -- rather than re-deriving from the segment values. A
      # categorical attribute is safe either way (its segment values are
      # character, which continuous_attr() always calls FALSE), but averaging
      # consecutive events can change the unique-value count in the other
      # direction: >12 distinct raw values whose pairwise means collapse to <=12
      # would be built as continuous segments and then coloured categorically,
      # with col_low_1/col_high_1 never rendered and so NULL.
      is_cont <- continuous_attr(get_attr_values(mv0, s$attr_1), threshold = ATTR_CAT_THRESHOLD)
      
      if (is_cont) {
        low  <- s$col_low_1  %||% "yellow"
        high <- s$col_high_1 %||% "blue"
        
        mv_full <- as_event(data, s$attr_1)
        orig_vals <- sf::st_drop_geometry(mv_full)[[s$attr_1]]
        all_vals  <- if (inherits(orig_vals, "units")) units::drop_units(orig_vals) else orig_vals
        all_vals  <- as.numeric(all_vals)
        all_vals  <- all_vals[is.finite(all_vals)]
        rng       <- if (length(all_vals)) range(all_vals) else c(0, 1)
        
        pal <- colorNumeric(colorRampPalette(c(low, high))(256), domain = rng, na.color = NA)
        
        list(mode = 1, segs = segs, is_cont = TRUE, pal = pal, legend_vals = rng, title = s$attr_1)
      } else {
        mv_full   <- as_event(data, s$attr_1)
        vals_full <- sf::st_drop_geometry(mv_full)[[s$attr_1]]
        levs_all  <- sort(unique(stats::na.omit(as.character(vals_full))))
        levs      <- sort(unique(stats::na.omit(as.character(vals))))
        levs_all  <- sort(unique(c(levs_all, levs)))
        if (!length(levs_all)) levs_all <- levs
        
        cols_all <- build_cat_palette(levs_all, s$cat_pal_1)
        pal      <- colorFactor(unname(cols_all), domain = levs_all, na.color = NA)
        list(mode = 1, segs = segs, is_cont = FALSE, pal = pal,
             legend_vals = levs, cols = unname(cols_all[levs]), title = s$attr_1)
      }
      
    } else { #option2
      req(s$cat_attr_2, s$cont_attr_2)
      mv02 <- as_event(mv, c(s$cat_attr_2, s$cont_attr_2))
      segs <- make_segments_2attr(mv02, s$cat_attr_2, s$cont_attr_2)
      shiny::validate(shiny::need(nrow(segs) > 0, "No segments for selected animals."))
      
      cat_full  <- sf::st_drop_geometry(as_event(data, s$cat_attr_2))[[s$cat_attr_2]]
      levs      <- sort(unique(stats::na.omit(as.character(segs$cat))))
      levs_all  <- sort(unique(c(sort(unique(stats::na.omit(as.character(cat_full)))), levs)))
      if (!length(levs_all)) levs_all <- levs
      
      cols_base <- build_cat_palette(levs_all, s$cat_pal_2)
      
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
        legend_levs = levs,
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
      
      hex <- rep(NA_character_, length(base_vec))
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

        orig_vals <- sf::st_drop_geometry(mv_legend)[[sp$title]]
        unit_str  <- if (inherits(orig_vals, "units")) units::deparse_unit(orig_vals) else NULL
        title_txt <- if (!is.null(unit_str) && nzchar(unit_str)) paste0(sp$title, " (", unit_str, ")") else sp$title

        grad <- cont_gradient_legend(title_txt, mn, mx, sp$pal(mn), sp$pal(mx))
        m <- leaflet::addControl(m, html = as.character(grad), position = "topright")
      } else {
        m <- add_cat_legend(m, title = sp$title, labels = sp$legend_vals, colors = sp$cols, position = "topright", group = "Categorical_Legend")
      }
    } else {
      leg_levs <- sp$legend_levs %||% names(sp$cat_legend)
      m <- add_cat_legend(m, title = sp$title_cat,
                          labels = leg_levs,
                          colors = unname(sp$cat_legend[leg_levs]),
                          position = "topright", group = "Categorical_Legend")
      
      rng <- sp$cont_range
      mn <- rng[1]; mx <- rng[2]

      # The bar shows the shading direction, so it runs white -> black for
      # "Light to Dark" and the other way round otherwise. Read the direction
      # from the setting itself rather than by re-parsing the legend title.
      light_to_dark <- identical(s$cont_pal_2, "Light to Dark")
      g1 <- if (light_to_dark) "white" else "black"
      g2 <- if (light_to_dark) "black" else "white"

      grad2 <- cont_gradient_legend(sp$title_cont, mn, mx, g1, g2)
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
        withSpinner(leafletOutput(ns(paste0("map_", i)), height = "45vh"), type = 4, color = "blue", size = 0.9)
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
    
    lapply(seq_along(ids), function(i) {
      local({
        id_loc <- ids[i]
        output[[paste0("map_", i)]] <- renderLeaflet({
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
  json_mirrors <- c(attr_1 = "attr_1_json", cat_attr_2 = "cat_attr_2_json",
                    cont_attr_2 = "cont_attr_2_json")
  lapply(names(json_mirrors), function(src) {
    observe({ updateTextInput(session, json_mirrors[[src]], value = input[[src]] %||% "") })
  })
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
        out <- file.path(td, paste0(safe_file_id(id), "_", Sys.Date(), ".html"))
        save_leaflet_html(leaflet_map(track_id = id), out, selfcontained = TRUE)
      }
      zip::zipr(zipfile = file, files = list.files(td, full.names = TRUE))
    }
  )
  
  
  ##save png
  save_leaflet_png <- function(widget, png_path, vwidth = 1400L, vheight = 900L, delay = 2) {
    # Render the widget to an HTML file first. selfcontained = FALSE writes the
    # map plus a sidecar "<name>_files/" dir instead of bundling everything into
    # one file. The bundling step is the only thing that needs pandoc and is
    # pointless here: the headless browser loads the local file (and its sidecar)
    # directly. Using FALSE drops the pandoc dependency for PNG export.
    html_file <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(widget, file = html_file, selfcontained = FALSE)
    html_file <- normalizePath(html_file, winslash = "/", mustWork = TRUE)

    # webshot2/chromote drive headless Chrome over the SAME global `later` event
    # loop that Shiny is already running. Calling it directly from a Shiny handler
    # re-enters that loop and deadlocks: the R process spins at ~100% CPU, the
    # screenshot never completes, and the download surfaces as a gateway 500 /
    # "connection prematurely closed". Running it in a separate R process via
    # callr gives chromote its own event loop and avoids the deadlock.
    callr::r(
      function(html_file, png_path, vwidth, vheight, delay) {
        # chromote does not pass these itself: default_chrome_args() is only
        # srgb/extensions/mute-audio, and there is no env-var route, so they have
        # to be set here, in the subprocess, before Chrome is launched. Inside the
        # MoveApps container Docker caps /dev/shm at 64 MB, so Chrome's renderer
        # crashes part-way through a large map and chromote reports "Session and
        # underlying target have been closed". The image cannot raise --shm-size
        # (MoveApps owns `docker run`), so tell Chrome not to use /dev/shm at all.
        # --no-sandbox covers the seccomp-blocked user namespace.
        chromote::set_chrome_args(c(
          "--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu",
          chromote::default_chrome_args()
        ))
        # A cold container can take longer to boot Chrome than chromote's 10s.
        options(chromote.timeout = 60)
        webshot2::webshot(
          url = html_file, file = png_path,
          vwidth = vwidth, vheight = vheight, cliprect = "viewport", delay = delay
        )
      },
      args = list(html_file = html_file, png_path = png_path,
                  vwidth = vwidth, vheight = vheight, delay = delay)
    )

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
        out <- file.path(td, paste0(safe_file_id(id), "_", Sys.Date(), ".png"))
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