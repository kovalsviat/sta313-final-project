# ============================================================
# R/mod_map.R
# Owner: Person B
# Reads:  app_state$crime_type, app_state$selected_year,
#         app_state$selected_hood
# Writes: app_state$selected_hood    via set_selected_hood()
#         app_state$detail_hood      via set_detail_hood()
#         app_state$hood_panel_open  via set_hood_panel_open()
#         app_state$active_year      via set_active_year()
# ============================================================

mod_map_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = "500px")
}

mod_map_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {
    last_click_id <- reactiveVal(NULL)
    last_click_time <- reactiveVal(as.numeric(NA))
    default_lng <- -79.38
    default_lat <- 43.72
    default_zoom <- 11

    redraw_selected_highlight <- function() {
      req(app_data$neighbourhoods_sf)
      hood <- app_state$selected_hood

      proxy <- leafletProxy(session$ns("map"))

      if (is.null(hood) || hood == "") {
        proxy %>% clearGroup("highlight")
      } else {
        selected_sf <- app_data$neighbourhoods_sf %>%
          dplyr::filter(HOOD_158 == hood)
        proxy %>%
          clearGroup("highlight") %>%
          addPolygons(
            data        = selected_sf,
            group       = "highlight",
            fillColor   = "transparent",
            color       = "#2c7bb6",
            weight      = 3,
            opacity     = 1,
            fillOpacity = 0,
            options     = leaflet::pathOptions(interactive = FALSE)
          )
      }
    }

    map_filtered <- reactive({
      req(app_data$neighbourhoods_sf)
      req(app_data$crime_rate_all)

      crime <- app_data$crime_rate_all %>%
        dplyr::filter(
          CSI_CATEGORY == app_state$crime_type,
          year         == app_state$selected_year
        ) %>%
        dplyr::select(HOOD_158, crime_count, crime_rate)

      app_data$neighbourhoods_sf %>%
        dplyr::left_join(crime, by = "HOOD_158") %>%
        dplyr::mutate(
          crime_count = tidyr::replace_na(crime_count, 0),
          label_text  = dplyr::if_else(
            is.na(crime_rate),
            paste0(neighbourhood_name, ": No data"),
            paste0(neighbourhood_name, ": ", round(crime_rate, 1), " per 10k")
          )
        )
    })

    palette_values <- reactive({
      req(app_data$crime_rate_all)

      vals <- app_data$crime_rate_all %>%
        dplyr::filter(CSI_CATEGORY == app_state$crime_type) %>%
        dplyr::pull(crime_rate)

      vals[is.finite(vals)]
    })

    palette_bins <- reactive({
      vals <- palette_values()

      if (length(vals) == 0) {
        return(c(0, 1))
      }

      max_val <- max(vals)
      base_bins <- switch(
        app_state$crime_type,
        "Auto Theft" = c(0, 10, 20, 30, 45, 70, 100),
        "Assault" = c(0, 30, 50, 75, 100, 140, 200),
        "Break and Enter" = c(0, 10, 15, 25, 35, 50, 70),
        "Robbery" = c(0, 3, 6, 10, 15, 22, 30),
        "Theft Over" = c(0, 2, 4, 6, 10, 15, 25),
        c(0, stats::quantile(vals, probs = c(0.2, 0.4, 0.6, 0.75, 0.9), na.rm = TRUE, names = FALSE))
      )

      bins <- if (max_val > max(base_bins)) {
        c(base_bins, max_val)
      } else {
        c(base_bins, max(base_bins) + 1)
      }

      bins <- unique(bins)

      if (length(bins) < 2) {
        return(c(vals[[1]], vals[[1]] + 1))
      }

      bins
    })

    pal <- reactive({
      req(palette_values(), palette_bins())
      leaflet::colorBin(
        palette = c("#fff5eb", "#fee6ce", "#fdd0a2", "#fdae6b", "#fd8d3c", "#e6550d", "#a63603"),
        domain  = palette_values(),
        bins    = palette_bins(),
        pretty  = FALSE,
        na.color = "#f0f0f0"
      )
    })

    output$map <- renderLeaflet({
      leaflet(
        options = leafletOptions(doubleClickZoom = FALSE)
      ) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = default_lng, lat = default_lat, zoom = default_zoom)
    })

    observe({
      req(map_filtered(), pal())
      df <- map_filtered()
      pal_fn <- pal()

      leafletProxy(session$ns("map")) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          data         = df,
          layerId      = ~HOOD_158,
          fillColor    = ~pal_fn(crime_rate),
          fillOpacity  = 0.85,
          color        = "white",
          weight       = 0.8,
          opacity      = 1,
          label        = ~label_text,
          labelOptions = labelOptions(
            style     = list("font-size" = "12px"),
            direction = "auto"
          ),
          highlightOptions = highlightOptions(
            color        = "white",
            weight       = 2,
            fillOpacity  = 0.9,
            bringToFront = FALSE
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal      = pal_fn,
          values   = palette_values(),
          title    = paste0(
            app_state$crime_type,
            "<br>per 10k pop",
            "<br><span style='font-weight: normal;'>custom bins fixed across years</span>"
          ),
          labFormat = leaflet::labelFormat(digits = 1),
          opacity  = 0.8
        )

      redraw_selected_highlight()
    })

    observe({
      redraw_selected_highlight()
    })

    observe({
      req(app_data$neighbourhoods_sf)
      hood <- app_state$selected_hood
      proxy <- leafletProxy(session$ns("map"))

      if (is.null(hood) || hood == "") {
        proxy %>% setView(lng = default_lng, lat = default_lat, zoom = default_zoom)
        return()
      }

      selected_sf <- app_data$neighbourhoods_sf %>%
        dplyr::filter(HOOD_158 == hood)

      if (nrow(selected_sf) == 0) {
        proxy %>% setView(lng = default_lng, lat = default_lat, zoom = default_zoom)
        return()
      }

      bbox <- sf::st_bbox(selected_sf)
      proxy %>%
        fitBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        )
    })

    observeEvent(input$map_shape_click, {
      hood_id <- input$map_shape_click$id
      req(hood_id)

      current_time <- as.numeric(Sys.time())
      is_fast_repeat <- identical(last_click_id(), hood_id) &&
        !is.na(last_click_time()) &&
        (current_time - last_click_time()) <= 0.4

      setters$set_selected_hood(hood_id)

      if (is_fast_repeat) {
        setters$set_detail_hood(hood_id)
        setters$set_active_year(app_state$selected_year)
        setters$set_hood_panel_open(TRUE)
        last_click_id(NULL)
        last_click_time(as.numeric(NA))
      } else {
        last_click_id(hood_id)
        last_click_time(current_time)
      }
    })

    observeEvent(input$map_shape_dblclick, {
      hood_id <- input$map_shape_dblclick$id
      req(hood_id)

      message(sprintf("[mod_map] map_shape_dblclick fired for hood %s", hood_id))
      setters$set_selected_hood(hood_id)
      setters$set_detail_hood(hood_id)
      setters$set_active_year(app_state$selected_year)
      setters$set_hood_panel_open(TRUE)
    })
  })
}
