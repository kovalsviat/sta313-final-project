# ============================================================
# R/mod_map.R
# ============================================================

mod_map_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"), height = "500px")
}

mod_map_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {
    last_click_id <- reactiveVal(NULL)
    last_click_time <- reactiveVal(as.numeric(NA))

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
          crime_rate  = tidyr::replace_na(crime_rate, 0),
          crime_count = tidyr::replace_na(crime_count, 0)
        )
    })

    pal <- reactive({
      req(map_filtered())
      leaflet::colorNumeric(
        palette = "YlOrRd",
        domain  = map_filtered()$crime_rate,
        na.color = "#f0f0f0"
      )
    })

    output$map <- renderLeaflet({
      leaflet(
        options = leafletOptions(doubleClickZoom = FALSE)
      ) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -79.38, lat = 43.72, zoom = 11)
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
          fillOpacity  = 0.7,
          color        = "white",
          weight       = 0.8,
          opacity      = 1,
          label        = ~paste0(neighbourhood_name, ": ", round(crime_rate, 1), " per 10k"),
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
          values   = df$crime_rate,
          title    = paste0(app_state$crime_type, "<br>per 10k pop"),
          opacity  = 0.8
        )
    })

    observe({
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
