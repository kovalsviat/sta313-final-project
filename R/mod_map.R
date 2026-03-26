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

    # ── Join crime data to spatial boundaries ───────────────
    map_filtered <- reactive({
      req(app_data$neighbourhoods_sf)
      req(app_data$crime_rate_all)

      crime <- app_data$crime_rate_all %>%
        dplyr::filter(
          CSI_CATEGORY == app_state$crime_type,
          year         == app_state$selected_year
        ) %>%
        dplyr::select(HOOD_158, crime_count, crime_rate)

      # Left join so neighbourhoods with 0 crimes still appear
      app_data$neighbourhoods_sf %>%
        dplyr::left_join(crime, by = "HOOD_158") %>%
        dplyr::mutate(
          crime_rate  = tidyr::replace_na(crime_rate, 0),
          crime_count = tidyr::replace_na(crime_count, 0)
        )
    })

    # ── Colour palette ──────────────────────────────────────
    pal <- reactive({
      req(map_filtered())
      leaflet::colorNumeric(
        palette = "YlOrRd",
        domain  = map_filtered()$crime_rate,
        na.color = "#f0f0f0"
      )
    })

    # ── Render base map (once) ──────────────────────────────
    output$map <- renderLeaflet({
      leaflet() %>%
        # Minimal CartoDB tiles — clean, low visual noise
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -79.38, lat = 43.72, zoom = 11)
    })

    # ── Update choropleth when filters change ───────────────
    # leafletProxy avoids full re-render on every filter change
    observe({
      req(map_filtered(), pal())
      df  <- map_filtered()
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
            color       = "white",
            weight      = 2,
            fillOpacity = 0.9,
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

    # ── Highlight selected_hood ─────────────────────────────
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
            fillOpacity = 0
          )
      }
    })

    # ── Single-click → set selected_hood ───────────────────
    observeEvent(input$map_shape_click, {
      hood_id <- input$map_shape_click$id
      req(hood_id)
      setters$set_selected_hood(hood_id)
    })

    # ── Double-click → open neighbourhood panel ─────────────
    observeEvent(input$map_shape_dblclick, {
      hood_id <- input$map_shape_dblclick$id
      req(hood_id)
      setters$set_selected_hood(hood_id)
      setters$set_detail_hood(hood_id)
      setters$set_active_year(app_state$selected_year)
      setters$set_hood_panel_open(TRUE)
    })
  })
}