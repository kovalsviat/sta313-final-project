# ============================================================
# R/mod_map.R
# Owner: Person B
# Reads:  app_state$crime_type, app_state$year_range,
#         app_state$selected_hood, app_state$active_year
# Writes: app_state$selected_hood  via set_selected_hood()
#         app_state$detail_hood    via set_detail_hood()
#         app_state$hood_panel_open via set_hood_panel_open()
# ============================================================

mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"), height = "600px")
  )
}

mod_map_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    # ── Filtered data reactive ──────────────────────────────
    map_filtered <- reactive({
      # TODO: filter app_data$map_data by app_state$crime_type
      #       and app_state$year_range
      app_data$map_data
    })

    # ── Render base map ─────────────────────────────────────
    output$map <- renderLeaflet({
      # TODO: render choropleth using leaflet()
      #       colour intensity = crime rate for selected crime_type/year
      leaflet() |>
        addTiles()
    })

    # ── React to animation (active_year ticking) ────────────
    observe({
      req(app_state$active_year)
      # TODO: update choropleth fill colour for app_state$active_year
      #       using leafletProxy() — do NOT re-render the full map
    })

    # ── Single-click → filter charts, highlight neighbourhood ──
    observeEvent(input$map_shape_click, {
      hood_id <- input$map_shape_click$id
      setters$set_selected_hood(hood_id)
    })

    # ── Double-click → open neighbourhood detail panel ──────
    observeEvent(input$map_shape_dblclick, {
      hood_id <- input$map_shape_dblclick$id
      setters$set_detail_hood(hood_id)
      setters$set_hood_panel_open(TRUE)
    })
  })
}
