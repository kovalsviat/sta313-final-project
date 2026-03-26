# ============================================================
# R/mod_barchart.R
# Owner: Person A
# Reads:  app_state$crime_type, app_state$year_range,
#         app_state$selected_hood
# Writes: app_state$crime_type  via set_crime_type()
#         (clicking a bar updates the active crime type filter)
# ============================================================

mod_barchart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("bar_chart"), height = "250px")
  )
}

mod_barchart_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    # ── Filtered data reactive ──────────────────────────────
    bar_filtered <- reactive({
      # TODO: filter app_data$bar_category_city by year_range
      #       if selected_hood is set, use neighbourhood-level data instead
      app_data$bar_category_city
    })

    # ── Render bar chart ────────────────────────────────────
    output$bar_chart <- renderPlotly({
      # TODO: horizontal bar chart, bar length = count/proportion
      #       highlight the currently selected crime_type bar
      plotly::plot_ly()
    })

    # ── Clicking a bar updates crime_type filter ────────────
    observeEvent(event_data("plotly_click", source = "bar_chart"), {
      click <- event_data("plotly_click", source = "bar_chart")
      # TODO: extract crime type from click$y and call setter
      # setters$set_crime_type(click$y)
    })
  })
}
