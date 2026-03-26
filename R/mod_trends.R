# ============================================================
# R/mod_trends.R
# Owner: Person A
# Reads:  app_state$crime_type, app_state$selected_year,
#         app_state$selected_hood
# Writes: app_state$crime_type via set_crime_type()
#           (clicking a line updates the active crime type)
# Note:   NO play/pause — animation lives in mod_neighbourhood only
# ============================================================

mod_trends_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("line_chart"), height = "400px")
}

mod_trends_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    # ── Filtered data reactive ──────────────────────────────
    trends_data <- reactive({
      # TODO: if app_state$selected_hood is set, use app_data$line_neigh_year
      #       filtered to that neighbourhood; otherwise use app_data$line_city_year
      # TODO: filter by app_state$crime_type
      # Show ALL years — selected_year is shown as a marker, not a filter
      app_data$line_city_year
    })

    # ── Render line chart ───────────────────────────────────
    output$line_chart <- renderPlotly({
      # TODO: plotly line chart
      #       x = year, y = crime_rate
      #       add a vertical dashed line at app_state$selected_year
      #       so users can see where the map/bar are currently pinned
      #       hovering a line updates crime_type via plotly click event
      plotly::plot_ly()
    })

    # ── Clicking a line → update crime_type filter ──────────
    observeEvent(event_data("plotly_click", source = "line_chart"), {
      click <- event_data("plotly_click", source = "line_chart")
      # TODO: extract crime type from click$curveNumber or customdata
      # setters$set_crime_type(...)
    })
  })
}
