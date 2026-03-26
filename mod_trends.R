# ============================================================
# R/mod_trends.R
# Owner: Person A
# Reads:  app_state$crime_type, app_state$year_range,
#         app_state$selected_hood, app_state$is_playing
# Writes: app_state$active_year  via set_active_year()
#         app_state$is_playing   via set_is_playing()
# ============================================================

mod_trends_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("line_chart"), height = "400px"),
    hr(),
    actionButton(ns("play"), label = icon("play"),
                 class = "btn-outline-primary btn-sm"),
    actionButton(ns("pause"), label = icon("pause"),
                 class = "btn-outline-secondary btn-sm")
  )
}

mod_trends_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    # ── Filtered data reactive ──────────────────────────────
    trends_filtered <- reactive({
      # TODO: filter app_data$line_city_year (or line_neigh_year when a
      #       neighbourhood is selected) by crime_type and year_range
      app_data$line_city_year
    })

    # ── Render line chart ───────────────────────────────────
    output$line_chart <- renderPlotly({
      # TODO: build plotly line chart from trends_filtered()
      #       x = year, y = crime_rate, colour = crime_type or neighbourhood
      plotly::plot_ly()
    })

    # ── Play button → start animation loop ─────────────────
    observeEvent(input$play, {
      setters$set_is_playing(TRUE)
    })

    # ── Pause button → stop animation loop ─────────────────
    observeEvent(input$pause, {
      setters$set_is_playing(FALSE)
    })

    # ── Animation timer ─────────────────────────────────────
    observe({
      req(app_state$is_playing)
      invalidateLater(800, session)   # advance every 800 ms
      isolate({
        current <- app_state$active_year %||% app_state$year_range[1]
        if (current >= app_state$year_range[2]) {
          setters$set_is_playing(FALSE)
        } else {
          setters$set_active_year(current + 1)
        }
      })
    })
  })
}
