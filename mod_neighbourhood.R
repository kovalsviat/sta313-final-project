# ============================================================
# R/mod_neighbourhood.R
# Owner: Person C
# Reads:  app_state$detail_hood, app_state$hood_panel_open,
#         app_state$active_year, app_state$year_range
# Writes: app_state$hood_panel_open    via set_hood_panel_open()
#         app_state$compare_modal_open via set_compare_modal_open()
#         app_state$compare_hoods      via set_compare_hoods()
#         app_state$active_year        via set_active_year()
# ============================================================

mod_neighbourhood_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Panel is conditionally shown based on hood_panel_open state
    uiOutput(ns("hood_panel"))
  )
}

mod_neighbourhood_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    # ── Show / hide panel based on state ────────────────────
    output$hood_panel <- renderUI({
      req(app_state$hood_panel_open)
      req(app_state$detail_hood)
      ns <- session$ns
      tagList(
        div(class = "hood-panel",
          h4(app_state$detail_hood),

          # ── Star glyph ──────────────────────────────────
          plotlyOutput(ns("glyph"), height = "300px"),

          # ── Time slider (controls active_year) ──────────
          sliderInput(ns("hood_year"),
                      label = "Year",
                      min   = 2012, max = 2025,
                      value = 2025, step = 1, sep = ""),

          # ── Heatmap tab ─────────────────────────────────
          plotlyOutput(ns("heatmap"), height = "250px"),

          hr(),

          # ── Buttons ─────────────────────────────────────
          actionButton(ns("open_compare"), "Compare",
                       class = "btn-primary btn-sm"),
          actionButton(ns("close_panel"),  "Close",
                       class = "btn-outline-secondary btn-sm")
        )
      )
    })

    # ── Render star glyph ───────────────────────────────────
    output$glyph <- renderPlotly({
      req(app_state$detail_hood)
      # TODO: render star glyph from glyph_scaled for detail_hood
      #       overlay glyph_avg as pink reference shape
      plotly::plot_ly()
    })

    # ── Render heatmap ──────────────────────────────────────
    output$heatmap <- renderPlotly({
      req(app_state$detail_hood)
      # TODO: render heatmap from heatmap_neigh for detail_hood
      #       x = year, y = month, fill = crime count
      plotly::plot_ly()
    })

    # ── Year slider → update active_year ────────────────────
    observeEvent(input$hood_year, {
      setters$set_active_year(input$hood_year)
    }, ignoreInit = TRUE)

    # ── Compare button → open compare modal ─────────────────
    observeEvent(input$open_compare, {
      setters$set_compare_modal_open(TRUE)
    })

    # ── Close button → dismiss panel ────────────────────────
    observeEvent(input$close_panel, {
      setters$set_hood_panel_open(FALSE)
      setters$set_detail_hood(NULL)
    })
  })
}
