# ============================================================
# R/mod_neighbourhood.R
# Owner: Person C
# Reads:  app_state$detail_hood, app_state$hood_panel_open,
#         app_state$active_year, app_state$crime_type
# Writes: app_state$hood_panel_open    via set_hood_panel_open()
#         app_state$detail_hood        via set_detail_hood()
#         app_state$compare_modal_open via set_compare_modal_open()
#         app_state$active_year        via set_active_year()
#         app_state$is_playing         via set_is_playing()
#
# Note on active_year:
#   - Initialised by mod_map on double-click (set to selected_year)
#   - After that, Person C owns it exclusively via year slider + play button
#   - Changing sidebar selected_year does NOT affect active_year once panel is open
# ============================================================

mod_neighbourhood_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("hood_panel"))
}

mod_neighbourhood_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    # ── Show / hide panel ───────────────────────────────────
    output$hood_panel <- renderUI({
      req(isTRUE(app_state$hood_panel_open))
      req(app_state$detail_hood)
      ns <- session$ns

      div(class = "hood-panel",

        # ── Header ────────────────────────────────────────
        div(class = "hood-panel-header",
          h4(app_state$detail_hood),
          actionButton(ns("close_panel"), icon("times"),
                       class = "btn-close-panel")
        ),

        # ── Star glyph ────────────────────────────────────
        plotlyOutput(ns("glyph"), height = "300px"),

        # ── Year slider (controls active_year independently) ─
        sliderInput(
          inputId = ns("active_year"),
          label   = "Year",
          min     = 2020,
          max     = 2025,
          value   = app_state$active_year,
          step    = 1,
          sep     = "",
          ticks   = TRUE
        ),

        # ── Play / pause controls ────────────────────────
        div(class = "play-controls",
          actionButton(ns("play"),  icon("play"),
                       class = "btn-outline-primary btn-sm"),
          actionButton(ns("pause"), icon("pause"),
                       class = "btn-outline-secondary btn-sm")
        ),

        # ── Heatmap ───────────────────────────────────────
        plotlyOutput(ns("heatmap"), height = "250px"),

        hr(),

        # ── Compare button ────────────────────────────────
        actionButton(ns("open_compare"), "Compare Neighbourhoods",
                     class = "btn-primary btn-sm w-100")
      )
    })

    # ── Render star glyph ───────────────────────────────────
    output$glyph <- renderPlotly({
      req(app_state$detail_hood)
      req(app_state$active_year)
      # TODO: filter app_data$glyph_scaled to detail_hood + active_year
      #       render star glyph — one axis per variable
      #       overlay app_data$glyph_avg as pink reference shape
      plotly::plot_ly()
    })

    # ── Render heatmap ──────────────────────────────────────
    output$heatmap <- renderPlotly({
      req(app_state$detail_hood)
      # TODO: filter app_data$heatmap_neigh to detail_hood + crime_type
      #       x = year, y = month, fill = crime count
      plotly::plot_ly()
    })

    # ── Year slider → update active_year ────────────────────
    observeEvent(input$active_year, {
      setters$set_active_year(input$active_year)
    }, ignoreInit = TRUE)

    # ── Play → start animation ───────────────────────────────
    observeEvent(input$play, {
      setters$set_is_playing(TRUE)
    })

    # ── Pause → stop animation ───────────────────────────────
    observeEvent(input$pause, {
      setters$set_is_playing(FALSE)
    })

    # ── Animation timer ─────────────────────────────────────
    observe({
      req(isTRUE(app_state$is_playing))
      invalidateLater(800, session)
      isolate({
        current <- app_state$active_year
        if (current >= 2025) {
          setters$set_is_playing(FALSE)
        } else {
          new_year <- current + 1
          setters$set_active_year(new_year)
          updateSliderInput(session, "active_year", value = new_year)
        }
      })
    })

    # ── Compare button → open modal ──────────────────────────
    observeEvent(input$open_compare, {
      setters$set_compare_modal_open(TRUE)
    })

    # ── Close panel ──────────────────────────────────────────
    # Clears detail_hood and stops animation
    # selected_hood is NOT cleared — map highlight persists
    observeEvent(input$close_panel, {
      setters$set_is_playing(FALSE)
      setters$set_detail_hood(NULL)
      setters$set_hood_panel_open(FALSE)
    })
  })
}