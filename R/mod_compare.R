# ============================================================
# R/mod_compare.R
# Owner: Person C
# Reads:  app_state$compare_modal_open, app_state$compare_hoods,
#         app_state$detail_hood, app_state$active_year
# Writes: app_state$compare_modal_open via set_compare_modal_open()
#         app_state$compare_hoods      via set_compare_hoods()
#
# Note on active_year:
#   Glyphs in the modal reflect app_state$active_year, which is driven
#   by the year slider / play button in the neighbourhood panel.
#   The modal itself does not expose its own year control.
# ============================================================

mod_compare_ui <- function(id) {
  ns <- NS(id)
  # Always mounted; shown/hidden via app_state$compare_modal_open
  uiOutput(ns("compare_modal"))
}

mod_compare_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    # ── Show / remove modal based on state ──────────────────
    observe({
      if (isTRUE(app_state$compare_modal_open)) {
        showModal(modalDialog(
          title     = paste("Compare Neighbourhoods —", app_state$active_year),
          size      = "xl",
          easyClose = FALSE,
          footer    = actionButton(session$ns("close_compare"), "Close",
                                   class = "btn-secondary"),

          # ── Neighbourhood selector ───────────────────────
          # detail_hood is pre-selected; user picks others to compare
          selectizeInput(
            inputId  = session$ns("hood_select"),
            label    = "Select neighbourhoods to compare (max 4)",
            choices  = NULL,   # TODO: populate from unique HOOD_158 values in app_data
            selected = app_state$detail_hood,
            multiple = TRUE,
            options  = list(maxItems = 4)
          ),

          # ── View mode toggle ─────────────────────────────
          radioButtons(
            inputId  = session$ns("compare_mode"),
            label    = "View mode",
            choices  = c("Overlay" = "overlay", "Side by side" = "sidebyside"),
            selected = "overlay",
            inline   = TRUE
          ),

          # ── Glyph output ─────────────────────────────────
          uiOutput(session$ns("glyph_output"))
        ))
      } else {
        removeModal()
      }
    })

    # ── Hood selection → update compare_hoods ───────────────
    observeEvent(input$hood_select, {
      setters$set_compare_hoods(input$hood_select)
    }, ignoreInit = TRUE)

    # ── Render glyphs ────────────────────────────────────────
    output$glyph_output <- renderUI({
      req(app_state$compare_hoods)
      mode <- input$compare_mode

      if (mode == "overlay") {
        # All hoods as overlapping traces in one plot
        plotlyOutput(session$ns("glyph_overlay"), height = "400px")
      } else {
        # One plot per hood in a row — small multiples principle
        fluidRow(
          lapply(app_state$compare_hoods, function(h) {
            column(3,
              h6(h),
              plotlyOutput(session$ns(paste0("glyph_", h)), height = "280px")
            )
          })
        )
      }
    })

    # ── Overlay glyph ────────────────────────────────────────
    output$glyph_overlay <- renderPlotly({
      req(app_state$compare_hoods)
      req(app_state$active_year)
      # TODO: filter app_data$glyph_scaled to compare_hoods + active_year
      #       one trace per neighbourhood, different colours
      #       overlay app_data$glyph_avg as grey reference shape
      plotly::plot_ly()
    })

    # TODO: add individual renderPlotly outputs for each hood in side-by-side mode
    # Pattern: output[[paste0("glyph_", hood_id)]] <- renderPlotly({ ... })

    # ── Close modal ──────────────────────────────────────────
    observeEvent(input$close_compare, {
      setters$set_compare_modal_open(FALSE)
    })
  })
}
