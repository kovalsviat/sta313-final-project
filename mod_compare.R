# ============================================================
# R/mod_compare.R
# Owner: Person C
# Reads:  app_state$compare_modal_open, app_state$compare_hoods,
#         app_state$detail_hood
# Writes: app_state$compare_modal_open via set_compare_modal_open()
#         app_state$compare_hoods      via set_compare_hoods()
# ============================================================

mod_compare_ui <- function(id) {
  ns <- NS(id)
  # Modal is always mounted in the DOM; shown/hidden via app_state
  uiOutput(ns("compare_modal"))
}

mod_compare_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    # ── Watch state to show / remove modal ──────────────────
    observe({
      if (isTRUE(app_state$compare_modal_open)) {
        showModal(modalDialog(
          title  = "Compare Neighbourhoods",
          size   = "xl",
          easyClose = FALSE,
          footer = tagList(
            actionButton(session$ns("close_compare"), "Close",
                         class = "btn-secondary")
          ),

          # ── Neighbourhood selector ─────────────────────
          selectizeInput(
            inputId  = session$ns("hood_select"),
            label    = "Select neighbourhoods to compare",
            choices  = NULL,   # TODO: populate from app_data neighbourhood list
            multiple = TRUE,
            options  = list(maxItems = 4)
          ),

          # ── View toggle: overlay vs side-by-side ───────
          radioButtons(
            inputId  = session$ns("compare_mode"),
            label    = "View mode",
            choices  = c("Overlay" = "overlay", "Side by side" = "sidebyside"),
            selected = "overlay",
            inline   = TRUE
          ),

          # ── Glyph output ───────────────────────────────
          uiOutput(session$ns("glyph_output"))
        ))
      } else {
        removeModal()
      }
    })

    # ── Neighbourhood selection → update compare_hoods ──────
    observeEvent(input$hood_select, {
      setters$set_compare_hoods(input$hood_select)
    }, ignoreInit = TRUE)

    # ── Render glyphs (overlay or side-by-side) ─────────────
    output$glyph_output <- renderUI({
      req(app_state$compare_hoods)
      hoods <- app_state$compare_hoods
      mode  <- input$compare_mode

      if (mode == "overlay") {
        # Single plotly output with all hoods overlaid
        plotlyOutput(session$ns("glyph_overlay"), height = "400px")
      } else {
        # One plotly output per hood arranged in columns
        lapply(hoods, function(h) {
          column(3, plotlyOutput(session$ns(paste0("glyph_", h)),
                                 height = "300px"))
        })
      }
    })

    output$glyph_overlay <- renderPlotly({
      req(app_state$compare_hoods)
      # TODO: render overlaid star glyphs for all compare_hoods
      #       one trace per neighbourhood
      plotly::plot_ly()
    })

    # TODO: add individual renderPlotly outputs for side-by-side mode

    # ── Close modal ─────────────────────────────────────────
    observeEvent(input$close_compare, {
      setters$set_compare_modal_open(FALSE)
    })
  })
}
