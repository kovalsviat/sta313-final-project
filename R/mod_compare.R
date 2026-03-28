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

    observeEvent(app_state$compare_modal_open, {
      if (
        isTRUE(app_state$compare_modal_open) &&
        length(app_state$compare_hoods) == 0 &&
        !is.null(app_state$detail_hood)
      ) {
        setters$set_compare_hoods(app_state$detail_hood)
      }
    }, ignoreInit = TRUE)

    observeEvent(app_state$compare_modal_open, {
      if (!isTRUE(app_state$compare_modal_open)) {
        removeModal()
        return()
      }

      selected_hoods <- isolate(unique(c(app_state$detail_hood, app_state$compare_hoods)))
      selected_hoods <- selected_hoods[!is.na(selected_hoods) & nzchar(selected_hoods)]
      active_year <- isolate(app_state$active_year)

      removeModal()
      showModal(modalDialog(
        title     = paste("Compare Neighbourhoods -", active_year),
        size      = "xl",
        easyClose = FALSE,
        footer    = actionButton(
          session$ns("close_compare"),
          "Close",
          class = "btn-secondary"
        ),

        # detail_hood is pre-selected; user picks others to compare
        selectizeInput(
          inputId  = session$ns("hood_select"),
          label    = "Select neighbourhoods to compare (max 4)",
          choices  = {
            hoods <- app_data$glyph_scaled %>%
              dplyr::distinct(HOOD_158, neighbourhood_name) %>%
              dplyr::arrange(neighbourhood_name)
            stats::setNames(hoods$HOOD_158, hoods$neighbourhood_name)
          },
          selected = selected_hoods,
          multiple = TRUE,
          options  = list(maxItems = 4)
        ),

        radioButtons(
          inputId  = session$ns("compare_mode"),
          label    = "View mode",
          choices  = c("Overlay" = "overlay", "Side by side" = "sidebyside"),
          selected = "overlay",
          inline   = TRUE
        ),

        uiOutput(session$ns("glyph_output"))
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$hood_select, {
      setters$set_compare_hoods(input$hood_select)
    }, ignoreInit = TRUE)

    output$glyph_output <- renderUI({
      compare_hoods <- unique(c(app_state$detail_hood, app_state$compare_hoods))
      compare_hoods <- compare_hoods[!is.na(compare_hoods) & nzchar(compare_hoods)]
      req(length(compare_hoods) > 0)
      mode <- input$compare_mode

      if (mode == "overlay") {
        plotlyOutput(session$ns("glyph_overlay"), height = "400px")
      } else {
        n_hoods <- length(compare_hoods)
        plot_height <- switch(
          as.character(n_hoods),
          "1" = "420px",
          "2" = "420px",
          "3" = "480px",
          "4" = "460px",
          "420px"
        )
        col_width <- switch(
          as.character(n_hoods),
          "1" = 12,
          "2" = 6,
          "3" = 4,
          "4" = 6,
          3
        )

        fluidRow(
          lapply(compare_hoods, function(h) {
            hood_label <- app_data$glyph_scaled %>%
              dplyr::filter(HOOD_158 == h) %>%
              dplyr::distinct(neighbourhood_name) %>%
              dplyr::pull(neighbourhood_name) %>%
              .[[1]]

            column(
              col_width,
              h6(hood_label),
              plotlyOutput(session$ns(paste0("glyph_", h)), height = plot_height)
            )
          })
        )
      }
    })

    output$glyph_overlay <- renderPlotly({
      req(app_state$active_year)
      compare_hoods <- unique(c(app_state$detail_hood, app_state$compare_hoods))
      compare_hoods <- compare_hoods[!is.na(compare_hoods) & nzchar(compare_hoods)]
      req(length(compare_hoods) > 0)

      glyph_df <- app_data$glyph_scaled %>%
        dplyr::filter(
          HOOD_158 %in% compare_hoods,
          year == app_state$active_year
        )
      avg_df <- app_data$glyph_avg %>%
        dplyr::filter(year == app_state$active_year)

      req(nrow(glyph_df) > 0, nrow(avg_df) > 0)

      theta <- c("Auto Theft", "Income", "Unemployment", "Low Income", "Housing Cost")
      colours <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")

      p <- plotly::plot_ly() %>%
        plotly::add_trace(
          type = "scatterpolar",
          mode = "lines",
          r = c(
            avg_df$s_theft[[1]],
            avg_df$s_income[[1]],
            avg_df$s_unemployment[[1]],
            avg_df$s_low_income[[1]],
            avg_df$s_housing[[1]],
            avg_df$s_theft[[1]]
          ),
          theta = c(theta, theta[[1]]),
          fill = "toself",
          name = "City average",
          line = list(color = "#7f7f7f", width = 2),
          fillcolor = "rgba(127,127,127,0.10)",
          hovertemplate = "%{theta}: %{r:.2f}<extra>City average</extra>"
        )

      for (i in seq_len(nrow(glyph_df))) {
        hood_row <- glyph_df[i, ]
        p <- p %>%
          plotly::add_trace(
            type = "scatterpolar",
            mode = "lines",
            r = c(
              hood_row$s_theft,
              hood_row$s_income,
              hood_row$s_unemployment,
              hood_row$s_low_income,
              hood_row$s_housing,
              hood_row$s_theft
            ),
            theta = c(theta, theta[[1]]),
            fill = "toself",
            name = hood_row$neighbourhood_name,
            line = list(color = colours[((i - 1) %% length(colours)) + 1], width = 2),
            fillcolor = "rgba(0,0,0,0)",
            hovertemplate = "%{theta}: %{r:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      p %>%
        plotly::layout(
          polar = list(
            radialaxis = list(range = c(0, 1), tickvals = c(0, 0.5, 1)),
            angularaxis = list(direction = "clockwise")
          ),
          showlegend = TRUE,
          margin = list(l = 20, r = 20, t = 20, b = 20)
        )
    })

    observe({
      compare_hoods <- unique(c(app_state$detail_hood, app_state$compare_hoods))
      compare_hoods <- compare_hoods[!is.na(compare_hoods) & nzchar(compare_hoods)]
      req(length(compare_hoods) > 0)

      lapply(compare_hoods, function(hood_id) {
        output[[paste0("glyph_", hood_id)]] <- renderPlotly({
          req(app_state$active_year)
          current_compare_hoods <- unique(c(app_state$detail_hood, app_state$compare_hoods))
          current_compare_hoods <- current_compare_hoods[!is.na(current_compare_hoods) & nzchar(current_compare_hoods)]
          n_hoods <- length(current_compare_hoods)
          side_margin <- switch(
            as.character(n_hoods),
            "1" = list(l = 70, r = 70, t = 45, b = 75),
            "2" = list(l = 75, r = 75, t = 45, b = 80),
            "3" = list(l = 90, r = 90, t = 50, b = 95),
            "4" = list(l = 85, r = 85, t = 50, b = 95),
            list(l = 75, r = 75, t = 45, b = 80)
          )
          angular_font_size <- if (n_hoods >= 4) 9 else if (n_hoods == 3) 10 else 11

          hood_df <- app_data$glyph_scaled %>%
            dplyr::filter(HOOD_158 == hood_id, year == app_state$active_year)
          avg_df <- app_data$glyph_avg %>%
            dplyr::filter(year == app_state$active_year)

          req(nrow(hood_df) > 0, nrow(avg_df) > 0)

          theta <- c("Auto Theft", "Income", "Unemployment", "Low Income", "Housing Cost")

          plotly::plot_ly() %>%
            plotly::add_trace(
              type = "scatterpolar",
              mode = "lines",
              r = c(
                avg_df$s_theft[[1]],
                avg_df$s_income[[1]],
                avg_df$s_unemployment[[1]],
                avg_df$s_low_income[[1]],
                avg_df$s_housing[[1]],
                avg_df$s_theft[[1]]
              ),
              theta = c(theta, theta[[1]]),
              fill = "toself",
              name = "City average",
              line = list(color = "#7f7f7f", width = 2),
              fillcolor = "rgba(127,127,127,0.10)",
              hovertemplate = "%{theta}: %{r:.2f}<extra>City average</extra>",
              showlegend = FALSE
            ) %>%
            plotly::add_trace(
              type = "scatterpolar",
              mode = "lines",
              r = c(
                hood_df$s_theft[[1]],
                hood_df$s_income[[1]],
                hood_df$s_unemployment[[1]],
                hood_df$s_low_income[[1]],
                hood_df$s_housing[[1]],
                hood_df$s_theft[[1]]
              ),
              theta = c(theta, theta[[1]]),
              fill = "toself",
              name = hood_df$neighbourhood_name[[1]],
              line = list(color = "#2c7fb8", width = 2),
              fillcolor = "rgba(44,127,184,0.25)",
              hovertemplate = "%{theta}: %{r:.2f}<extra>%{fullData.name}</extra>",
              showlegend = FALSE
            ) %>%
            plotly::layout(
              polar = list(
                radialaxis = list(range = c(0, 1), tickvals = c(0, 0.5, 1)),
                angularaxis = list(
                  direction = "clockwise",
                  tickfont = list(size = angular_font_size)
                )
              ),
              margin = side_margin
            )
        })
      })
    })

    observeEvent(input$close_compare, {
      setters$set_compare_hoods(character(0))
      setters$set_compare_modal_open(FALSE)
    })
  })
}
