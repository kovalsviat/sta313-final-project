# ============================================================
# R/mod_trends.R
# Owner: Person A
# Reads:  app_state$crime_type, app_state$selected_year,
#         app_state$selected_hood
# Writes: app_state$selected_year via set_selected_year()
# ============================================================

mod_trends_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("line_chart"), height = "400px")
}

mod_trends_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    # ── Filtered data — ALL years ───────────────────────────
    trends_filtered <- reactive({
      req(app_state$crime_type)

      if (!is.null(app_state$selected_hood)) {
        app_data$line_neigh_year %>%
          dplyr::filter(
            HOOD_158     == app_state$selected_hood,
            CSI_CATEGORY == app_state$crime_type
          )
      } else {
        app_data$line_city_year %>%
          dplyr::filter(
            CSI_CATEGORY == app_state$crime_type
          )
      }
    })

    # ── Render ──────────────────────────────────────────────
    output$line_chart <- renderPlotly({
      df           <- trends_filtered()
      req(nrow(df) > 0)
      message("DEBUG trends: nrow=", nrow(df), " sel_year=", app_state$selected_year)
      sel_year     <- app_state$selected_year
      is_hood_view <- !is.null(app_state$selected_hood)

      line_col <- unname(CRIME_COLOURS[app_state$crime_type] %||% BRAND$primary)

      y_var   <- if (is_hood_view) "crime_rate" else "crime_count"
      y_label <- if (is_hood_view) "Crime rate (per 10k)" else "Crime count"
      title   <- if (is_hood_view) {
        paste0(dplyr::first(df$neighbourhood_name[!is.na(df$neighbourhood_name)]),
               " — ", app_state$crime_type, " trend")
      } else {
        paste0("City-wide ", app_state$crime_type, " trend")
      }

      # Pull plain vectors — no tidy eval inside plot_ly
      x_vals <- df$year
      y_vals <- df[[y_var]]
      tt_vals <- if (is_hood_view) {
        paste0("Year: ",  df$year,
               "<br>Rate: ",  round(df$crime_rate, 2),
               "<br>Count: ", df$crime_count)
      } else {
        paste0("Year: ",  df$year,
               "<br>Count: ", df$crime_count)
      }

      df_sel <- df[df$year == sel_year, ]

      # ── Base trace ─────────────────────────────────────────
      p <- plotly::plot_ly(source = "trends_line") %>%
        plotly::add_trace(
          x          = x_vals,
          y          = y_vals,
          type       = "scatter",
          mode       = "lines+markers",
          text       = tt_vals,
          hoverinfo  = "text",
          line       = list(color = line_col, width = 2),
          marker     = list(
            color = line_col,
            size  = 7,
            line  = list(color = BRAND$darkest, width = 1)
          ),
          showlegend = FALSE
        )

      # ── Selected year highlight ────────────────────────────
      if (nrow(df_sel) > 0) {
        y_sel  <- df_sel[[y_var]]
        tt_sel <- if (is_hood_view) {
          paste0("Year: ",  df_sel$year,
                 "<br>Rate: ",  round(df_sel$crime_rate, 2),
                 "<br>Count: ", df_sel$crime_count)
        } else {
          paste0("Year: ",  df_sel$year,
                 "<br>Count: ", df_sel$crime_count)
        }

        p <- p %>%
          plotly::add_trace(
            x          = df_sel$year,
            y          = y_sel,
            type       = "scatter",
            mode       = "markers",
            text       = tt_sel,
            hoverinfo  = "text",
            marker     = list(
              color  = "#EF9F27",
              size   = 13,
              symbol = "circle",
              line   = list(color = "#854F0B", width = 2)
            ),
            showlegend = FALSE
          )
      }

      # ── Layout + vertical rule + year label ───────────────
      p %>%
        plotly::layout(
          title     = list(text = title, font = list(size = 14)),
          xaxis     = list(
            title      = "Year",
            tickmode   = "linear",
            dtick      = 1,
            tickformat = "d"
          ),
          yaxis     = list(title = y_label),
          hovermode = "closest",
          margin    = list(t = 50),
          shapes = list(
            list(
              type = "line",
              x0   = sel_year, x1 = sel_year,
              y0   = 0,        y1 = 1,
              yref = "paper",
              line = list(color = "#EF9F27", width = 1.5, dash = "dot")
            )
          ),
          annotations = list(
            list(
              x         = sel_year,
              y         = 1,
              yref      = "paper",
              text      = as.character(sel_year),
              showarrow = FALSE,
              font      = list(color = "#854F0B", size = 11),
              bgcolor   = "rgba(250,238,218,0.85)",
              borderpad = 3
            )
          )
        ) %>%
        plotly::event_register("plotly_click")
    })

    # ── Click → set selected_year ───────────────────────────
    observeEvent(
      plotly::event_data("plotly_click", source = "trends_line"), {
        click <- plotly::event_data("plotly_click", source = "trends_line")
        req(!is.null(click))
        setters$set_selected_year(as.integer(round(click$x)))
      }
    )

  })
}