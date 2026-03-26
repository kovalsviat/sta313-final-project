# ============================================================
# R/mod_barchart.R
# Owner: Person A
# Reads:  app_state$crime_type, app_state$selected_year,
#         app_state$selected_hood
# Writes: app_state$crime_type via set_crime_type()
#           (clicking a bar updates the active crime type)
# ============================================================

mod_barchart_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("bar_chart"), height = "250px")
}

mod_barchart_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    # ── Filtered data reactive ──────────────────────────────
    bar_filtered <- reactive({
      req(app_data$bar_data)

      df <- app_data$bar_data %>%
        filter(year == app_state$selected_year)

      # If a neighbourhood is selected, filter to it
      # Otherwise aggregate to city-wide
      if (!is.null(app_state$selected_hood)) {
        df <- df %>% filter(HOOD_158 == app_state$selected_hood)
      } else {
        df <- df %>%
          group_by(CSI_CATEGORY) %>%
          summarise(crime_count = sum(crime_count, na.rm = TRUE), .groups = "drop")
      }

      # Compute proportion after filtering so it always sums to 100%
      df %>%
        mutate(proportion = crime_count / sum(crime_count)) %>%
        arrange(desc(proportion))
    })

    # ── Render bar chart ────────────────────────────────────
    output$bar_chart <- renderPlotly({
      req(bar_filtered())
      df <- bar_filtered()

      # Highlight the currently selected crime type
      bar_colors <- ifelse(
        df$CSI_CATEGORY == app_state$crime_type,
        "#2c7bb6",   # highlighted — matches map blue
        "#a8c8e8"    # default — lighter blue
      )

      plot_ly(
        data        = df,
        x           = ~proportion,
        y           = ~reorder(CSI_CATEGORY, proportion),
        type        = "bar",
        orientation = "h",
        marker      = list(color = bar_colors),
        customdata  = ~CSI_CATEGORY,
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          "Proportion: %{x:.1%}<br>",
          "Count: %{text}<extra></extra>"
        ),
        text        = ~crime_count,
        source      = "bar_chart"
      ) %>%
        layout(
          xaxis = list(
            title      = "Proportion of crimes",
            tickformat = ".0%",
            range      = c(0, max(df$proportion) * 1.1)
          ),
          yaxis  = list(title = ""),
          margin = list(l = 120, r = 20, t = 10, b = 40),
          plot_bgcolor  = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        event_register("plotly_click")
    })

    # ── Clicking a bar → update crime_type filter ───────────
    observeEvent(event_data("plotly_click", source = "bar_chart"), {
      click <- event_data("plotly_click", source = "bar_chart")
      req(click)
      # customdata holds CSI_CATEGORY value
      setters$set_crime_type(click$customdata)
    })
  })
}