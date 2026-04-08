# ============================================================
# R/mod_trends.R
# Owner: Person A
# Reads:  app_state$crime_type, app_state$selected_year,
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
      req(app_state$crime_type)
      if (!is.null(app_state$selected_hood)) {
        df <- app_data$line_neigh_year %>%
          dplyr::filter(
            HOOD_158 == app_state$selected_hood,
            CSI_CATEGORY == app_state$crime_type
          )
      } else {
        df <- app_data$line_city_year %>%
          dplyr::filter(
            CSI_CATEGORY == app_state$crime_type
          )
      }
      df
    })
    
    # ── Render line chart ───────────────────────────────────
    output$line_chart <- renderPlotly({
      df <- trends_filtered()
      req(nrow(df) > 0)
      
      if (!is.null(app_state$selected_hood)) {
        hood_label <- df$neighbourhood_name[!is.na(df$neighbourhood_name)][1]
        
        p <- ggplot2::ggplot(
          df,
          ggplot2::aes(
            x = year,
            y = crime_rate,
            text = paste0(
              "Neighbourhood: ", neighbourhood_name,
              "<br>Crime type: ", CSI_CATEGORY,
              "<br>Year: ", year,
              "<br>Crime rate: ", round(crime_rate, 2)
            )
          )
        ) +
          ggplot2::geom_line(linewidth = 1, group = 1) +
          ggplot2::geom_point(size = 2) +
          ggplot2::labs(
            title = paste("Crime Trend in", hood_label),
            x = "Year",
            y = "Crime Rate"
          ) +
          ggplot2::theme_minimal()
      } else {
        p <- ggplot2::ggplot(
          df,
          ggplot2::aes(
            x = year,
            y = crime_count,
            text = paste0(
              "Crime type: ", CSI_CATEGORY,
              "<br>Year: ", year,
              "<br>Crime count: ", crime_count
            )
          )
        ) +
          ggplot2::geom_line(linewidth = 1, group = 1) +
          ggplot2::geom_point(size = 2) +
          ggplot2::labs(
            title = "City-wide Crime Trend",
            x = "Year",
            y = "Crime Count"
          ) +
          ggplot2::theme_minimal()
      }
      
      plotly::ggplotly(p, tooltip = "text")
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
      invalidateLater(800, session)
      
      isolate({
        current <- app_state$active_year %||% 2014
        
        df <- trends_filtered()
        yrs <- sort(unique(df$year))
        req(length(yrs) > 0)
        
        current_index <- match(current, yrs)
        
        if (is.na(current_index)) {
          setters$set_active_year(min(yrs))
        } else if (current_index >= length(yrs)) {
          setters$set_is_playing(FALSE)
        } else {
          setters$set_active_year(yrs[current_index + 1])
        }
      })
    })
  })
}
