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

    hood_meta <- reactive({
      req(app_state$detail_hood)

      app_data$glyph_scaled %>%
        dplyr::filter(HOOD_158 == app_state$detail_hood) %>%
        dplyr::distinct(HOOD_158, neighbourhood_name) %>%
        dplyr::slice(1)
    })

    output$hood_panel <- renderUI({ NULL })

    observeEvent(list(app_state$hood_panel_open, app_state$detail_hood, app_state$compare_modal_open), {
      if (isTRUE(app_state$compare_modal_open)) {
        return()
      }

      if (!isTRUE(app_state$hood_panel_open) || is.null(app_state$detail_hood)) {
        removeModal()
        return()
      }

      req(hood_meta())
      ns <- session$ns
      meta <- hood_meta()
      initial_year <- isolate(app_state$active_year)

      removeModal()
      showModal(modalDialog(
        easyClose = FALSE,
        size = "xl",
        footer = NULL,
        tagList(
          tags$script(HTML(
            "$(document)
              .off('shown.bs.modal.hoodpanel shown.bs.tab.hoodpanel')
              .on('shown.bs.modal.hoodpanel shown.bs.tab.hoodpanel', function() {
                setTimeout(function() {
                  $(window).trigger('resize');
                }, 100);
              });
             setTimeout(function() {
               $(window).trigger('resize');
             }, 100);"
          )),
          div(
            class = "hood-panel",
            div(
              style = "display:flex; justify-content:space-between; align-items:flex-start; gap:16px;",
              div(
                h4(style = "margin:0;", meta$neighbourhood_name[[1]]),
                div(style = "font-size:14px; color:#666;", meta$HOOD_158[[1]])
              ),
              actionButton(
                ns("close_panel"),
                icon("times"),
                class = "btn-close-panel"
              )
            ),
            div(
              style = "display:flex; gap:24px; align-items:flex-start; margin-top:16px; width:100%; min-width:0;",
              div(
                style = "flex:0 0 240px; width:240px; padding-top:100px;",
                leafletOutput(ns("hood_map"), height = "190px", width = "100%"),
                actionButton(
                  ns("open_compare"),
                  "Compare",
                  class = "btn-primary btn-sm",
                  style = "width:100%; margin-top:16px;"
                )
              ),
              div(
                style = "flex:1 1 0; min-width:0;",
                tabsetPanel(
                  id = ns("detail_view"),
                  selected = "Overview",
                  type = "tabs",
                  tabPanel(
                    "Overview",
                    div(
                      style = "width:100%; min-width:0; overflow:hidden; margin-top:20px;",
                      plotlyOutput(ns("glyph"), height = "340px", width = "100%")
                    ),
                    div(
                      style = "display:flex; align-items:flex-end; justify-content:center; gap:12px; flex-wrap:wrap; width:100%; min-width:0; margin-top:12px;",
                      div(
                        style = "flex:0 1 360px; min-width:0;",
                        sliderInput(
                          inputId = ns("active_year"),
                          label   = "Year",
                          min     = 2020,
                          max     = 2025,
                          value   = initial_year,
                          step    = 1,
                          sep     = "",
                          ticks   = TRUE
                        )
                      ),
                      div(
                        class = "play-controls",
                        style = "padding-bottom:12px;",
                        actionButton(
                          ns("play"),
                          icon("play"),
                          class = "btn-outline-primary btn-sm"
                        ),
                        actionButton(
                          ns("pause"),
                          icon("pause"),
                          class = "btn-outline-secondary btn-sm"
                        )
                      )
                    )
                  ),
                  tabPanel(
                    "Crimes",
                    plotlyOutput(ns("heatmap"), height = "420px", width = "100%")
                  )
                )
              )
            )
          )
        )
      ))
    }, ignoreInit = TRUE)

    output$hood_map <- renderLeaflet({
      req(app_state$detail_hood)

      hood_sf <- app_data$neighbourhoods_sf %>%
        dplyr::filter(HOOD_158 == app_state$detail_hood)

      req(nrow(hood_sf) > 0)
      bbox <- sf::st_bbox(hood_sf)

      leaflet::leaflet(hood_sf) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        ) %>%
        leaflet::addPolygons(
          fillColor = "#6baed6",
          fillOpacity = 0.65,
          color = "#2c7fb8",
          weight = 2
        )
    })

    output$glyph <- renderPlotly({
      req(app_state$detail_hood)
      req(app_state$active_year)

      detail_id <- as.character(app_state$detail_hood)
      active_year <- as.integer(app_state$active_year)

      hood_df <- app_data$glyph_scaled %>%
        dplyr::mutate(
          HOOD_158 = as.character(HOOD_158),
          year = as.integer(year)
        ) %>%
        dplyr::filter(
          HOOD_158 == detail_id,
          year == active_year
        )

      avg_df <- app_data$glyph_avg %>%
        dplyr::mutate(year = as.integer(year)) %>%
        dplyr::filter(year == active_year)

      message(
        "glyph debug | detail_hood=", detail_id,
        " active_year=", active_year,
        " hood_rows=", nrow(hood_df),
        " avg_rows=", nrow(avg_df)
      )

      validate(
        need(nrow(hood_df) > 0,
            paste("No glyph data for HOOD_158 =", detail_id, "year =", active_year)),
        need(nrow(avg_df) > 0,
            paste("No city average glyph data for year =", active_year))
      )

      theta <- c("Auto Theft", "Income", "Unemployment", "Low Income", "Housing Cost")
      hood_r <- c(
        hood_df$s_theft[[1]],
        hood_df$s_income[[1]],
        hood_df$s_unemployment[[1]],
        hood_df$s_low_income[[1]],
        hood_df$s_housing[[1]]
      )
      avg_r <- c(
        avg_df$s_theft[[1]],
        avg_df$s_income[[1]],
        avg_df$s_unemployment[[1]],
        avg_df$s_low_income[[1]],
        avg_df$s_housing[[1]]
      )

      plotly::plot_ly() %>%
        plotly::add_trace(
          type = "scatterpolar",
          mode = "lines",
          r = c(avg_r, avg_r[[1]]),
          theta = c(theta, theta[[1]]),
          fill = "toself",
          name = "City average",
          line = list(color = "#e78ac3", width = 2),
          fillcolor = "rgba(231,138,195,0.18)"
        ) %>%
        plotly::add_trace(
          type = "scatterpolar",
          mode = "lines",
          r = c(hood_r, hood_r[[1]]),
          theta = c(theta, theta[[1]]),
          fill = "toself",
          name = hood_df$neighbourhood_name[[1]],
          line = list(color = "#2c7fb8", width = 2),
          fillcolor = "rgba(44,127,184,0.28)"
        ) %>%
        plotly::layout(
          polar = list(
            radialaxis = list(range = c(0, 1), tickvals = c(0, 0.5, 1)),
            angularaxis = list(direction = "clockwise")
          ),
          showlegend = TRUE,
          margin = list(l = 20, r = 20, t = 20, b = 20)
        )
    })

    output$heatmap <- renderPlotly({
      req(app_state$detail_hood)
      req(app_state$crime_type)

      heat_df <- app_data$heatmap_neigh %>%
        dplyr::filter(
          HOOD_158 == app_state$detail_hood,
          CSI_CATEGORY == app_state$crime_type
        ) %>%
        dplyr::mutate(month = factor(month, levels = month.name, ordered = TRUE))

      req(nrow(heat_df) > 0)

      plotly::plot_ly(
        data = heat_df,
        x = ~year,
        y = ~month,
        z = ~crime_count,
        type = "heatmap",
        colorscale = "Reds",
        hovertemplate = paste0(
          "<b>", app_state$crime_type, "</b><br>",
          "Year: %{x}<br>",
          "Month: %{y}<br>",
          "Count: %{z}<extra></extra>"
        )
      ) %>%
        plotly::layout(
          xaxis = list(title = "Year", dtick = 1),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = rev(month.name)
          ),
          margin = list(l = 80, r = 20, t = 20, b = 40)
        )
    })

    observeEvent(input$active_year, {
      setters$set_active_year(input$active_year)
    }, ignoreInit = TRUE)

    observeEvent(input$play, {
      setters$set_is_playing(TRUE)
    })

    observeEvent(input$pause, {
      setters$set_is_playing(FALSE)
    })

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

    observeEvent(input$open_compare, {
      setters$set_compare_modal_open(TRUE)
    })

    observeEvent(input$close_panel, {
      setters$set_is_playing(FALSE)
      setters$set_detail_hood(NULL)
      setters$set_hood_panel_open(FALSE)
    })
  })
}
