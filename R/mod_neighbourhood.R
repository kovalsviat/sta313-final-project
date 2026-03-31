# ============================================================
# R/mod_neighbourhood.R
# Owner: Person C
# Reads:  app_state$detail_hood, app_state$hood_panel_open,
#         app_state$active_year, app_state$crime_type
# Writes: app_state$hood_panel_open    via set_hood_panel_open()
#         app_state$detail_hood        via set_detail_hood()
#         app_state$compare_modal_open via set_compare_modal_open()
#         app_state$is_playing         via set_is_playing()
#
# Note on active_year:
#   - Initialised by mod_map on double-click (set to selected_year)
#   - Used to render the overview glyph and compare modal
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
      removeModal()
      showModal(modalDialog(
        easyClose = FALSE,
        size = "xl",
        footer = NULL,
        tagList(
          tags$style(HTML(
            ".hood-panel .hood-tabs {
               min-height: 500px;
               display: flex;
               flex-direction: column;
             }
             .hood-panel .hood-tabs .tab-content {
               flex: 1 1 auto;
               min-height: 440px;
             }
             .hood-panel .hood-tabs .tab-pane {
               min-height: 440px;
             }
             .hood-panel .hood-tab-pane {
               min-height: 440px;
               width: 100%;
             }
             .hood-panel .hood-tab-pane--overview {
               display: flex;
               align-items: center;
               justify-content: center;
             }
             .hood-panel .hood-tab-pane--crimes {
               display: flex;
               align-items: flex-start;
               justify-content: center;
               padding-top: 20px;
             }"
          )),
          tags$script(HTML(
            "function resizeHoodModal() {
               var $dialog = $('.modal-dialog:has(.hood-panel)');
               $dialog.css({
                 width: '88vw',
                 'max-width': '1400px',
                 margin: '3vh auto'
               });
               $dialog.find('.modal-content').css({
                 height: '86vh'
               });
               $dialog.find('.modal-body').css({
                 height: 'calc(86vh - 30px)',
                 'overflow-y': 'auto',
                 display: 'flex'
               });
             }
             $(document)
              .off('shown.bs.modal.hoodpanel shown.bs.tab.hoodpanel')
              .on('shown.bs.modal.hoodpanel shown.bs.tab.hoodpanel', function() {
                resizeHoodModal();
                setTimeout(function() {
                  $(window).trigger('resize');
                }, 100);
              });
             setTimeout(function() {
               resizeHoodModal();
               $(window).trigger('resize');
             }, 100);"
          )),
          div(
            class = "hood-panel",
            style = "width:100%; min-height:72vh; display:flex; flex-direction:column;",
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
              style = "display:flex; gap:28px; align-items:center; justify-content:center; margin-top:20px; width:100%; min-width:0; flex-wrap:wrap; flex:1 1 auto; align-content:center;",
              div(
                style = "flex:1 1 340px; width:360px; max-width:420px; min-width:300px; padding-top:32px;",
                leafletOutput(ns("hood_map"), height = "320px", width = "100%"),
                actionButton(
                  ns("open_compare"),
                  "Compare",
                  class = "btn-primary btn-sm",
                  style = "width:100%; margin-top:16px;"
                )
              ),
              div(
                style = "flex:2 1 620px; min-width:420px; display:flex; align-items:center;",
                div(
                  class = "hood-tabs",
                  style = "width:100%;",
                  tabsetPanel(
                    id = ns("detail_view"),
                    selected = "Overview",
                    type = "tabs",
                    tabPanel(
                      "Overview",
                      div(
                        class = "hood-tab-pane hood-tab-pane--overview",
                        plotlyOutput(ns("glyph"), height = "340px", width = "100%")
                      )
                    ),
                    tabPanel(
                      "Crimes",
                      div(
                        class = "hood-tab-pane hood-tab-pane--crimes",
                        plotlyOutput(ns("heatmap"), height = "420px", width = "100%")
                      )
                    )
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
        leaflet::addMapPane("hood_fill", zIndex = 390) %>%
        leaflet::addMapPane("hood_labels", zIndex = 410) %>%
        leaflet::addMapPane("hood_outline", zIndex = 420) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.PositronNoLabels) %>%
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]],
          options = list(
            padding = c(8, 8),
            maxZoom = 14
          )
        ) %>%
        leaflet::addPolygons(
          fillColor = "#6baed6",
          fillOpacity = 0.18,
          color = NA,
          weight = 0,
          options = leaflet::pathOptions(pane = "hood_fill")
        ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.PositronOnlyLabels,
          options = leaflet::providerTileOptions(pane = "hood_labels")
        ) %>%
        leaflet::addPolygons(
          color = "#08519c",
          weight = 3,
          opacity = 1,
          fillOpacity = 0,
          options = leaflet::pathOptions(pane = "hood_outline")
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
