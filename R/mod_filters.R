# ============================================================
# R/mod_filters.R
# Owner: Person D
# Reads:  app_data$bar_data (for neighbourhood list)
# Writes: app_state$crime_type      via set_crime_type()
#         app_state$selected_year   via set_selected_year()
#         app_state$selected_hood   via set_selected_hood()
#           (neighbourhood selector syncs with map clicks)
# ============================================================

mod_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # ── Selected neighbourhood display ───────────────────────
    div(class = "filter-selected-label",
      strong("Selected:"),
      uiOutput(ns("selected_label"), inline = TRUE)
    ),

    hr(),

    # ── Crime type selector ──────────────────────────────────
    selectInput(
      inputId  = ns("crime_type"),
      label    = "Crime Type",
      choices  = c(
        "Auto Theft"      = "Auto Theft",
        "Assault"         = "Assault",
        "Break and Enter" = "Break and Enter",
        "Robbery"         = "Robbery",
        "Theft Over"      = "Theft Over"
      ),
      selected = "Auto Theft"
    ),

    # ── Year slider ──────────────────────────────────────────
    sliderInput(
      inputId = ns("selected_year"),
      label   = "Year",
      min     = 2020,
      max     = 2025,
      value   = 2025,
      step    = 1,
      sep     = "",
      ticks   = TRUE
    ),

    # ── Neighbourhood selector ───────────────────────────────
    # Syncs with map clicks — setting this highlights on map too
    selectInput(
      inputId  = ns("selected_hood"),
      label    = "Neighbourhood",
      choices  = c("All" = ""),   # populated in server once data loads
      selected = ""
    ),

    hr(),

    actionButton(ns("reset"), "Reset Filters",
                 class = "btn-outline-secondary btn-sm w-100")
  )
}

mod_filters_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    # ── Populate neighbourhood dropdown from data ────────────
    observe({
      req(app_data$bar_data)
      hoods <- app_data$bar_data %>%
        dplyr::distinct(HOOD_158, neighbourhood_name) %>%
        dplyr::arrange(neighbourhood_name)
      choices <- c("All" = "", setNames(hoods$HOOD_158, hoods$neighbourhood_name))
      updateSelectInput(session, "selected_hood", choices = choices,
                        selected = app_state$selected_hood %||% "")
    })

    # ── Show currently selected neighbourhood name ───────────
    output$selected_label <- renderUI({
      hood <- app_state$selected_hood
      if (is.null(hood) || hood == "") {
        span("All", style = "color: #888; font-size: 0.85em;")
      } else {
        req(app_data$bar_data)
        name <- app_data$bar_data %>%
          dplyr::filter(HOOD_158 == hood) %>%
          dplyr::pull(neighbourhood_name) %>%
          dplyr::first()
        span(name, style = "color: #2c7bb6; font-size: 0.85em;")
      }
    })

    # ── Crime type → update state ────────────────────────────
    observeEvent(input$crime_type, {
      setters$set_crime_type(input$crime_type)
    }, ignoreInit = TRUE)

    # ── Year → update state ──────────────────────────────────
    observeEvent(input$selected_year, {
      setters$set_selected_year(input$selected_year)
    }, ignoreInit = TRUE)

    # ── Neighbourhood dropdown → update state + map ──────────
    observeEvent(input$selected_hood, {
      hood <- if (input$selected_hood == "") NULL else input$selected_hood
      setters$set_selected_hood(hood)
    }, ignoreInit = TRUE)

    # ── Map click → sync dropdown ────────────────────────────
    # When map sets selected_hood, update this dropdown to match
    observeEvent(app_state$selected_hood, {
      updateSelectInput(session, "selected_hood",
                        selected = app_state$selected_hood %||% "")
    }, ignoreInit = TRUE)

    observeEvent(app_state$crime_type, {
      updateSelectInput(session, "crime_type", selected = app_state$crime_type)
    }, ignoreInit = TRUE)

    # ── Reset ────────────────────────────────────────────────
    observeEvent(input$reset, {
      updateSelectInput(session, "crime_type",    selected = "Auto Theft")
      updateSliderInput(session, "selected_year", value    = 2025)
      updateSelectInput(session, "selected_hood", selected = "")
      setters$set_crime_type("Auto Theft")
      setters$set_selected_year(2025)
      setters$set_selected_hood(NULL)
    })
  })
}