# ============================================================
# R/mod_filters.R
# Owner: Person D
# Reads:  (none — this is the source of filter state)
# Writes: app_state$crime_type      via set_crime_type()
#         app_state$year_range      via set_year_range()
# ============================================================

mod_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId  = ns("crime_type"),
      label    = "Crime Type",
      choices  = c(
        "Car Theft"     = "car_theft",
        "Break & Enter" = "break_enter",
        "Assault"       = "assault",
        "Robbery"       = "robbery",
        "Theft Over"    = "theft_over",
        "Auto Theft"    = "auto_theft",
        "Homicide"      = "homicide"
      ),
      selected = "car_theft"
    ),
    sliderInput(
      inputId = ns("year_range"),
      label   = "Year Range",
      min     = 2012, max = 2025,
      value   = c(2012, 2025),
      step    = 1, sep = ""
    ),
    hr(),
    actionButton(ns("reset"), "Reset Filters",
                 class = "btn-outline-secondary btn-sm w-100")
  )
}

mod_filters_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$crime_type, {
      setters$set_crime_type(input$crime_type)
    }, ignoreInit = TRUE)

    observeEvent(input$year_range, {
      setters$set_year_range(input$year_range)
    }, ignoreInit = TRUE)

    observeEvent(input$reset, {
      updateSelectInput(session, "crime_type", selected = "car_theft")
      updateSliderInput(session, "year_range", value = c(2012, 2025))
      setters$set_crime_type("car_theft")
      setters$set_year_range(c(2012, 2025))
    })
  })
}
