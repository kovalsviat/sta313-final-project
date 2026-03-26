# ============================================================
# app.R
# Owner: Person D (integration lead)
# Responsibility:
#   - Load all data
#   - Define shared app_state (reactiveValues)
#   - Define all setters via make_setters()
#   - Wire all modules together
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(plotly)
library(readr)

# ── Source modules ─────────────────────────────────────────
source("R/mod_filters.R")
source("R/mod_map.R")
source("R/mod_trends.R")
source("R/mod_barchart.R")
source("R/mod_neighbourhood.R")
source("R/mod_compare.R")

# ── Load data ──────────────────────────────────────────────
# TODO: uncomment each line as CSVs are placed in /data
# line_city_year    <- read_csv("data/line_city_year.csv",    show_col_types = FALSE)
# line_neigh_year   <- read_csv("data/line_neigh_year.csv",   show_col_types = FALSE)
# map_data          <- read_csv("data/map_data.csv",          show_col_types = FALSE)
# crime_rate_all    <- read_csv("data/crime_rate_all.csv",    show_col_types = FALSE)
# bar_category_city <- read_csv("data/bar_category_city.csv", show_col_types = FALSE)
# heatmap_neigh     <- read_csv("data/heatmap_neigh.csv",     show_col_types = FALSE)
# glyph_data        <- read_csv("data/glyph_data.csv",        show_col_types = FALSE)
# glyph_scaled      <- read_csv("data/glyph_scaled.csv",      show_col_types = FALSE)
# glyph_avg         <- read_csv("data/glyph_avg.csv",         show_col_types = FALSE)

app_data <- list(
  line_city_year    = NULL,  
  line_neigh_year   = NULL,
  map_data          = NULL,
  crime_rate_all    = NULL,
  bar_category_city = NULL,
  heatmap_neigh     = NULL,
  glyph_data        = NULL,
  glyph_scaled      = NULL,
  glyph_avg         = NULL
)

# ── Setters factory ────────────────────────────────────────
# Person D owns ALL writes to app_state.
# Every other module calls these — never writes to app_state directly.
make_setters <- function(app_state) {
  list(
    # Filters
    set_crime_type         = function(v) { app_state$crime_type         <- v },
    set_year_range         = function(v) { app_state$year_range         <- v },

    # Map interaction
    set_selected_hood      = function(v) { app_state$selected_hood      <- v },
    set_detail_hood        = function(v) { app_state$detail_hood        <- v },

    # Panel / modal visibility
    set_hood_panel_open    = function(v) { app_state$hood_panel_open    <- v },
    set_compare_modal_open = function(v) { app_state$compare_modal_open <- v },

    # Compare selections
    set_compare_hoods      = function(v) { app_state$compare_hoods      <- v },

    # Animation
    set_active_year        = function(v) { app_state$active_year        <- v },
    set_is_playing         = function(v) { app_state$is_playing         <- v }
  )
}

# ── UI ─────────────────────────────────────────────────────
ui <- page_sidebar(
  title  = "Toronto Crime Dashboard",
  theme  = bs_theme(bootswatch = "flatly", version = 5),
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  sidebar = sidebar(
    width = 280,
    mod_filters_ui("filters")
  ),

  navset_card_tab(
    id = "main_tabs",
    nav_panel("Trends",        mod_trends_ui("trends")),
    nav_panel("Map",           mod_map_ui("map")),
    nav_panel("Neighbourhood", mod_neighbourhood_ui("neighbourhood"))
  ),

  card(mod_barchart_ui("barchart")),

  # Compare modal is always mounted; visibility driven by app_state
  mod_compare_ui("compare")
)

# ── Server ─────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Shared state (Person D defines, no one else touches directly) ──
  app_state <- reactiveValues(
    # Filters
    crime_type         = "car_theft",
    year_range         = c(2012, 2025),

    # Map interaction
    selected_hood      = NULL,   # single-click → filters charts
    detail_hood        = NULL,   # double-click → opens hood panel
                                 # NULL = panel closed, value = panel open

    # Panel / modal visibility
    hood_panel_open    = FALSE,
    compare_modal_open = FALSE,

    # Compare
    compare_hoods      = c(),

    # Animation
    active_year        = NULL,
    is_playing         = FALSE
  )

  setters <- make_setters(app_state)

  # ── Wire modules ──────────────────────────────────────────
  mod_filters_server("filters",              app_state, setters, app_data)
  mod_map_server("map",                      app_state, setters, app_data)
  mod_trends_server("trends",                app_state, setters, app_data)
  mod_barchart_server("barchart",            app_state, setters, app_data)
  mod_neighbourhood_server("neighbourhood",  app_state, setters, app_data)
  mod_compare_server("compare",              app_state, setters, app_data)
}

shinyApp(ui, server)
