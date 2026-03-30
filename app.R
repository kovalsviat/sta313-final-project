# ============================================================
# app.R
# Owner: Person D (integration lead)
# Responsibility:
#   - Load all data
#   - Define shared app_state (reactiveValues)
#   - Define all setters via make_setters()
#   - Wire all modules together
#   - Hide/show sidebar based on hood_panel_open
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(plotly)
library(readr)
library(sf)
library(stringr)


# ── Source modules ─────────────────────────────────────────
source("R/mod_filters.R")
source("R/mod_map.R")
source("R/mod_trends.R")
source("R/mod_barchart.R")
source("R/mod_neighbourhood.R")
source("R/mod_compare.R")

# ── Load data ──────────────────────────────────────────────
# TODO: uncomment each line as CSVs are placed in /data
line_city_year    <- read_csv("data/line_city_year.csv",    show_col_types = FALSE)
line_neigh_year   <- read_csv("data/line_neigh_year.csv",   show_col_types = FALSE)
map_data          <- read_csv("data/map_data.csv",          show_col_types = FALSE)
crime_rate_all    <- read_csv("data/crime_rate_all.csv",    show_col_types = FALSE)
bar_data          <- read_csv("data/bar_data.csv", show_col_types = FALSE)
heatmap_neigh     <- read_csv("data/heatmap_neigh.csv",     show_col_types = FALSE)
glyph_data        <- read_csv("data/glyph_data.csv",        show_col_types = FALSE)
glyph_scaled      <- read_csv("data/glyph_scaled.csv",      show_col_types = FALSE)
glyph_avg         <- read_csv("data/glyph_avg.csv",         show_col_types = FALSE)
neighbourhood_profile_2021 <- read_csv("data/neighbourhood_profile_2021.csv", show_col_types = FALSE)
glyph_crime_by_year        <- read_csv("data/glyph_crime_by_year.csv",        show_col_types = FALSE)
glyph_avg                  <- read_csv("data/glyph_avg.csv",                  show_col_types = FALSE)
correlation_2021           <- read_csv("data/correlation_2021.csv",           show_col_types = FALSE)
# Load boundaries
neighbourhoods_sf <- st_read("data/Neighbourhoods.geojson") %>%
  mutate(
    HOOD_158 = str_pad(as.character(AREA_SHORT_CODE), width = 3, side = "left", pad = "0"),
    neighbourhood_name = AREA_NAME
  )

app_data <- list(
  line_city_year  = line_city_year,
  line_neigh_year = line_neigh_year,
  map_data        = map_data,
  crime_rate_all  = crime_rate_all,
  bar_data        = bar_data,
  heatmap_neigh   = heatmap_neigh,
  glyph_data      = glyph_data,
  glyph_scaled    = glyph_scaled,
  glyph_avg       = glyph_avg,
  neighbourhoods_sf = neighbourhoods_sf,
  neighbourhood_profile  = neighbourhood_profile_2021,   
  glyph_crime_by_year    = glyph_crime_by_year,          
  glyph_avg              = glyph_avg,
  correlation_2021       = correlation_2021
)

# ── Setters factory ────────────────────────────────────────
# Person D owns ALL writes to app_state.
# Every other module calls these — never writes to app_state directly.
make_setters <- function(app_state) {
  list(
    # Filters
    set_crime_type         = function(v) { app_state$crime_type         <- v },
    set_selected_year      = function(v) { app_state$selected_year      <- v },

    # Map interaction
    set_selected_hood      = function(v) { app_state$selected_hood      <- v },
    set_detail_hood        = function(v) { app_state$detail_hood        <- v },

    # Panel / modal visibility
    set_hood_panel_open    = function(v) { app_state$hood_panel_open    <- v },
    set_compare_modal_open = function(v) { app_state$compare_modal_open <- v },

    # Compare selections
    set_compare_hoods      = function(v) { app_state$compare_hoods      <- v },

    # Glyph animation (Person C owns, neighbourhood panel only)
    set_active_year        = function(v) { app_state$active_year        <- v },
    set_is_playing         = function(v) { app_state$is_playing         <- v }
  )
}

# ── UI ─────────────────────────────────────────────────────
ui <- page_sidebar(
  title  = "Toronto Crime Dashboard",
  theme  = bs_theme(bootswatch = "flatly", version = 5),
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # Sidebar hidden when neighbourhood panel is open
  sidebar = sidebar(
    id    = "main_sidebar",
    width = 280,
    mod_filters_ui("filters")
  ),

# ── Top: Map full width ───────────────────────────────────
  card(
    card_header("Neighbourhoods Map"),
    mod_map_ui("map"),
    full_screen = TRUE
  ),

  # ── Bottom row: Line chart (wide) + Bar chart (narrow) ───
  layout_columns(
    col_widths = c(8, 4),
    card(
      card_header("Crime Trends"),
      mod_trends_ui("trends")
    ),
    card(
      card_header("Crime Type Distribution"),
      mod_barchart_ui("barchart")
    )
  ),

  # Neighbourhood panel — always mounted, shown via hood_panel_open
  mod_neighbourhood_ui("neighbourhood"),

  # Compare modal — always mounted, shown via compare_modal_open
  mod_compare_ui("compare")
)

# ── Server ─────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Shared state ──────────────────────────────────────────
  app_state <- reactiveValues(
    # Filters (overview — drives map, line chart, bar chart)
    crime_type = "Auto Theft",
    selected_year      = 2025,        # single year, not a range

    # Map interaction
    selected_hood      = NULL,        # single-click  → highlight + filter charts
    detail_hood        = NULL,        # double-click  → open neighbourhood panel

    # Panel / modal visibility
    hood_panel_open    = FALSE,
    compare_modal_open = FALSE,

    # Compare
    compare_hoods      = c(),

    # Glyph animation (neighbourhood panel only — owned by Person C)
    # Initialised to selected_year when panel opens; independent after that
    active_year        = 2025,
    is_playing         = FALSE
  )

  setters <- make_setters(app_state)

  # ── Sidebar visibility ─────────────────────────────────────
  # Hide sidebar when neighbourhood panel opens; restore when it closes
  observe({
    if (isTRUE(app_state$hood_panel_open)) {
      bslib::sidebar_toggle("main_sidebar", open = FALSE)
    } else {
      bslib::sidebar_toggle("main_sidebar", open = TRUE)
    }
  })

  # ── Wire modules ──────────────────────────────────────────
  mod_filters_server("filters",             app_state, setters, app_data)
  mod_map_server("map",                     app_state, setters, app_data)
  mod_trends_server("trends",               app_state, setters, app_data)
  mod_barchart_server("barchart",           app_state, setters, app_data)
  mod_neighbourhood_server("neighbourhood", app_state, setters, app_data)
  mod_compare_server("compare",             app_state, setters, app_data)
}

shinyApp(ui, server)