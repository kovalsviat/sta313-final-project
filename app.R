# ============================================================
# app.R
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
source("R/colours.R")
source("R/mod_filters.R")
source("R/mod_map.R")
source("R/mod_trends.R")
source("R/mod_barchart.R")
source("R/mod_neighbourhood.R")
source("R/mod_compare.R")

# ── Load data ──────────────────────────────────────────────
line_city_year        <- read_csv("data/line_city_year.csv",             show_col_types = FALSE)
line_neigh_year       <- read_csv("data/line_neigh_year.csv",            show_col_types = FALSE)
map_data              <- read_csv("data/map_data.csv",                   show_col_types = FALSE)
crime_rate_all        <- read_csv("data/crime_rate_all.csv",             show_col_types = FALSE)
bar_data              <- read_csv("data/bar_data.csv",                   show_col_types = FALSE)
heatmap_neigh         <- read_csv("data/heatmap_neigh.csv",              show_col_types = FALSE)
glyph_crime_by_year   <- read_csv("data/glyph_crime_by_year.csv",        show_col_types = FALSE)
glyph_avg             <- read_csv("data/glyph_avg.csv",                  show_col_types = FALSE)
neighbourhood_profile <- read_csv("data/neighbourhood_profile_2021.csv", show_col_types = FALSE)
correlation_2021      <- read_csv("data/correlation_2021.csv",           show_col_types = FALSE)
neighbourhoods_sf <- st_read("data/Neighbourhoods.geojson") %>%
  mutate(
    HOOD_158 = str_pad(as.character(AREA_SHORT_CODE), width = 3, side = "left", pad = "0"),
    neighbourhood_name = AREA_NAME
  )

app_data <- list(
  line_city_year        = line_city_year,
  line_neigh_year       = line_neigh_year,
  map_data              = map_data,
  crime_rate_all        = crime_rate_all,
  bar_data              = bar_data,
  heatmap_neigh         = heatmap_neigh,
  glyph_crime_by_year   = glyph_crime_by_year,
  glyph_avg             = glyph_avg,
  neighbourhood_profile = neighbourhood_profile,
  correlation_2021      = correlation_2021,
  neighbourhoods_sf     = neighbourhoods_sf
)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Setters factory ────────────────────────────────────────
make_setters <- function(app_state) {
  list(
    set_crime_type         = function(v) { app_state$crime_type         <- v },
    set_selected_year      = function(v) { app_state$selected_year      <- v },
    set_selected_hood      = function(v) { app_state$selected_hood      <- v },
    set_detail_hood        = function(v) { app_state$detail_hood        <- v },
    set_hood_panel_open    = function(v) { app_state$hood_panel_open    <- v },
    set_compare_modal_open = function(v) { app_state$compare_modal_open <- v },
    set_compare_hoods      = function(v) { app_state$compare_hoods      <- v },
    set_active_year        = function(v) { app_state$active_year        <- v },
    set_is_playing         = function(v) { app_state$is_playing         <- v }
  )
}

# ── UI ─────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Toronto Crime Dashboard",
  theme = bs_theme(
    version   = 5,
    primary   = BRAND$primary,
    secondary = BRAND$light,
    bg        = BRAND$tint96,
    fg        = "#1A2A38"
  ),
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  sidebar = sidebar(
    id    = "main_sidebar",
    width = 280,
    mod_filters_ui("filters")
  ),

  # ── Map ───────────────────────────────────────────────────
  card(
    card_header(
      div(style = "display:flex; align-items:center; gap:8px;",
        span("Neighbourhoods Map"),
        div(style = "position:relative; display:inline-block;",
          tags$span("?",
            onclick = "toggleMapHint()",
            style = "display:inline-flex; align-items:center; justify-content:center;
                     width:15px; height:15px; border-radius:50%;
                     background:#e8eaed; color:#555; font-size:9px;
                     font-weight:700; cursor:pointer; line-height:1;"
          ),
          tags$div(id = "map_hint",
            style = "display:none; position:absolute; left:20px; top:0;
                     width:230px; background:#1a2a38; color:#e9eff3;
                     font-size:11px; line-height:1.55; border-radius:6px;
                     padding:9px 11px; z-index:9999;
                     box-shadow:0 2px 8px rgba(0,0,0,0.3);",
            tags$b("Single click"), " a neighbourhood to select it and filter the charts below.",
            tags$br(), tags$br(),
            tags$b("Double click"), " to open the full neighbourhood dashboard with crime trends and socioeconomic profile."
          )
        )
      )
    ),
    tags$script(HTML(
      "function toggleMapHint() {
         var el = document.getElementById('map_hint');
         if (!el) return;
         el.style.display = el.style.display === 'block' ? 'none' : 'block';
       }"
    )),
    mod_map_ui("map"),
    full_screen = TRUE
  ),

  # ── Bottom row ────────────────────────────────────────────
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

  mod_neighbourhood_ui("neighbourhood"),
  mod_compare_ui("compare")
)

# ── Server ─────────────────────────────────────────────────
server <- function(input, output, session) {

  app_state <- reactiveValues(
    crime_type         = "Auto Theft",
    selected_year      = 2025,
    selected_hood      = NULL,
    detail_hood        = NULL,
    hood_panel_open    = FALSE,
    compare_modal_open = FALSE,
    compare_hoods      = c(),
    active_year        = 2025,
    is_playing         = FALSE
  )

  setters <- make_setters(app_state)

  observe({
    if (isTRUE(app_state$hood_panel_open)) {
      bslib::sidebar_toggle("main_sidebar", open = FALSE)
    } else {
      bslib::sidebar_toggle("main_sidebar", open = TRUE)
    }
  })

  mod_filters_server("filters",             app_state, setters, app_data)
  mod_map_server("map",                     app_state, setters, app_data)
  mod_trends_server("trends",               app_state, setters, app_data)
  mod_barchart_server("barchart",           app_state, setters, app_data)
  mod_neighbourhood_server("neighbourhood", app_state, setters, app_data)
  mod_compare_server("compare",             app_state, setters, app_data)
}

shinyApp(ui, server)