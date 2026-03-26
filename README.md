# Toronto Crime Dashboard

## Repo Structure

```
app.R                        ← Person D — entry point, state, wiring
R/
  mod_filters.R              ← Person D — sidebar filters
  mod_map.R                  ← Person B — choropleth, click events
  mod_trends.R               ← Person A — line chart, play animation
  mod_barchart.R             ← Person A — crime category bar chart
  mod_neighbourhood.R        ← Person C — hood panel, glyph, heatmap
  mod_compare.R              ← Person C — compare modal, glyph overlay
data/                        ← CSVs (not committed to git)
www/
  styles.css                 ← shared styles
```

---

## Reactive State Contract

`app_state` is defined **only in app.R** (Person D).  
Every field has exactly **one owner** who writes to it.  
All other modules are **read-only** — they call setters, never write directly.

| Field | Type | Owner | Description |
|---|---|---|---|
| `crime_type` | string | D (filters) | Active crime type |
| `year_range` | int[2] | D (filters) | Selected year range |
| `selected_hood` | string\|NULL | B (map) | Single-click highlight |
| `detail_hood` | string\|NULL | B (map) | Double-click → opens panel |
| `hood_panel_open` | bool | B (map) / C (close) | Panel visibility |
| `compare_modal_open` | bool | C (neighbourhood/compare) | Modal visibility |
| `compare_hoods` | string[] | C (compare) | Hoods in compare modal |
| `active_year` | int\|NULL | A (trends) / C (slider) | Animated year |
| `is_playing` | bool | A (trends) | Animation running |

---

## The Golden Rule

```r
# ✅ Correct — call the setter
setters$set_crime_type("assault")

# ❌ Wrong — never write to app_state directly from a module
app_state$crime_type <- "assault"
```

---

## Module Signature

Every module server follows the same signature:

```r
mod_xxx_server <- function(id, app_state, setters, app_data) {
  moduleServer(id, function(input, output, session) {
    # read app_state freely
    # write only via setters$set_xxx()
  })
}
```

---

## Data Files (place in /data)

| File | Used by |
|---|---|
| `line_city_year.csv` | mod_trends |
| `line_neigh_year.csv` | mod_trends |
| `map_data.csv` | mod_map |
| `crime_rate_all.csv` | mod_map |
| `bar_category_city.csv` | mod_barchart |
| `heatmap_neigh.csv` | mod_neighbourhood |
| `glyph_data.csv` | mod_neighbourhood, mod_compare |
| `glyph_scaled.csv` | mod_neighbourhood, mod_compare |
| `glyph_avg.csv` | mod_neighbourhood, mod_compare |
