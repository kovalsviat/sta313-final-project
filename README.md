# Toronto Crime Dashboard

## Repo Structure

```
app.R                        ← Person D — entry point, state, wiring, sidebar toggle
R/
  mod_filters.R              ← Person D — sidebar filters
  mod_map.R                  ← Person B — choropleth, click/dblclick events
  mod_trends.R               ← Person A — line chart (no play)
  mod_barchart.R             ← Person A — crime category bar chart
  mod_neighbourhood.R        ← Person C — hood panel, glyph, heatmap, play/pause
  mod_compare.R              ← Person C — compare modal, glyph overlay/side-by-side
data/                        ← CSVs (not committed to git)
www/
  styles.css                 ← shared styles
```

---

## Reactive State Contract

`app_state` is defined **only in app.R** (Person D).
Every field has exactly **one owner** who writes to it.
All other modules are **read-only** — they call setters, never write directly.

| Field | Type | Owner (writer) | Description |
|---|---|---|---|
| `crime_type` | string | D (filters) | Active crime type |
| `selected_year` | int | D (filters) | Single year — drives map, line, bar |
| `selected_hood` | string\|NULL | B (map) | Single-click highlight — persists after panel closes |
| `detail_hood` | string\|NULL | B (map) / C (close) | Double-click — opens neighbourhood panel |
| `hood_panel_open` | bool | B (map) / C (close) | Panel visibility — also triggers sidebar hide/show |
| `compare_modal_open` | bool | C | Compare modal visibility |
| `compare_hoods` | string[] | C | Hoods selected in compare modal |
| `active_year` | int | B (init on dblclick) → C (slider/play) | Glyph year — independent of selected_year once panel opens |
| `is_playing` | bool | C | Glyph animation running |

---

## Key Behaviours

**Map highlight**
- Single-click → `selected_hood` set → polygon highlighted, charts filter
- Double-click → `selected_hood` + `detail_hood` set → panel opens, highlight stays
- Panel close → `detail_hood` cleared, `selected_hood` stays → highlight persists

**Sidebar / panel**
- Sidebar visible by default
- Panel opens → sidebar auto-hides (app.R observer)
- Panel closes → sidebar auto-restores

**active_year vs selected_year**
- `selected_year` = global filter for overview (map, line, bar)
- `active_year` = local to glyph only; initialised from `selected_year` at panel open, then independent
- Changing sidebar year after panel is open does NOT affect `active_year`

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

<<<<<<< Updated upstream
Every module server follows the same signature:
=======
Every module server follows the same four-argument pattern:
>>>>>>> Stashed changes

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
