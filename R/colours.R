# ============================================================
# R/colours.R  —  R MIRROR OF www/styles.css :root VARIABLES
# ============================================================

# ── Brand palette ───────────────────────────────────────────
BRAND <- list(
  darkest  = "#0B2E4A",   # --color-darkest   navbar bg
  sidebar  = "#0D3859",   # --color-sidebar   sidebar bg
  dark     = "#104269",   # --color-dark      hover / active borders
  primary  = "#134E7C",   # --color-primary   card headers, buttons, slider
  light    = "#427196",   # --color-light     secondary text, borders
  lighter  = "#7194B0",   # --color-lighter   muted / disabled
  tint65   = "#ACC1D1",   # --color-tint65    sidebar labels
  tint82   = "#D4DFE7",   # --color-tint82    card / input borders
  tint91   = "#E9EFF3",   # --color-tint91    hover backgrounds
  tint96   = "#F5F7F9"    # --color-tint96    page background
)

# ── Crime type colours ──────────────────────────────────────
# Mirrors --crime-* variables in styles.css.
# Order matches filter button order.
CRIME_COLOURS <- c(
  "Auto Theft"      = "#003d5c",   # --crime-auto-theft
  "Assault"         = "#594e90",   # --crime-assault
  "Break and Enter" = "#bc4c96",   # --crime-break-and-enter
  "Robbery"         = "#ff5f66",   # --crime-robbery
  "Theft Over"      = "#ffa600"    # --crime-theft-over
)

# TRUE = colour is dark enough to need white overlaid text
CRIME_DARK_BG <- c(
  "Auto Theft"      = TRUE,
  "Assault"         = TRUE,
  "Break and Enter" = TRUE,
  "Robbery"         = FALSE,
  "Theft Over"      = FALSE
)
