library(tidyverse)
library(lubridate)

# ============================================================
# data_prep.R
# Run this script once to generate all CSVs for the dashboard.
# Output files go into the /data folder.
# ============================================================

# ----------- Load raw data ----------------------------------
data1 <- read.csv("neighbourhood-profiles-2021-158-model.csv")
meta  <- read.csv("NBHD metadata.csv")
crime <- read.csv("Major_Crime_Indicators_Open_Data_-4289692410590149445.csv")

# ----------- Clean metadata ---------------------------------
meta_clean <- meta %>%
  mutate(
    HOOD_158 = str_pad(HDNUM, width = 3, side = "left", pad = "0"),
    neighbourhood_name = HDNAME
  ) %>%
  select(HOOD_158, neighbourhood_name, TSNS.DESIGNATION)

# ----------- Clean crime data -------------------------------
# Filter to 2020–2025 early — all downstream datasets inherit this
crime_clean <- crime %>%
  mutate(
    HOOD_158 = str_pad(as.character(HOOD_158), width = 3, side = "left", pad = "0"),
    OCC_MONTH = factor(OCC_MONTH, levels = month.name, ordered = TRUE),
    OCC_DOW = str_trim(OCC_DOW),
    CSI_CATEGORY = str_trim(CSI_CATEGORY)
  ) %>%
  filter(
    !is.na(HOOD_158),
    HOOD_158 != "",
    HOOD_158 != "NSA",
    OCC_YEAR >= 2020,
    OCC_YEAR <= 2025
  )

# Sanity check
crime_clean %>%
  summarise(
    min_year         = min(OCC_YEAR, na.rm = TRUE),
    max_year         = max(OCC_YEAR, na.rm = TRUE),
    n_neighbourhoods = n_distinct(HOOD_158)
  ) %>%
  print()

# ----------- Clean neighbourhood profile data ---------------
profile_long_raw <- data1 %>%
  pivot_longer(
    cols      = -Neighbourhood.Name,
    names_to  = "profile_neighbourhood",
    values_to = "value_raw"
  )

meta_name_key <- meta_clean %>%
  mutate(
    profile_neighbourhood = neighbourhood_name %>%
      str_replace_all("-", ".") %>%
      str_replace_all(" ", ".") %>%
      str_replace_all("`", ".") %>%
      str_replace_all("'", ".") %>%
      str_replace_all("/", ".")
  ) %>%
  select(HOOD_158, neighbourhood_name, profile_neighbourhood)

profile_long <- profile_long_raw %>%
  left_join(meta_name_key, by = "profile_neighbourhood")

# Check for unmatched names
unmatched <- profile_long %>%
  filter(is.na(HOOD_158)) %>%
  distinct(profile_neighbourhood)
print(unmatched)

# Select and clean profile variables
selected_vars <- c(
  "Number of persons in private households",
  "  Median total income of household in 2020 ($)",
  "Unemployment rate",
  "Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)",
  "  Spending 30% or more of income on shelter costs"
)

profile_wide <- profile_long %>%
  filter(Neighbourhood.Name %in% selected_vars) %>%
  mutate(
    value = value_raw %>%
      str_replace_all(",", "") %>%
      na_if("NSA") %>%
      na_if("") %>%
      as.numeric()
  ) %>%
  select(HOOD_158, neighbourhood_name, variable = Neighbourhood.Name, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename(
    population   = `Number of persons in private households`,
    income       = `  Median total income of household in 2020 ($)`,
    unemployment = `Unemployment rate`,
    low_income   = `Prevalence of low income based on the Low-income measure, after tax (LIM-AT) (%)`,
    housing_cost = `  Spending 30% or more of income on shelter costs`
  )

# ============================================================
# TREND DATA
# ============================================================

# City-level yearly trend by crime type
line_city_year <- crime_clean %>%
  group_by(OCC_YEAR, CSI_CATEGORY) %>%
  summarise(crime_count = n(), .groups = "drop") %>%
  rename(year = OCC_YEAR)

# Neighbourhood-level yearly trend by crime type (with crime rate)
line_neigh_year <- crime_clean %>%
  group_by(HOOD_158, OCC_YEAR, CSI_CATEGORY) %>%
  summarise(crime_count = n(), .groups = "drop") %>%
  left_join(meta_clean, by = "HOOD_158") %>%
  left_join(profile_wide %>% select(HOOD_158, population), by = "HOOD_158") %>%
  mutate(crime_rate = crime_count / population * 10000) %>%
  rename(year = OCC_YEAR)

# ============================================================
# MAP DATA
# ============================================================

# Neighbourhood-level yearly crime counts + rates by crime type
map_data <- crime_clean %>%
  group_by(HOOD_158, OCC_YEAR, CSI_CATEGORY) %>%
  summarise(crime_count = n(), .groups = "drop") %>%
  left_join(meta_clean, by = "HOOD_158") %>%
  rename(year = OCC_YEAR)

# Crime rate per 10,000 population (for choropleth colour)
crime_rate_all <- map_data %>%
  left_join(profile_wide %>% select(HOOD_158, population), by = "HOOD_158") %>%
  mutate(crime_rate = crime_count / population * 10000)

# ============================================================
# BAR CHART DATA
# ============================================================
# One table covers both city-wide and neighbourhood views.
# In mod_barchart.R:
#   - No hood selected → aggregate all HOOD_158 for selected year
#   - Hood selected    → filter to that HOOD_158 for selected year
# Always display proportion; show raw count in tooltip.

bar_data <- crime_clean %>%
  group_by(HOOD_158, OCC_YEAR, CSI_CATEGORY) %>%
  summarise(crime_count = n(), .groups = "drop") %>%
  left_join(meta_clean %>% select(HOOD_158, neighbourhood_name), by = "HOOD_158") %>%
  rename(year = OCC_YEAR)
# Note: proportion is computed in the module after filtering,
# so it always sums to 100% within the filtered subset.

# ============================================================
# HEATMAP DATA
# ============================================================
# Monthly × yearly crime count per neighbourhood + crime type.
# No population join needed — user views one neighbourhood at a time,
# so raw count is sufficient to show seasonal patterns.

heatmap_neigh <- crime_clean %>%
  group_by(HOOD_158, OCC_YEAR, OCC_MONTH, CSI_CATEGORY) %>%
  summarise(crime_count = n(), .groups = "drop") %>%
  left_join(meta_clean %>% select(HOOD_158, neighbourhood_name), by = "HOOD_158") %>%
  rename(year = OCC_YEAR, month = OCC_MONTH)

# ============================================================
# GLYPH DATA
# ============================================================
# Star glyph shows auto theft rate + socioeconomic variables per neighbourhood.
# Socioeconomic vars are from 2021 census (static across years).
# Auto theft rate changes per year.
# All variables normalized 0–1 for glyph rendering.

auto_theft_year <- crime_clean %>%
  filter(CSI_CATEGORY == "Auto Theft") %>%
  group_by(HOOD_158, OCC_YEAR) %>%
  summarise(theft_count = n(), .groups = "drop") %>%
  left_join(meta_clean, by = "HOOD_158") %>%
  rename(year = OCC_YEAR)

glyph_data <- auto_theft_year %>%
  left_join(profile_wide, by = c("HOOD_158", "neighbourhood_name")) %>%
  mutate(auto_theft_rate = theft_count / population * 10000)

# Min-max normalize all glyph variables to 0–1
glyph_scaled <- glyph_data %>%
  mutate(
    s_theft        = (auto_theft_rate - min(auto_theft_rate, na.rm = TRUE)) /
                     (max(auto_theft_rate, na.rm = TRUE) - min(auto_theft_rate, na.rm = TRUE)),
    s_income       = (income - min(income, na.rm = TRUE)) /
                     (max(income, na.rm = TRUE) - min(income, na.rm = TRUE)),
    s_unemployment = (unemployment - min(unemployment, na.rm = TRUE)) /
                     (max(unemployment, na.rm = TRUE) - min(unemployment, na.rm = TRUE)),
    s_low_income   = (low_income - min(low_income, na.rm = TRUE)) /
                     (max(low_income, na.rm = TRUE) - min(low_income, na.rm = TRUE)),
    s_housing      = (housing_cost - min(housing_cost, na.rm = TRUE)) /
                     (max(housing_cost, na.rm = TRUE) - min(housing_cost, na.rm = TRUE))
  )

# City average glyph shape per year (used as pink reference in neighbourhood panel)
glyph_avg <- glyph_scaled %>%
  group_by(year) %>%
  summarise(
    s_theft        = mean(s_theft,        na.rm = TRUE),
    s_income       = mean(s_income,       na.rm = TRUE),
    s_unemployment = mean(s_unemployment, na.rm = TRUE),
    s_low_income   = mean(s_low_income,   na.rm = TRUE),
    s_housing      = mean(s_housing,      na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# EXPORT
# ============================================================

write.csv(meta_clean,      "meta_clean.csv",      row.names = FALSE)
write.csv(line_city_year,  "line_city_year.csv",  row.names = FALSE)
write.csv(line_neigh_year, "line_neigh_year.csv", row.names = FALSE)
write.csv(map_data,        "map_data.csv",        row.names = FALSE)
write.csv(crime_rate_all,  "crime_rate_all.csv",  row.names = FALSE)
write.csv(bar_data,        "bar_data.csv",        row.names = FALSE)
write.csv(heatmap_neigh,   "heatmap_neigh.csv",   row.names = FALSE)
write.csv(glyph_data,      "glyph_data.csv",      row.names = FALSE)
write.csv(glyph_scaled,    "glyph_scaled.csv",    row.names = FALSE)
write.csv(glyph_avg,       "glyph_avg.csv",       row.names = FALSE)

message("✓ All data files written to /data")