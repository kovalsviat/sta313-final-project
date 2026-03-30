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

neighbourhood_profile_2021 <- profile_wide %>%
  left_join(
    meta_clean %>% select(HOOD_158, TSNS.DESIGNATION),
    by = "HOOD_158"
  ) %>%
  select(HOOD_158, neighbourhood_name, TSNS.DESIGNATION,
         population, income, unemployment, low_income, housing_cost)

glyph_crime_by_year <- crime_clean %>%
  group_by(HOOD_158, OCC_YEAR, CSI_CATEGORY) %>%
  summarise(crime_count = n(), .groups = "drop") %>%
  left_join(meta_clean %>% select(HOOD_158, neighbourhood_name), by = "HOOD_158") %>%
  left_join(profile_wide %>% select(HOOD_158, population), by = "HOOD_158") %>%
  mutate(crime_rate = crime_count / population * 10000) %>%
  group_by(OCC_YEAR, CSI_CATEGORY) %>%
  mutate(
    city_avg_rate = mean(crime_rate, na.rm = TRUE),
    s_crime_rate  = (crime_rate - min(crime_rate, na.rm = TRUE)) /
                    (max(crime_rate, na.rm = TRUE) - min(crime_rate, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  rename(year = OCC_YEAR)

glyph_avg <- glyph_crime_by_year %>%
  group_by(year, CSI_CATEGORY) %>%
  summarise(
    city_avg_rate    = mean(crime_rate,   na.rm = TRUE),
    s_city_avg_rate  = mean(s_crime_rate, na.rm = TRUE),
    .groups = "drop"
  )

library(broom)

crime_2021 <- glyph_crime_by_year %>%
  filter(year == 2021) %>%
  left_join(
    profile_wide %>% select(HOOD_158, income, unemployment, low_income, housing_cost),
    by = "HOOD_158"
  )

crime_types <- unique(crime_2021$CSI_CATEGORY)
soc_vars    <- c("income", "unemployment", "low_income", "housing_cost")

correlation_2021 <- expand_grid(
  crime_type = crime_types,
  soc_var    = soc_vars
) %>%
  pmap_dfr(function(crime_type, soc_var) {
    subset <- crime_2021 %>%
      filter(CSI_CATEGORY == crime_type) %>%
      filter(!is.na(crime_rate), !is.na(.data[[soc_var]]))

    if (nrow(subset) < 10) return(NULL)

    cor.test(subset$crime_rate, subset[[soc_var]], method = "spearman") %>%
      tidy() %>%
      mutate(crime_type = crime_type, soc_var = soc_var)
  }) %>%
  select(crime_type, soc_var, spearman_r = estimate, p_value = p.value) %>%
  mutate(
    significant = p_value < 0.05,
    strong      = abs(spearman_r) > 0.4,
    show_in_ui  = significant & strong
  ) %>%
  arrange(crime_type, desc(abs(spearman_r)))

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
write.csv(neighbourhood_profile_2021, "neighbourhood_profile_2021.csv", row.names = FALSE)
write.csv(glyph_crime_by_year,        "glyph_crime_by_year.csv",        row.names = FALSE)
write.csv(glyph_avg, "glyph_avg.csv", row.names = FALSE)
write.csv(correlation_2021,  "correlation_2021.csv",  row.names = FALSE)
message("✓ All data files written to /data")