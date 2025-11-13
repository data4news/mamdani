########################################
## 1. Setup ----
########################################

# install.packages("tidycensus")
# install.packages("tidyverse")
# install.packages("tigris")
# install.packages("sf")

library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)

options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# Set your Census API key (run once with install = TRUE, then restart R)
# census_api_key("YOUR_API_KEY_HERE", install = TRUE)
# After first time, you can just rely on it being stored, or:
# Sys.setenv(CENSUS_API_KEY = "YOUR_API_KEY_HERE")

########################################
## 2. Choose ACS year & variables ----
########################################

# Set to a recent ACS 5-year release that tidycensus supports.
acs_year <- 2022   # change if you want another year

# Race variables (ACS table B02001)
race_vars <- c(
  total_pop = "B02001_001",
  white     = "B02001_002",
  black     = "B02001_003",
  asian     = "B02001_005",
  other     = "B02001_007"
)

# Poverty variables (ACS table B17001)
poverty_vars <- c(
  total_poverty_universe = "B17001_001",  # population for whom poverty status is determined
  below_poverty          = "B17001_002"   # income in past 12 months below poverty level
)

# Median household income (ACS table B19013)
income_vars <- c(
  median_household_income = "B19013_001"
)

all_vars <- c(race_vars, poverty_vars, income_vars)

########################################
## 3. Get ACS data for NY Assembly Districts ----
########################################

ny_assembly_acs <- get_acs(
  geography  = "state legislative district (lower chamber)",  # NY Assembly
  state      = "NY",
  variables  = all_vars,
  year       = acs_year,
  survey     = "acs5",
  geometry   = TRUE,     # need geometry to spatially filter to NYC
  cache_table = TRUE
)

# Look at the structure if you want:
# glimpse(ny_assembly_acs)

########################################
## 4. Make data wide & label variables ----
########################################

# Make a lookup to go from variable code -> nice name
var_lookup <- tibble(
  variable = all_vars,
  var_name = names(all_vars)
)

# Geometry (one row per district)
assembly_geom <- ny_assembly_acs %>%
  select(GEOID, geometry) %>%
  distinct()

# Wide data frame with one row per Assembly district and one column per variable
assembly_wide <- ny_assembly_acs %>%
  st_drop_geometry() %>%
  select(GEOID, NAME, variable, estimate) %>%
  left_join(var_lookup, by = "variable") %>%
  select(GEOID, NAME, var_name, estimate) %>%
  pivot_wider(
    names_from  = var_name,
    values_from = estimate
  )

# Join geometry back on
assembly_wide_sf <- assembly_geom %>%
  left_join(assembly_wide, by = "GEOID") %>%
  st_as_sf()

########################################
## 5. Get NYC counties & spatially filter ----
########################################

# NYC county FIPS codes:
# 005 = Bronx, 047 = Kings, 061 = New York, 081 = Queens, 085 = Richmond
nyc_counties <- counties(state = "NY", cb = TRUE, year = acs_year) %>%
  filter(COUNTYFP %in% c("005", "047", "061", "081", "085"))

# Keep only Assembly districts that intersect NYC
nyc_assembly_sf <- assembly_wide_sf[
  st_intersects(assembly_wide_sf, nyc_counties, sparse = FALSE),
]

########################################
## 6. Calculate percentages & final table ----
########################################

nyc_assembly_sf <- nyc_assembly_sf %>%
  mutate(
    pct_white   = 100 * white / total_pop,
    pct_black   = 100 * black / total_pop,
    pct_asian   = 100 * asian / total_pop,
    pct_other   = 100 * other / total_pop,
    pct_poverty = 100 * below_poverty / total_poverty_universe
  )

# If you want a plain data frame without geometry:
nyc_assembly_df <- nyc_assembly_sf %>%
  st_drop_geometry()

########################################
## 7. Example: view or write out ----
########################################

# View a few columns
nyc_assembly_df %>%
  select(GEOID, NAME,
         total_pop, white, black, asian, other,
         pct_white, pct_black, pct_asian, pct_other,
         below_poverty, pct_poverty,
         median_household_income) %>%
  arrange(GEOID) %>%
  print(n = 20)

# Optionally, write to CSV:
write_csv(nyc_assembly_df, "nyc_assembly_acs_race_poverty_income.csv")

# Optionally, plot an example map (median income):
# library(ggplot2)
# ggplot(nyc_assembly_sf) +
#   geom_sf(aes(fill = median_household_income)) +
#   scale_fill_viridis_c(option = "plasma") +
#   labs(
#     title = paste("Median Household Income by NY Assembly District in NYC, ACS", acs_year, "5-year"),
#     fill  = "Median HH income (USD)"
#   ) +
#   theme_minimal()
