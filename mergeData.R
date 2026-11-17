#install.packages(c("sf","tidyverse","tidycensus","tigris","readxl"))

library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
library(readxl)

acsdata <- read_sf("Data/acs_tract_data.shp")
precincts_cleaned <- read_sf("Data/precinct_data.shp")
policysurvey <- read_csv("Data/policy_survey_cleaned.csv")

precincts_cleaned <- left_join(x = precincts_cleaned,
                               y = policysurvey,
                               by = c("state", "year"))

# Merge ACS tract and UF Election Lab precinct data 
precincts_cleaned <- st_make_valid(precincts_cleaned)
precincts_cleaned <- st_transform(precincts_cleaned, 5070)
acsdata <- st_make_valid(acsdata)
acsdata <- st_transform(acsdata, 5070)

acsdata$tract_area <- as.numeric(st_area(acsdata))

st_agr(acsdata) <- c(
  GEOID = "identity",
  year = "identity",
  NAME = "identity",
  median_household_inc = "constant",
  median_age = "constant",
  tract_area = "constant",
  population = "aggregate",
  white_non_hispanic = "aggregate",
  black_non_hispanic = "aggregate",
  asian_non_hispanic = "aggregate",
  hispanic = "aggregate"
)

st_agr(precincts_cleaned) <- "aggregate"
st_agr(precincts_cleaned) <- c(
  row_id = "identity",
  state = "identity",
  year = "identity"
)

# Use st_intersection to get sliver of each tract within each precinct
state_fips_codes <- fips_codes |>
  distinct(state, state_code) |>
  mutate(state = tolower(state))

precincts_cleaned <- left_join(x = precincts_cleaned,
                               y = state_fips_codes,
                               by = "state")

acsdata <- acsdata |>
  mutate(state_code = substr(GEOID, 1, 2))

states_to_intersect <- unique(precincts_cleaned$state_code)
states_to_intersect <- sort(states_to_intersect)
parts_list <- vector(mode = "list", length(states_to_intersect))
names(parts_list) <- states_to_intersect

for (s in states_to_intersect) {
  print(s)
  
  precincts_state <- precincts_cleaned |>
    filter(state_code == s) |>
    select(row_id)
  
  tracts_state <- acsdata |>
    filter(state_code == s)
  
  parts_list[[s]] <- st_intersection(tracts_state, precincts_state)
}

parts <- parts_list |>
  compact() |>
  bind_rows() |>
  st_as_sf()

parts <- parts |>
  mutate(
    part_area = as.numeric(st_area(geometry)),
    part_share_of_tract = part_area / tract_area,
    weight = population * part_share_of_tract
  )

demographics <- st_drop_geometry(parts) |>
  group_by(row_id) |>
  summarize(
    pop_est = sum(weight, na.rm = TRUE),
    pop_white_est = sum(white_non_hispanic * part_share_of_tract, na.rm = TRUE),
    pop_black_est = sum(black_non_hispanic * part_share_of_tract, na.rm = TRUE),
    pop_asian_est = sum(asian_non_hispanic * part_share_of_tract, na.rm = TRUE),
    pop_hispanic_est = sum(hispanic * part_share_of_tract, na.rm = TRUE),
    median_household_inc_est = sum(median_household_inc * weight, na.rm = TRUE),
    median_age_est = sum(median_age * weight, na.rm = TRUE)
  ) |>
  ungroup()

precincts_final <- left_join(x = precincts_cleaned,
                             y = demographics,
                             by = "row_id")

