#install.packages(c("sf","tidyverse","tidycensus","tigris","readxl"))

library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
library(readxl)

acsdata <- read_sf("Data/acs_tract_data.shp")
precincts_cleaned <- read_sf("Data/precinct_data.shp")
policysurvey <- read_csv("Data/policy_survey_cleaned.csv")

policysurvey <- policysurvey |>
  complete(year = full_seq(year, 1)) |>
  select(-c(`...1`, state, all_mail, early_voting)) |>
  mutate(
    mail_excuse_req = ifelse(year > 2019, 0, 1),
    early_voting_excuse_req = ifelse(year > 2018, 0, 1)
  )

precincts_cleaned <- left_join(x = precincts_cleaned,
                               y = policysurvey,
                               by = "year")

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
  prcnctd = "identity",
  contyfp = "identity",
  localty = "identity",
  prcnct = "identity",
  year = "identity",
  prcn__2 = "constant",
  prsdnty = "constant",
  senatyr = "constant",
  gvrnryr = "constant",
  sttsnty = "constant",
  mail_excuse_req = "constant",
  early_voting_excuse_req = "constant"
)

# Use st_intersection to get sliver of each tract within each precinct
result <- map_dfr(unique(precincts_cleaned$year), \(y) {
  st_intersection(
    filter(precincts_cleaned, year == y),
    filter(acsdata, year == y) |> select(-year)
  )
})

result <- result |>
  mutate(
    part_area = as.numeric(st_area(geometry)),
    part_share_of_tract = part_area / tract_area,
    weight = popultn * part_share_of_tract
  )

demographics <- st_drop_geometry(result) |>
  group_by(prcnctd, year) |>
  summarize(
    pop_est = sum(weight, na.rm = TRUE),
    pop_white_est = sum(wht_nn_ * part_share_of_tract, na.rm = TRUE),
    pop_black_est = sum(blck_n_ * part_share_of_tract, na.rm = TRUE),
    pop_asian_est = sum(asn_nn_ * part_share_of_tract, na.rm = TRUE),
    pop_hispanic_est = sum(hispanc * part_share_of_tract, na.rm = TRUE),
    median_household_inc_est = sum(mdn_hs_ * weight, na.rm = TRUE),
    median_age_est = sum(medin_g * weight, na.rm = TRUE)
  ) |>
  ungroup()

precincts_final <- left_join(x = precincts_cleaned,
                             y = demographics,
                             by = c("prcnctd","year"))

