#install.packages(c("sf","tidyverse","tidycensus","tigris","readxl"))

library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
library(readxl)

zip_dirs <- c(
  "Data/precinct2016",
  "Data/precinct2018",
  "Data/precinct2020"
)

extract_root <- "Data/cleaned"

states <- tolower(c(state.abb, "dc"))
years <- c(2016, 2018, 2020)

zip_pattern <- paste0(
  "^(", paste(states, collapse = "|"), ")_",
  "(", paste(years, collapse = "|"), ")\\.zip$"
)

zip_files <- list.files(path = zip_dirs, pattern = zip_pattern, full.names = TRUE)

for (f in zip_files) {
  unzip(f, exdir = extract_root)
}

shape_files <- list.files(path = extract_root, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

out <- list()
for (f in shape_files) {
  x <- st_read(f)
  x <- st_zm(x, drop = TRUE, what = "ZM")
  st_geometry(x) <- st_set_precision(st_geometry(x), 1e7)
  x <- st_make_valid(x)
  x <- x[st_geometry_type(x) %in% c("POLYGON","MULTIPOLYGON"), ]
  x <- st_cast(x, "MULTIPOLYGON")
  x <- st_transform(x, 5070)
  g <- st_geometry(x)
  d <- st_drop_geometry(x)
  d[] <- lapply(d, as.character)
  names(d) <- tolower(names(d))
  d$state <- substr(basename(f), 1, 2)
  d$year <- as.integer(substr(basename(f), 4, 7))
  out[[f]] <- st_sf(d, geometry = g)
}

geom <- votes <- list()
n <- 0
for (f in names(out)) {
  d <- st_drop_geometry(out[[f]])
  yr <- d$year[1]
  rid <- n + seq_len(nrow(d))
  n <- n + nrow(d)
  
  geom[[f]] <- st_sf(data.frame(row_id = rid, state = d$state, year = d$year), geometry = st_geometry(out[[f]]))
  
  vote_cols <- grep("^g\\d{2}", names(d), value = TRUE)
  votes[[f]] <- pivot_longer(cbind(row_id = rid, d[vote_cols]), -row_id, names_to = "k", "values_to" = "x") |>
    mutate(x = as.numeric(x)) |>
    group_by(row_id, office_code = substr(k, 4, 6), party_code = substr(k, 7, 7)) |>
    summarize(votes = sum(x, na.rm = TRUE)) |>
    ungroup()
}

precincts <- bind_rows(geom)
precincts <- st_as_sf(precincts)

votes_collapsed <- bind_rows(votes)

votes_cleaned <- votes_collapsed |>
  arrange(row_id, office_code, party_code) |>
  mutate(
    col_name = paste0(office_code, "_", party_code)
  ) |>
  select(row_id, col_name, votes) |>
  pivot_wider(names_from = col_name, values_from = votes, values_fill = NA_real_) |>
  mutate(
    pre_total = ifelse(if_all(starts_with("pre"), is.na), NA_real_, rowSums(across(starts_with("pre")), na.rm = TRUE)),
    uss_total = ifelse(if_all(starts_with("uss"), is.na), NA_real_, rowSums(across(starts_with("uss")), na.rm = TRUE)),
    gov_total = ifelse(if_all(starts_with("gov"), is.na), NA_real_, rowSums(across(starts_with("gov")), na.rm = TRUE)),
    hal_total = ifelse(if_all(starts_with("hal"), is.na), NA_real_, rowSums(across(starts_with("hal")), na.rm = TRUE)),
  ) |>
  select(row_id, pre_total, uss_total, gov_total, hal_total, starts_with("pre"), starts_with("uss"), starts_with("gov"), starts_with("hal")) 

precincts_cleaned <- precincts |>
  left_join(votes_cleaned, by = "row_id") 

pull_tidycensus_data <- function(state_fips, var_year, variable_code){
  
  #' Creates dataframe of 5-year ACS tract-level data for a single variable 
  #'
  #' @param var_year The year of data you want to pull.
  #' @param variable_code The ACS variable code
  #' @param variable_name The ACS variable name (in readable format)
  #' @param state_fips The two-digit State FIPS.
  #' @returns A dataframe
  
  tidycensus_data <- get_acs(
    geography = "tract",
    variables = c(variable_code),
    state = state_fips,
    year = var_year,
    survey = "acs5",
    geometry = TRUE
  ) |>
    mutate(year = var_year) |>
    select(-moe)
}

state_fips <- fips_codes |> 
  distinct(state_code) |> 
  filter(as.integer(state_code) <= 56) |>
  pull(state_code)

args <- expand_grid(state_fips = state_fips, year = years)

# Median Household Income
mhidata <- map2(args$state_fips, args$year, pull_tidycensus_data, 
    variable_code = "B19049_001"
)

mhidata <- bind_rows(mhidata) |>
  rename(median_household_inc = estimate) |>
  select(-variable)

# Median Age
agedata <- map2(args$state_fips, args$year, pull_tidycensus_data, 
                variable_code = "B01002_001"
)

agedata <- bind_rows(agedata) |>
  rename(median_age = estimate) |>
  select(-variable)

# Race Data
racedata <- map2(args$state_fips, args$year, pull_tidycensus_data, 
                variable_code = c(population = "B02001_001",
                                  white_non_hispanic = "B03002_003",
                                  black_non_hispanic = "B03002_004",
                                  asian_non_hispanic = "B03002_006",
                                  hispanic = "B03002_012"
                )
)

racedata <- bind_rows(racedata) |>
  pivot_wider(names_from = "variable",
              values_from = "estimate")

tracts <- mhidata |> select(GEOID, year)

mhidata <- st_drop_geometry(mhidata)
agedata <- st_drop_geometry(agedata)
racedata <- st_drop_geometry(racedata)

acsdata <- left_join(x = mhidata, 
                   y = agedata,
                   by = c("GEOID","NAME","year"))

acsdata <- left_join(x = acsdata, 
                   y = racedata,
                   by = c("GEOID","NAME","year"))

acsdata <- left_join(x = tracts,
                     y = acsdata,
                     by = c("GEOID","year"))

policysurvey <- read_excel("Data/Policy_Survey_Time_Series_2008_2024_Appended_Labels.xlsx")

policysurvey <- policysurvey |>
  mutate(
    year = as.integer(Year),
    state = tolower(`State Abbreviation`)
  ) |>
  filter(Year %in% c("2016", "2018", "2020") & !state %in% c("as","gu","mp","pr","vi")) |>
  rename(
    mail_excuse_req = `Excuse Required for Mail Voting`,
    all_mail = `All Vote-By-Mail Used`,
    ipev = `Early Voting Terminology, In-Person Early Voting`,
    ipav = `Early Voting Terminology, In-Person Absentee Voting`,
    ipev_other = `Early Voting Terminology, Other In-Person Pre-Election Voting`,
    ipev_none = `Early Voting Terminology, No In-Person Pre-Election Voting`,
    ipev_na = `Early Voting Terminology, No Response`,
    ipev_excuse_req = `Excuse Required for In-Person Pre-Election Voting`
  ) |>
  mutate(
    mail_excuse_req = ifelse(mail_excuse_req == "No, an excuse is not required", 0, ifelse(mail_excuse_req == "Yes, an excuse is required", 1, NA_integer_)),
    all_mail = ifelse(all_mail == "No", 0, ifelse(all_mail == "Yes", 1, NA_integer_)),
    ipev = ifelse(ipev == "Selected", 1, ifelse(ipev == "Not Selected", 0, NA_integer_)),
    ipav = ifelse(ipav == "Selected", 1, ifelse(ipav == "Not Selected", 0, NA_integer_)),
    ipev_other = ifelse(ipev_other == "Selected", 1, ifelse(ipev_other == "Not Selected", 0, NA_integer_)),
    ipev_none = ifelse(ipev_none == "Selected", 1, ifelse(ipev_none == "Not Selected", 0, NA_integer_)),
    ipev_na = ifelse(ipev_na == "Answered", 1, NA_integer_),
    early_voting = rowSums(pick(ipev, ipav, ipev_other), na.rm = TRUE),
    early_voting = ifelse(early_voting > 0, 1, ifelse(ipev_none == 1, 0, NA_integer_)),
    early_voting_excuse_req = ifelse(ipev_excuse_req == "No" | early_voting == 0, 0, ifelse(ipev_excuse_req == "Yes", 1, NA_integer_))
  ) |>
  select(state, year, mail_excuse_req, all_mail, 
         early_voting, early_voting_excuse_req)

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

