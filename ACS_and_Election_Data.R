#install.packages(c("sf","tidyverse","tidycensus","tigris","readxl"))

library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
library(readxl)

years <- 2014:2020

pull_va_tidycensus_data <- function(var_year, variable_code){
  
  #' Creates dataframe of 5-year ACS tract-level data for a single variable 
  #'
  #' @param var_year The year of data you want to pull.
  #' @param variable_code The ACS variable code
  #' @param variable_name The ACS variable name (in readable format)
  #' @returns A dataframe
  
  tidycensus_data <- get_acs(
    geography = "tract",
    variables = c(variable_code),
    state = "51",
    year = var_year,
    survey = "acs5",
    geometry = TRUE
  ) |>
    mutate(year = var_year) |>
    select(-moe)
}

# Median Household Income
mhidata <- map(years, pull_va_tidycensus_data, 
    variable_code = "B19049_001"
)

mhidata <- bind_rows(mhidata) |>
  rename(median_household_inc = estimate) |>
  select(-variable)

# Median Age
agedata <- map(years, pull_va_tidycensus_data, 
                variable_code = "B01002_001"
)

agedata <- bind_rows(agedata) |>
  rename(median_age = estimate) |>
  select(-variable)

# Race Data
racedata <- map(years, pull_va_tidycensus_data, 
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

write_sf(acsdata, "Data/acs_tract_data.shp", append = FALSE)

policysurvey <- read_excel("Data/Policy_Survey_Time_Series_2008_2024_Appended_Labels.xlsx")

policysurvey <- policysurvey |>
  mutate(
    year = as.integer(Year),
    state = tolower(`State Abbreviation`)
  ) |>
  filter(state %in% c("va") & year >= 2014) |>
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

write.csv(policysurvey, "Data/policy_survey_cleaned.csv")

