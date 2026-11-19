#install.packages(c("sf","tidyverse","tidycensus","tigris","readxl"))

library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
library(readxl)

zip_dir <- "Data/precincts"

zip_files <- list.files(path = zip_dir, full.names = TRUE)

extract_root <- "Data/cleaned"

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

precincts <- bind_rows(out)

vote_cols <- grep("^g\\d{2}", names(precincts), value = TRUE, ignore.case = TRUE)

precincts2 <- precincts |>
  mutate(
    across(all_of(vote_cols), as.numeric),
    statesenate2015 = rowSums(across(starts_with("g15sov")), na.rm = TRUE),
    delegates2015 = rowSums(across(starts_with("g15hod")), na.rm = TRUE),
    
    president2016 = rowSums(across(starts_with("g16pre")), na.rm = TRUE),
    
    governor2017 = rowSums(across(starts_with("g17gov")), na.rm = TRUE),
    ltgovernor2017 = rowSums(across(starts_with("g17ltg")), na.rm = TRUE),
    attorneygeneral2017 = rowSums(across(starts_with("g17atg")), na.rm = TRUE),
    delegates2017 = rowSums(across(starts_with("g17hod")), na.rm = TRUE),
    
    senate2018 = rowSums(across(starts_with("g18uss")), na.rm = TRUE),
    
    statesenate2019 = rowSums(across(starts_with("g19sov")), na.rm = TRUE),
    delegates2019 = rowSums(across(starts_with("g19hod")), na.rm = TRUE),
    
    president2020 = rowSums(across(starts_with("g20pre")), na.rm = TRUE),
    senate2020 = rowSums(across(starts_with("g20uss")), na.rm = TRUE),
    
    governor2021 = rowSums(across(starts_with("g21gov")), na.rm = TRUE),
    ltgovernor2021 = rowSums(across(starts_with("g21ltg")), na.rm = TRUE),
    attorneygeneral2021 = rowSums(across(starts_with("g21atg")), na.rm = TRUE)
  ) |>
  select(countyfp, locality, precinct, year, 
         statesenate2015, delegates2015,
         president2016,
         governor2017, ltgovernor2017, attorneygeneral2017, delegates2017, 
         senate2018, 
         statesenate2019, delegates2019,
         president2020, senate2020,
         governor2021, ltgovernor2021, attorneygeneral2021
  )
  
precincts_clean <- precincts2 |>
  group_by(countyfp, locality, precinct, year) |>
  summarize(
    across(
      c(statesenate2015, delegates2015, president2016, governor2017,
        ltgovernor2017, attorneygeneral2017, delegates2017, 
        senate2018, statesenate2019,
        delegates2019, president2020, senate2020, governor2021,
        ltgovernor2021, attorneygeneral2021),
      ~ sum(.x, na.rm = TRUE)
    )
  ) |>
  ungroup() |>
  mutate(
    president = rowSums(across(c(president2016, president2020)), na.rm = TRUE),
    president = ifelse(year %in% c(2016, 2020), president, NA_integer_),
    senate = rowSums(across(c(senate2018, senate2020)), na.rm = TRUE),
    senate = ifelse(year %in% c(2018, 2020), senate, NA_integer_),
    governor = rowSums(across(c(governor2017, governor2021)), na.rm = TRUE),
    governor = ifelse(year %in% c(2017, 2021), governor, NA_integer_),
    ltgovernor = rowSums(across(c(ltgovernor2017, ltgovernor2021)), na.rm = TRUE),
    ltgovernor = ifelse(year %in% c(2017, 2021), ltgovernor, NA_integer_),
    attorneygeneral = rowSums(across(c(attorneygeneral2017, attorneygeneral2021)), na.rm = TRUE),
    attorneygeneral = ifelse(year %in% c(2017, 2021), attorneygeneral, NA_integer_),
    statesenate = rowSums(across(c(statesenate2015, statesenate2019)), na.rm = TRUE),
    statesenate = ifelse(year %in% c(2015, 2019), statesenate, NA_integer_),
    delegate = rowSums(across(c(delegates2015, delegates2017, delegates2019)), na.rm = TRUE),
    delegate = ifelse(year %in% c(2015, 2017, 2019), delegate, NA_integer_),
    
    presidentyear = ifelse(year %in% c(2016, 2020), 1, 0),
    senateyear = ifelse(year %in% c(2018, 2020), 1, 0),
    governoryear = ifelse(year %in% c(2017, 2021), 1, 0),
    statesenateyear = ifelse(year %in% c(2015, 2019), 1, 0)
  ) |>
  group_by(countyfp, locality, precinct) |>
  mutate(
    precinctid = cur_group_id(),
    precinct_area_m2 = as.numeric(st_area(geometry))
  ) |>
  ungroup() |>
  select(precinctid, countyfp, locality, precinct, precinct_area_m2, year, president, senate, 
         governor, ltgovernor, attorneygeneral, statesenate, delegate,
         presidentyear, senateyear, governoryear, statesenateyear)

precincts_clean <- st_cast(precincts_clean, "MULTIPOLYGON")

st_write(precincts_clean, "Data/precinct_data.shp", append = FALSE)
