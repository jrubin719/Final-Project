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

st_write(precincts_cleaned, "Data/precinct_data.shp")