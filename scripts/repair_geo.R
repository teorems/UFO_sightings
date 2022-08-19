library(dplyr)
library(tidygeocoder)
library(leaflet)
library(tidyr)



# repairing wrong geocoding (usually in osm1+ n queries)
repair_geo <- function(file) {
  nuforc_events_date <- readRDS(file)

  nuforc_events_date <- nuforc_events_date %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    unite("localisation", city, state, country, sep = ",", na.rm = TRUE, remove = FALSE)

  possibly_wrong_geo <- nuforc_events_date %>%
    filter(!query == "osm1")

  new_geocodings <- geocode(possibly_wrong_geo, address = localisation, method = "arcgis", unique_only = TRUE)

  nf_data_adjusted_geo <- left_join(nuforc_events_date, new_geocodings, by = c("localisation" = "address"))

  nuforc_events_clean <- nf_data_adjusted_geo %>% mutate(
    lat = if_else(!query == "osm1", lat.y, lat.x),
    long = if_else(!query == "osm1", long.y, long.x), .keep = "unused"
  )

  filename <- paste("nuforc_events", as.character(min(nuforc_events_clean$year)), as.character(max(nuforc_events_clean$year)), "v2.Rds", sep = "_")

  saveRDS(nuforc_events_clean, file = filename, )
}

files <- dir(path = "./data", pattern = ".Rds", full.names = TRUE)
library(furrr)
plan(multisession, workers = 4)
future_walk(files, repair_geo, .progress = TRUE)
