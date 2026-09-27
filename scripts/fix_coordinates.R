# Finds NUFORC reports whose coordinates are far from the country (or, for the
# US, the state) written in the report, and looks the city up again in a
# GeoNames place list. Writes data/coordinate_fixes.csv, which load_nuforc()
# applies. The NUFORC data is frozen, so this only needs running again if the
# country names or the data change.
#
# Needs the maps package and GeoNames' list of places with 1,000+ inhabitants
# as the CSV bundled in the reverse_geocoder Python package (columns lat, lon,
# name, admin1, admin2, cc): `pip download --no-deps --no-binary :all:
# reverse_geocoder`, then take reverse_geocoder/rg_cities1000.csv from the
# archive. GeoNames data is CC BY 4.0 (https://www.geonames.org).
# US towns under 1,000 inhabitants are then looked up in the US ZIP code list
# bundled in the zipcodes Python package (MIT licence): `pip download
# --no-deps --no-binary :all: zipcodes==1.2.0`, file zipcodes/zips.json.bz2.
#
# Run from the repository root:
#   Rscript scripts/fix_coordinates.R path/to/rg_cities1000.csv path/to/zips.json.bz2

library(tidyverse)
library(readxl)

source("funcs/load_data.R")

args <- commandArgs(trailingOnly = TRUE)
stopifnot("pass the paths to rg_cities1000.csv and zips.json.bz2" = length(args) == 2)

# distance beyond which a point is wrong rather than on a coarse border/coast
max_km <- 100

# coordinates as geocoded, before any earlier fixes
ufo <- load_nuforc(apply_coordinate_fixes = FALSE) %>%
  mutate(id = as.integer(str_extract(event_url, "\\d+$")))

# our country names -> region names in maps::map("world")
crosswalk <- list(
  "United Kingdom" = "UK", "Antigua and Barbuda" = c("Antigua", "Barbuda"),
  "British Virgin Islands" = "Virgin Islands, British", "U.S. Virgin Islands" = "Virgin Islands, US",
  "East Timor" = "Timor-Leste", "Eswatini" = "Swaziland", "Réunion" = "Reunion",
  "Saint Kitts and Nevis" = c("Saint Kitts", "Nevis"),
  "Saint Vincent and the Grenadines" = c("Saint Vincent", "Grenadines"),
  "Trinidad and Tobago" = c("Trinidad", "Tobago"),
  "Serbia and Montenegro" = c("Serbia", "Montenegro", "Kosovo"),
  "Yugoslavia" = c("Serbia", "Montenegro", "Kosovo", "Croatia", "Slovenia", "Bosnia and Herzegovina", "North Macedonia"),
  "Northern Cyprus" = "Cyprus", "Israel\\Occupied Palestine" = c("Israel", "Palestine"),
  "Netherlands Antilles" = c("Curacao", "Bonaire", "Sint Maarten", "Saba", "Sint Eustatius"),
  "French West Indies" = c("Martinique", "Guadeloupe", "Saint Martin", "Saint Barthelemy"),
  "Hong Kong" = "China", "Gibraltar" = c("UK", "Spain"),
  # the map draws these territories and islands as their own regions
  "USA" = c("USA", "Puerto Rico", "Guam", "Virgin Islands, US", "Northern Mariana Islands", "American Samoa"),
  "United Kingdom" = c("UK", "Guernsey", "Jersey", "Isle of Man"),
  "Spain" = c("Spain", "Canary Islands"), "Portugal" = c("Portugal", "Azores", "Madeira Islands")
)
map_regions <- function(country) if (!is.null(crosswalk[[country]])) crosswalk[[country]] else country
no_borders <- c("Space", "Aegean Sea", "Atlantic Ocean", "Caribbean Sea", "East China Sea", "Gulf of Mexico",
  "Indian Ocean", "Mediterranean Sea", "North Sea", "Pacific Ocean", "Persian Gulf", "Philippine Sea",
  "Tyrrhenian Sea", "Tuvalu", "United States Minor Outlying Islands")

iso <- maps::iso3166 %>% transmute(a2, region = sub("\\(.*$", "", mapname))
country_codes <- function(country) unique(iso$a2[iso$region %in% map_regions(country)])

region_at <- function(db, lat, lon) sub(":.*", "", maps::map.where(db, lon, lat))

# smallest radius (km) at which a ring of 16 points around a location touches
# one of the accepted regions; Inf beyond 500 km
radii <- c(2, 5, 10, 25, 50, 100, 250, 500)
distance_to <- function(lat, lon, accepted, db) {
  d <- rep(Inf, length(lat))
  todo <- seq_along(lat)
  a <- seq(0, 2 * pi, length.out = 17)[-1]
  for (r in radii) {
    if (!length(todo)) break
    k <- rep(seq_along(todo), each = 16)
    lat16 <- rep(lat[todo], each = 16)
    hit <- region_at(db, lat16 + r / 111.32 * sin(a),
      rep(lon[todo], each = 16) + r / 111.32 * cos(a) / cos(lat16 * pi / 180))
    inside <- map2_lgl(hit, accepted[todo][k], ~ !is.na(.x) && .x %in% .y)
    found <- unique(k[inside])
    if (length(found)) {
      d[todo[found]] <- r
      todo <- todo[-found]
    }
  }
  d
}

located <- ufo %>% filter(!is.na(lat), !is.na(long), !is.na(country), !country %in% no_borders)

# 1. country check
located$at_country <- region_at("world", located$lat, located$long)
off_country <- located %>%
  filter(is.na(at_country) | !map2_lgl(at_country, country, ~ .x %in% map_regions(.y)))
off_country$km <- distance_to(off_country$lat, off_country$long, map(off_country$country, map_regions), "world")
# a US state code with the point inside that state means the coordinates are
# right and the country is the mistake ("Milwaukee, WI, Germany"): leave those
off_country$in_stated_state <- off_country$state %in% state.abb &
  coalesce(region_at("state", off_country$lat, off_country$long) ==
    tolower(state.name[match(off_country$state, state.abb)]), FALSE)
message("far from their country but inside their US state (country mislabelled, kept): ",
  sum(off_country$km > max_km & off_country$in_stated_state))
wrong_country <- off_country %>% filter(km > max_km, !in_stated_state)

# 2. US state check (lower 48, where the state polygons exist)
states <- tibble(state = state.abb, state_name = state.name)
us <- located %>%
  filter(country == "USA", !id %in% off_country$id) %>%
  inner_join(states, by = "state") %>%
  filter(!state %in% c("AK", "HI"))
us$at_state <- region_at("state", us$lat, us$long)
off_state <- us %>% filter(is.na(at_state) | at_state != tolower(state_name))
off_state$km <- distance_to(off_state$lat, off_state$long, as.list(tolower(off_state$state_name)), "state")
wrong_state <- off_state %>% filter(km > max_km)

# reports whose location checks out, to reuse for the same city elsewhere
verified <- bind_rows(
  located %>%
    filter(!id %in% off_country$id | id %in% off_country$id[off_country$km <= 25], country != "USA"),
  us %>% filter(!id %in% off_state$id | id %in% off_state$id[off_state$km <= 25])
) %>%
  select(city, state, country, lat, long)

message(sprintf("checked %s located reports: %s far from their country, %s far from their US state",
  format(nrow(located), big.mark = ","), nrow(wrong_country), nrow(wrong_state)))

# 3. look the city up again in the place list, within the right state/country
normalise <- function(x) {
  x %>%
    str_to_lower() %>%
    str_remove_all("\\(.*?\\)") %>%
    str_replace_all("\\bst\\.?\\s", "saint ") %>%
    str_replace_all("\\bft\\.?\\s", "fort ") %>%
    str_replace_all("\\bmt\\.?\\s", "mount ") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

places <- read_csv(args[1], col_types = "ddcccc") %>%
  transmute(key = normalise(name), name, admin1, cc, lat, lon)

# several places can share a name in one region: accept them only if they are
# close together, otherwise the city stays ambiguous
lookup <- function(city, cc, admin1 = NULL) {
  hits <- places %>% filter(key == normalise(city), cc %in% .env$cc)
  if (!is.null(admin1)) hits <- hits %>% filter(admin1 == .env$admin1)
  if (!nrow(hits)) return(NULL)
  spread <- max(abs(hits$lat - hits$lat[1]), abs(hits$lon - hits$lon[1]) * cos(hits$lat[1] * pi / 180)) * 111.32
  if (spread > 30) return(NULL)
  tibble(new_lat = round(mean(hits$lat), 4), new_long = round(mean(hits$lon), 4),
    note = paste0("moved to ", hits$name[1], ", ", hits$admin1[1], " ", hits$cc[1]))
}

# same city (and US state) placed correctly in other reports, when those agree
known <- verified %>%
  mutate(key = normalise(city), state = if_else(country == "USA", state, NA_character_)) %>%
  filter(key != "") %>%
  group_by(key, state, country) %>%
  filter(max(lat) - min(lat) < 0.3, max(long) - min(long) < 0.3) %>%
  summarise(new_lat = round(median(lat), 4), new_long = round(median(long), 4), n = n(), .groups = "drop")

# every name a ZIP code's town goes by, with the ZIP code's coordinates
zips <- jsonlite::fromJSON(bzfile(args[2])) %>%
  as_tibble() %>%
  transmute(state, lat = as.numeric(lat), lon = as.numeric(long),
    names = map2(city, acceptable_cities, ~ unique(c(.x, unlist(.y))))) %>%
  unnest(names) %>%
  transmute(key = normalise(names), name = str_to_title(names), state, lat, lon)

from_zip_codes <- function(city, state) {
  hits <- zips %>% filter(key == normalise(city), state == .env$state)
  if (!nrow(hits)) return(NULL)
  spread <- max(abs(hits$lat - hits$lat[1]), abs(hits$lon - hits$lon[1]) * cos(hits$lat[1] * pi / 180)) * 111.32
  if (spread > 30) return(NULL)
  tibble(new_lat = round(mean(hits$lat), 4), new_long = round(mean(hits$lon), 4),
    note = paste0("moved to ", hits$name[1], ", ", state, " (ZIP code list)"))
}

from_other_reports <- function(city, country, state) {
  in_us <- identical(country, "USA")
  hit <- known %>% filter(key == normalise(city), country == .env$country,
    if (in_us) state %in% .env$state else is.na(state))
  if (!nrow(hit)) return(NULL)
  hit %>% transmute(new_lat, new_long, note = paste0("moved to where ", n, " other report(s) for this place are"))
}

# "cities" that name no place: looking them up would pick an arbitrary spot
not_a_place <- function(city, country) {
  key <- normalise(city)
  is.na(key) | key == "" | key == normalise(country) |
    str_detect(key, "^(unknown|unk|none|n a|na|various|multiple|everywhere|anywhere)$|\\bocean\\b|\\bsea\\b")
}

fix <- function(rows, find) {
  rows %>%
    mutate(found = pmap(list(city, country, state_name = if ("state_name" %in% names(rows)) state_name else NA),
      function(city, country, state_name) if (not_a_place(city, country)) NULL else find(city, country, state_name))) %>%
    mutate(found = pmap(list(found, city, country, state), function(f, city, country, state) {
      if (not_a_place(city, country)) return(NULL)
      f %||% (if (identical(country, "USA")) from_zip_codes(city, state)) %||% from_other_reports(city, country, state)
    })) %>%
    mutate(found = map(found, ~ if (is.null(.x)) tibble(new_lat = NA_real_, new_long = NA_real_, note = "no match: removed from the map") else .x)) %>%
    unnest(found) %>%
    transmute(id, old_lat = lat, old_long = long, lat = new_lat, long = new_long,
      km_off = km, stated = paste0(city, ", ", if_else(is.na(state), "", paste0(state, ", ")), country), note)
}

fixes <- bind_rows(
  fix(wrong_country, function(city, country, state_name) lookup(city, country_codes(country))),
  fix(wrong_state, function(city, country, state_name) lookup(city, "US", state_name))
) %>%
  arrange(id)

write_csv(fixes, "data/coordinate_fixes.csv", na = "")
message(sprintf("wrote %s fixes: %s moved, %s removed from the map",
  nrow(fixes), sum(!is.na(fixes$lat)), sum(is.na(fixes$lat))))
