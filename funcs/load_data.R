# Shared by app.R and scripts/build_site_data.R. Needs tidyverse and readxl.

# Every extract in data/ is merged. The extracts overlap at their boundaries
# (e.g. 2022 is both in the 2011-2022 bundle and in the standalone 2022
# refresh), so duplicates are dropped by event_url, which identifies a report.
load_nuforc <- function(dir = "data", apply_coordinate_fixes = TRUE) {
  data_cols <- c(
    "date_time", "localisation", "city", "state", "country", "shape",
    "duration", "summary", "posted", "images", "event_url", "full_desc",
    "year", "lat", "long"
  )

  ufo <- list.files(dir, pattern = "\\.Rds$", full.names = TRUE) %>%
    map(readRDS) %>%
    map(~ select(.x, all_of(data_cols))) %>%
    bind_rows() %>%
    distinct(event_url, .keep_all = TRUE) %>%
    # the scraped /webreports/.../S<id>.html pages no longer exist since NUFORC
    # rebuilt its site; the same report id now lives at /sighting/?id=<id>
    mutate(event_url = str_replace(
      event_url, "^.*/S(\\d+)\\.html$", "https://nuforc.org/sighting/?id=\\1"
    ))

  # extracts spell countries differently ("USA" vs "Usa" for the whole
  # 2011-2022 bundle, "France" vs "france"...), which split the country filter;
  # keep each country's most common spelling, capitalising short codes
  spellings <- ufo %>%
    filter(!is.na(country)) %>%
    count(key = str_to_lower(str_squish(country)), spelling = str_squish(country)) %>%
    group_by(key) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(spelling = if_else(nchar(spelling) <= 3, str_to_upper(spelling), spelling)) %>%
    select(key, spelling)

  # variants, typos, regions and non-answers, listed in data/country_names.csv
  # (an empty "to" means unknown)
  fixes <- read_csv(
    file.path(dir, "country_names.csv"),
    comment = "#", col_types = "cc", na = character()
  ) %>%
    transmute(key = str_to_lower(str_squish(from)), fixed = na_if(to, ""))

  ufo <- ufo %>%
    mutate(key = str_to_lower(str_squish(country))) %>%
    left_join(spellings, by = "key") %>%
    left_join(fixes, by = "key") %>%
    mutate(country = if_else(key %in% fixes$key, fixed, spelling)) %>%
    select(-key, -spelling, -fixed)

  ufo <- clean_cities(ufo)

  # reports geocoded far from their country or US state (mostly towns sharing a
  # name with one in another state), re-placed or removed from the map by
  # scripts/fix_coordinates.R
  coords_file <- file.path(dir, "coordinate_fixes.csv")
  if (apply_coordinate_fixes && file.exists(coords_file)) {
    coords <- read_csv(coords_file, col_types = cols(id = "i", lat = "d", long = "d", .default = "c")) %>%
      select(id, fixed_lat = lat, fixed_long = long)
    ufo <- ufo %>%
      mutate(id = as.integer(str_extract(event_url, "\\d+$"))) %>%
      left_join(coords, by = "id") %>%
      mutate(
        fixed = id %in% coords$id,
        lat = if_else(fixed, fixed_lat, lat),
        long = if_else(fixed, fixed_long, long)
      ) %>%
      select(-id, -fixed_lat, -fixed_long, -fixed)
  }

  ufo %>%
    arrange(date_time) %>%
    rowid_to_column("index")
}

# City names: spaces and dangling punctuation trimmed (the scraper left a
# trailing space wherever it cut a note in brackets), placeholders made
# unknown, names typed all in lower case or all in capitals re-capitalised, and
# spellings of one town within a state/country merged under the most common one
# ("St. Louis", "Saint Louis", "st louis").
clean_cities <- function(ufo) {
  city <- ufo$city %>%
    str_squish() %>%
    str_remove("^[\\s,;:)/-]+") %>%
    str_remove("[\\s,;:(/-]+$")
  unbalanced <- !is.na(city) & str_count(city, fixed("(")) != str_count(city, fixed(")"))
  city[unbalanced] <- str_squish(str_remove_all(city[unbalanced], "[()]"))
  placeholder <- "^(unknown|unk|n/?a|none|not sure|not known|various|multiple|anywhere|everywhere|undisclosed|[?.-]+)$"
  city[is.na(city) | city == "" | str_detect(str_to_lower(city), placeholder)] <- NA

  lower <- !is.na(city) & !str_detect(city, "[A-Z]")
  capitals <- !is.na(city) & !str_detect(city, "[a-z]") & str_count(city, "[A-Z]") >= 4
  recase <- lower | capitals
  city[recase] <- str_to_title(city[recase]) %>%
    str_replace_all("\\bMc[a-z]", ~ paste0("Mc", toupper(substring(.x, 3)))) %>%
    str_replace_all("\\bO'[a-z]", ~ paste0("O'", toupper(substring(.x, 3)))) %>%
    str_replace_all("(?<=\\s)(And|Of|The|On|In|At|By|For|De|Del|Da|Du|Des|Di)\\b", tolower)

  city_key <- city %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_remove_all("['.]") %>%
    str_replace_all("[-,]", " ") %>%
    str_replace_all("\\bst\\b", "saint") %>%
    str_replace_all("\\bmt\\b", "mount") %>%
    str_replace_all("\\bft\\b", "fort") %>%
    str_squish()
  spellings <- tibble(country = ufo$country, state = ufo$state, city_key, city) %>%
    filter(!is.na(city)) %>%
    count(country, state, city_key, city) %>%
    group_by(country, state, city_key) %>%
    slice_max(n, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(country, state, city_key, spelling = city)

  ufo %>%
    mutate(city_key = city_key) %>%
    left_join(spellings, by = c("country", "state", "city_key")) %>%
    mutate(city = spelling) %>%
    select(-city_key, -spelling)
}

# GEIPAN's case export (case search page of https://www.cnes-geipan.fr). Dates
# like "--/08/1947" have unknown parts, so they are rewritten as "1947-08",
# which also sorts correctly.
load_geipan <- function(path = "data/export_cas.xlsx") {
  read_excel(path) %>%
    transmute(
      id = .data[["ID Etude de Cas"]],
      # titles end with the date in several spellings (29.5.2018, --.08.1947, 2004)
      place = str_squish(str_remove(
        .data[["Titre du Cas"]], "\\s*(([-\\d]{1,2}\\.){2}[-\\d]{2,4}|[-\\d]{4})$"
      )),
      date = map_chr(
        str_split(.data[["Date d'observation"]], "/"),
        ~ paste(rev(.x[.x != "--"]), collapse = "-")
      ),
      year = as.integer(.data[["Année"]]),
      region = .data[["Région"]],
      class = .data[["Classification"]],
      explanation = .data[["Phénomène"]],
      summary = .data[["Identification"]],
      details = .data[["Détails"]],
      lat = .data[["Latitude"]],
      long = .data[["Longitude"]]
    ) %>%
    arrange(desc(year), desc(date))
}
