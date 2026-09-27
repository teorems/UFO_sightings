# Shared by app.R and scripts/build_site_data.R. Needs tidyverse and readxl.

# Every extract in data/ is merged. The extracts overlap at their boundaries
# (e.g. 2022 is both in the 2011-2022 bundle and in the standalone 2022
# refresh), so duplicates are dropped by event_url, which identifies a report.
load_nuforc <- function(dir = "data") {
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

  ufo %>%
    mutate(key = str_to_lower(str_squish(country))) %>%
    left_join(spellings, by = "key") %>%
    left_join(fixes, by = "key") %>%
    mutate(country = if_else(key %in% fixes$key, fixed, spelling)) %>%
    select(-key, -spelling, -fixed) %>%
    arrange(date_time) %>%
    rowid_to_column("index")
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
