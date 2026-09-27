# Exports the data used by the static site in docs/ (GitHub Pages).
# Run from the repository root: Rscript scripts/build_site_data.R

library(tidyverse)
library(readxl)
library(jsonlite)

source("funcs/load_data.R")

out <- "docs/data"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

write_columns <- function(df, file) {
  write_json(df, file.path(out, file), dataframe = "columns", na = "null", digits = NA)
}

ufo <- load_nuforc()

# one row per report, without text; the summaries are a separate file in the
# same order so the map can draw before they arrive. Full report texts
# (~50 MB compressed) aren't published: each report links to nuforc.org.
ufo %>%
  transmute(
    id = as.integer(str_extract(event_url, "\\d+$")),
    t = format(date_time, "%Y-%m-%d %H:%M"),
    city, state, country, shape, duration,
    lat = round(lat, 3),
    lng = round(long, 3)
  ) %>%
  write_columns("nuforc.json")

write_json(ufo$summary, file.path(out, "nuforc_summaries.json"), na = "null")

load_geipan() %>%
  rename(lng = long) %>%
  write_columns("geipan.json")

walk(list.files(out, full.names = TRUE), ~ message(sprintf("%-28s %6.1f MB", basename(.x), file.size(.x) / 1e6)))
