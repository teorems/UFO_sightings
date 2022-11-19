library(tidyverse)
library(rvest)
library(lubridate)

# this is the url which lists all the reportings by date
by_date_url <- "http://www.nuforc.org/webreports/ndxevent.html"

# we first obtain the monthly listings
event_month_urls <- by_date_url %>%
  read_html() %>%
  html_elements("td a") %>%
  html_attr("href") %>%
  paste0("http://www.nuforc.org/webreports/", .)

### dataframe of urls by year - in this way we can limit the information retrieved in a time window (retrieving all the information at once can be very time consuming, it's best to split e.g. by decades).

start_year <- "2022"
end_year <- "2022"
urls_by_year <- data.frame(url = event_month_urls, year = str_extract(event_month_urls, "\\d{4}"))
selection <- urls_by_year %>% filter(between(year, start_year, end_year))

source("funcs/scrape_events.R")

system.time(nuforc_events <- scrape_events(selection))

### cleaning
# regex for years before 1900
# "(?<=/)(\\d+(?=\\s)|\\d+(?=$))" matches the ../../** part in dates

nuforc_events_clean <- nuforc_events %>%
  rename_with(janitor::make_clean_names) %>%
  mutate(city = str_replace_all(city, "\\([\\w\\s////+~{}`@?;:&#*,.-]+\\)", "")) %>%
  mutate(date_time = str_replace_all(date_time, "(?<=/)(\\d+(?=\\s)|\\d+(?=$))", year)) |>
  mutate(
    date_time = mdy_hm(date_time, truncated = 3),
    posted = mdy(posted)
  ) %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  unite("localisation", city, state, country, sep = ", ", na.rm = TRUE, remove = FALSE)


### geocoding

library(tidygeocoder)
nuforc_events_geocoded <- geocode_combine(
  nuforc_events_clean,
  queries = list(list(method = "osm", city = "city", state = "state", country = "country"),
    list(method = "arcgis", address = "localisation")),
    cascade = TRUE
  )


if (length(unique(year(nuforc_events_geocoded$date_time))) > 1) {
  years <- paste(c(year(min(nuforc_events_geocoded$date_time)), year(max(nuforc_events_geocoded$date_time))), collapse = "_")
} else {
  years <- unique(year(nuforc_events_geocoded$date_time))
}

### further cleaning

nuforc_events_geocoded <- nuforc_events_geocoded %>% mutate(shape = if_else(is.na(shape)| shape == "Unknown", "Other", shape))

### saveRDS

saveRDS(nuforc_events_geocoded, file = paste0("data/nuforc_events_", years, ".Rds"))
