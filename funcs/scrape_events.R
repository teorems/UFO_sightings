library(purrr)
library(furrr)

plan(multisession(workers = 3))

scrape_events <- function(selection) {
  future_map2_dfr(selection$url, selection$year, function(url, year) {
    url %>%
      read_html() %>%
      html_element("table") %>%
      html_table() %>%
      select(1:9) %>%
      mutate(
        event_url = url %>%
          read_html() %>%
          html_elements("td a") %>%
          html_attr("href") %>%
          paste0("http://www.nuforc.org/webreports/", .)
      ) |>
      mutate(full_desc = future_map(event_url, possibly(~ read_html(., encoding = "Windows-1252"), NA)) %>%
               future_map(possibly(~ html_element(., "tr:nth-child(2) td"),NA)) %>%
               future_map_chr(possibly(~ html_text(.),NA))) |>
      mutate(year = year)
  }, .progress = TRUE)
}