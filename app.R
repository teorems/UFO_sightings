### LIBRARIES & DATA
library(pacman)
pacman::p_load(leaflet, tidyverse, lubridate, plotly, DT, shinythemes, shinycssloaders)

# load and merge every yearly/decade extract in data/ instead of only the
# latest one, so the app reflects the full history described in the README.
# the extracts overlap at their boundaries (e.g. 2022 appears both in the
# 2011-2022 bundle and in the standalone 2022 refresh), so duplicates are
# dropped by event_url, which uniquely identifies a report.
data_cols <- c(
  "date_time", "localisation", "city", "state", "country", "shape",
  "duration", "summary", "posted", "images", "event_url", "full_desc",
  "year", "lat", "long"
)

UFO <- list.files("data", pattern = "\\.Rds$", full.names = TRUE) %>%
  map(readRDS) %>%
  map(~ select(.x, all_of(data_cols))) %>%
  bind_rows() %>%
  distinct(event_url, .keep_all = TRUE) %>%
  arrange(date_time) %>%
  rowid_to_column("index")

###

# UI ----

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      # CartoDB's dark_all/positron tiles now require a paid API key, so the
      # map uses plain OpenStreetMap tiles with a CSS filter to fake the dark
      # look the app had before; scoped to the tile pane so markers/popups
      # (drawn in separate leaflet panes) are left untouched.
      "#map .leaflet-tile-pane {
        filter: invert(1) hue-rotate(180deg) brightness(0.95) contrast(0.9);
      }"
    ))
  ),
  theme = shinythemes::shinytheme("darkly"),
  titlePanel("UFO Sightings around the world"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose a country:", choices = c("World", sort(
        unique(UFO$country)
      ))),
      dateRangeInput(
        "dates",
        "Choose a date range:",
        # default to the last 2 years so the first render (map + plot + table)
        # stays light; the full history is still one filter widen away, since
        # min/max span the whole dataset.
        start = max(UFO$date_time, na.rm = TRUE) - years(2),
        end = max(UFO$date_time, na.rm = TRUE),
        min = min(UFO$date_time, na.rm = TRUE),
        max = max(UFO$date_time, na.rm = TRUE)
      ),
      helpText(
        "NUFORC geolocated and time standardised ufo reports.",
        div(
          p(
            "Original data from ",
            a("US National UFO Reporting Center.", href = "https://nuforc.org/"),
            "Source and data retrieval scripts on",
            a("GitHub.", href = "https://github.com/teorems/UFO_sightings")
          )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", withSpinner(leafletOutput("map")),
                 br(),
                 htmlOutput("full_rep"),
                 br(),
                 uiOutput("rep_url"),
                 br()),
        tabPanel("Plot", withSpinner(plotlyOutput("shapes"))),
        tabPanel("Table", withSpinner(DT::dataTableOutput("sightings")))
      )
    )
  )
)

server <- function(input, output) {
  selection <- reactive({
    if (!input$country == "World") {
      UFO %>%
        filter(
          country == input$country,
          (trunc(date_time, "days") >= input$dates[1] &
            trunc(date_time, "days") <= input$dates[2])
        )
    } else {
      UFO %>%
        filter((trunc(date_time, "days") >= input$dates[1] &
          trunc(date_time, "days") <= input$dates[2]))
    }
  })

  ## map -----

  output$map <- renderLeaflet({
    selection() %>%
      leaflet(options = leafletOptions(
        preferCanvas = TRUE,
        minZoom = 1
      )) %>%
      addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(updateWhenIdle = FALSE)) %>%
      addCircleMarkers(
        lng = ~long,
        lat = ~lat,
        popup = ~ paste0(
          format(date_time, "%d %b %Y %H:%M"),
          "<br>",
          country,
          "<br>",
          "<i>",
          city,
          "</i>",
          "<br>",
          summary
        ),
        clusterOptions = markerClusterOptions(),
        layerId = ~index
      )
  })

  ## barplot #----

  output$shapes <- renderPlotly({
    p <- selection() %>%
      mutate(shape = replace_na(shape, "Unknown")) %>%
      mutate(shape = fct_rev(fct_infreq(shape))) %>%
      ggplot(aes(shape)) +
      geom_bar(fill = "#3498db", color = NA, width = 0.7) +
      labs(
        title = paste("UFO sightings in", input$country),
        subtitle = paste(format(input$dates, "%d %b %Y"), collapse = " – "),
        x = NULL,
        y = "Sightings"
      ) +
      coord_flip() +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(fill = "#222222", color = NA),
        panel.background = element_rect(fill = "#222222", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#3a3a3a"),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#e9ecef"),
        axis.text = element_text(color = "#e9ecef"),
        plot.title = element_text(color = "#ffffff", face = "bold"),
        plot.subtitle = element_text(color = "#adb5bd")
      ) +
      scale_y_continuous(breaks = ~ round(unique(pretty(., n = 5))))

    ggplotly(p) %>%
      layout(
        paper_bgcolor = "#222222",
        plot_bgcolor = "#222222",
        font = list(color = "#e9ecef")
      )
  })

  ## table ----

  output$sightings <- DT::renderDataTable({
    selection() %>%
      transmute(
        Date = format(date_time, "%d %b %Y %H:%M"),
        City = city,
        State = state,
        Country = country,
        Shape = shape,
        Duration = duration,
        Posted = posted,
        Report = paste0("<a href='", event_url, "' target='_blank'>link</a>")
      )
  },
  escape = -8, rownames = FALSE,
  # bootstrap style picks up the darkly theme; DT's default style draws
  # light rows under darkly's white text
  style = "bootstrap", class = "table-condensed table-striped table-hover",
  options = list(pageLength = 25))


  # observe click events on the map map

  observeEvent(input$map_marker_click, {
    if (!is.null(input$map_marker_click)) {
      p <- input$map_marker_click
    }
    text <- reactive({
      UFO %>%
        pull(full_desc) %>%
        .[p$id]
    })
    url <- reactive({
      UFO %>%
        pull(event_url) %>%
        .[p$id]
    })

    output$full_rep <- renderText({
      gsub(
        pattern = "\\\\n",
        replacement = "<br>",
        x = text()
      )
    })

    output$rep_url <- renderUI({
      tagList(a(url(), href = url()))
    })
  })
}

shinyApp(ui, server)