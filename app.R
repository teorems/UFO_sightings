### LIBRARIES & DATA
library(pacman)
pacman::p_load(leaflet, tidyverse, lubridate, plotly, DT, shinythemes)

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
      ""
    ))
  ),
  theme = shinythemes::shinytheme("darkly"),
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
            "Original Data from ",
            a("US National UFO Reporting Center.", href = "https://nuforc.org/"),
            "Data retrieval and shiny app by",
            a("myself.", href = "https://emanuele-messori.shinyapps.io/PFolio/")
          )
        )
      )
    ),
    mainPanel(
      tags$head(
      tags$style(HTML(
        "#sightings {
        font-family:Lucida Console;
        font-size : 9px;
        }"
      ))),
      tabsetPanel(
        tabPanel("Map", leafletOutput("map"),
                 br(),
                 htmlOutput("full_rep"),
                 br(),
                 uiOutput("rep_url"),
                 br()),
        tabPanel("Plot", plotlyOutput("shapes")),
        tabPanel("Table", DT::dataTableOutput("sightings"))
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
      addProviderTiles("CartoDB.DarkMatter", options = providerTileOptions(updateWhenIdle = FALSE)) %>%
      addCircleMarkers(
        lng = ~long,
        lat = ~lat,
        popup = ~ paste0(
          date_time,
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
    ggplotly(
      selection() %>%
        mutate(shape = fct_rev(fct_infreq(shape))) %>%
        ggplot(aes(shape)) +
        geom_bar(fill = "#0b110e", color = "white") +
        labs(
          title = paste("UFO sightings in", input$country),
          subtitle = paste(input$dates, collapse = " to "),
          x = "Shape",
          y = ""
        ) +
        coord_flip() +
        theme_classic() +
        scale_y_continuous(breaks = ~ round(unique(pretty(., n = 5))))
    )
  })

  ## table ----

  output$sightings <- DT::renderDataTable(
    selection() %>% select(-c(index,full_desc, summary))
  )


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