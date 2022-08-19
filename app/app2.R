### DATA
library(leaflet)
library(tidyverse, quietly = TRUE)
library(lubridate)

UFO <-readRDS("../data/nuforc_events_2011_2022_v2.Rds")
UFO <- UFO %>% rowid_to_column("index")

###

server <- function(input, output) {
  # CODE BELOW: Create a plot output of sightings by shape,
  # For the selected inputs
  selection <- reactive({
    if (!input$country == "World") {
      UFO %>%
        filter(country == input$country,
               (trunc(date_time,'days') >= input$dates[1] &
                  trunc(date_time, 'days') <= input$dates[2]))
    } else {
      UFO %>%
        filter((trunc(date_time,'days') >= input$dates[1] &
                  trunc(date_time,'days') <= input$dates[2]))
    }
    
  })
  
  output$map <- renderLeaflet({
    selection() %>%
      leaflet(options = leafletOptions(preferCanvas = TRUE,
                                       minZoom = 1)) %>%
      addProviderTiles("CartoDB.DarkMatter", options = providerTileOptions(updateWhenIdle = FALSE)) %>%
      addCircleMarkers(
        lng = ~ long,
        lat =  ~ lat,
        popup = ~ paste0(
          date_time,
          '<br>',
          country,
          '<br>',
          '<i>',
          city,
          '</i>',
          '<br>',
          summary
        ),
        clusterOptions = markerClusterOptions(),
        layerId = ~ index
      )
    
  })
  
  output$shapes <- renderPlot({
    ggplot(data = selection(), aes(fct_rev(fct_infreq(shape)))) +
      geom_bar(fill = "#0b110e", color = "white") + labs(
        title = paste("UFO sightings in", input$country),
        subtitle = paste(input$dates, collapse = " to "),
        x = "Shape",
        y = ""
      ) +
      coord_flip() +
      theme_minimal() +
      scale_y_continuous(breaks = ~ round(unique(pretty(., n = 5))))
  })
  
  
  
  # observe click events on the map map
  
  observeEvent(input$map_marker_click, {
    if (!is.null(input$map_marker_click))
      p <- input$map_marker_click
    text <- reactive({
      UFO %>% pull(full_desc) %>% .[p$id]
    })
    url <- reactive({
      UFO %>% pull(event_url) %>% .[p$id]
    })
    output$full_rep <- renderText({
      gsub(pattern = "\\\\n",
           replacement = "<br>",
           x = text())
    })
    output$rep_url <- renderUI({
      tagList(a(url(), href = url()))
    })
  })
  
}


ui <- fluidPage(
  titlePanel("UFO Sightings"),
  theme = shinythemes::shinytheme('darkly'),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose a country:", choices = c("World", sort(
        unique(UFO$country)
      ))),
      dateRangeInput(
        "dates",
        "Choose a date range:",
        start = min(UFO$date_time, na.rm = TRUE),
        end = max(UFO$date_time, na.rm = TRUE)
      ),
      helpText(
        "NUFORC geolocated and time standardized ufo reports for close to a century of data. 80,000 + reports."
      )
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput('map')),
        tabPanel("Plot", plotOutput("shapes"))
      ),
      br(),
      # tagAppendAttributes(textOutput("full_rep"), style="white-space:pre-wrap;"),
      # textOutput("full_rep"),
      # tags$style(type="text/css", "#full_rep {white-space: pre-wrap;}"),
      htmlOutput("full_rep"),
      br(),
      uiOutput("rep_url"),
      br(),
      div(
        p(
          "Original Data from ",
          a("US National UFO Reporting Center", href = "https://nuforc.org/"),
          "extracted and cleaned by ",
          a("planetsig", href = "https://github.com/planetsig/ufo-reports"),
          ".",
          "Additional data wrangling and shiny app by",
          a("myself.", href = "https://emanuele-messori.shinyapps.io/PFolio/")
        )
      )
    )
  )
)


shinyApp(ui, server)