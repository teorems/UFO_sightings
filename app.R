### LIBRARIES & DATA
library(pacman)
pacman::p_load(leaflet, tidyverse, lubridate, plotly, DT, shinythemes, shinycssloaders, readxl)

source("funcs/load_data.R")
UFO <- load_nuforc()
GEIPAN <- load_geipan()

geipan_classes <- c(
  A = "A: identified",
  B = "B: probably identified",
  C = "C: not enough information",
  D = "D: unexplained after investigation"
)
# one-hue ramp, dimmest for identified cases and brightest for unexplained
# ones; validated as an ordinal ramp against the #222222 background
geipan_colors <- c(A = "#1c5cab", B = "#3987e5", C = "#86b6ef", D = "#cde2fb")

theme_app <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#222222", color = NA),
      panel.background = element_rect(fill = "#222222", color = NA),
      panel.grid.major = element_line(color = "#3a3a3a"),
      panel.grid.minor = element_blank(),
      text = element_text(color = "#e9ecef"),
      axis.text = element_text(color = "#e9ecef"),
      plot.title = element_text(color = "#ffffff", face = "bold"),
      plot.subtitle = element_text(color = "#adb5bd")
    )
}

dark_plotly <- function(p) {
  ggplotly(p) %>%
    layout(
      paper_bgcolor = "#222222",
      plot_bgcolor = "#222222",
      font = list(color = "#e9ecef")
    )
}

###

# UI ----

ui <- navbarPage(
  title = "UFO Sightings",
  theme = shinythemes::shinytheme("darkly"),
  header = tags$head(
    tags$style(HTML(
      # CartoDB's dark_all/positron tiles now require a paid API key, so the
      # maps use plain OpenStreetMap tiles with a CSS filter to fake the dark
      # look the app had before; scoped to the tile pane so markers/popups
      # (drawn in separate leaflet panes) are left untouched.
      "#map .leaflet-tile-pane, #geipan_map .leaflet-tile-pane {
        filter: invert(1) hue-rotate(180deg) brightness(0.95) contrast(0.9);
      }"
    ))
  ),
  tabPanel(
    "Worldwide (NUFORC)",
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
  ),
  tabPanel(
    "France (GEIPAN)",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "geipan_years", "Years:",
          min = min(GEIPAN$year), max = max(GEIPAN$year),
          value = range(GEIPAN$year), step = 1, sep = ""
        ),
        checkboxGroupInput(
          "geipan_class", "Classification:",
          choiceNames = unname(geipan_classes),
          choiceValues = names(geipan_classes),
          selected = names(geipan_classes)
        ),
        selectInput(
          "geipan_region", "Region:",
          choices = c("All", sort(unique(na.omit(GEIPAN$region))))
        ),
        helpText(
          p(
            "Cases investigated by GEIPAN, the unit of the French space agency",
            "(CNES) that studies unidentified aerospace phenomena. After",
            "investigation each case is classified from A (identified) to D",
            "(still unexplained)."
          ),
          p(
            "Case descriptions are in French. GEIPAN rounds locations to 0.1°",
            "to protect witnesses."
          ),
          p("Data from ", a("GEIPAN / CNES.", href = "https://www.cnes-geipan.fr", target = "_blank"))
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Map", withSpinner(leafletOutput("geipan_map")),
                   br(),
                   uiOutput("geipan_case")),
          tabPanel("Plots",
                   withSpinner(plotlyOutput("geipan_years_plot")),
                   br(),
                   withSpinner(plotlyOutput("geipan_explanations", height = "500px"))),
          tabPanel("Table", withSpinner(DT::dataTableOutput("geipan_table")))
        )
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
      theme_app() +
      theme(panel.grid.major.y = element_blank()) +
      scale_y_continuous(breaks = ~ round(unique(pretty(., n = 5))))

    dark_plotly(p)
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
  escape = -8, rownames = FALSE, selection = "none",
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
      a("Open the original report on NUFORC", href = url(), target = "_blank")
    })
  })

  ## GEIPAN ----

  geipan_selection <- reactive({
    GEIPAN %>%
      filter(
        between(year, input$geipan_years[1], input$geipan_years[2]),
        class %in% input$geipan_class,
        input$geipan_region == "All" | region == input$geipan_region
      )
  })

  output$geipan_map <- renderLeaflet({
    pal <- colorFactor(unname(geipan_colors), levels = names(geipan_colors))
    geipan_selection() %>%
      leaflet(options = leafletOptions(preferCanvas = TRUE, minZoom = 1)) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      # centred on mainland France; overseas cases are there when zooming out
      setView(lng = 2.5, lat = 46.6, zoom = 5) %>%
      addCircleMarkers(
        lng = ~long,
        lat = ~lat,
        layerId = ~id,
        radius = 5,
        color = "#222222",
        weight = 1,
        fillColor = ~ pal(class),
        fillOpacity = 0.9,
        popup = ~ paste0(
          "<b>", htmltools::htmlEscape(place), "</b><br>",
          htmltools::htmlEscape(date), "<br>",
          htmltools::htmlEscape(geipan_classes[class]), "<br>",
          "<i>", htmltools::htmlEscape(coalesce(explanation, "")), "</i>"
        )
      ) %>%
      addLegend(
        "bottomright",
        colors = unname(geipan_colors),
        labels = unname(geipan_classes),
        title = "GEIPAN class",
        opacity = 1
      )
  })

  output$geipan_case <- renderUI({
    click <- input$geipan_map_marker_click
    if (is.null(click)) {
      return(helpText("Click a point on the map to read the case."))
    }
    case <- GEIPAN %>% filter(id == click$id)
    tagList(
      h4(case$place, "–", case$date),
      p(
        strong(geipan_classes[[case$class]]),
        if (!is.na(case$explanation)) paste0("(", case$explanation, ")")
      ),
      p(em(case$summary)),
      p(HTML(gsub("\n", "<br>", htmltools::htmlEscape(case$details))))
    )
  })

  output$geipan_years_plot <- renderPlotly({
    validate(need(nrow(geipan_selection()) > 0, "No cases in the current selection."))
    p <- geipan_selection() %>%
      # ggplotly ignores scale labels, so the legend text has to be the level itself
      mutate(class = factor(geipan_classes[class], levels = geipan_classes)) %>%
      ggplot(aes(year, fill = class)) +
      geom_bar(width = 0.9, color = "#222222", linewidth = 0.2) +
      scale_fill_manual(values = setNames(geipan_colors, geipan_classes), name = NULL, drop = FALSE) +
      labs(title = "GEIPAN cases per year", x = NULL, y = "Cases") +
      theme_app() +
      theme(panel.grid.major.x = element_blank())

    dark_plotly(p)
  })

  output$geipan_explanations <- renderPlotly({
    top <- geipan_selection() %>%
      filter(
        class %in% c("A", "B"),
        !is.na(explanation),
        explanation != "Phénomène identifié non indiqué"
      ) %>%
      count(explanation, sort = TRUE) %>%
      slice_head(n = 15)
    validate(need(nrow(top) > 0, "No identified cases (class A or B) in the current selection."))

    p <- top %>%
      mutate(explanation = fct_reorder(explanation, n)) %>%
      ggplot(aes(explanation, n)) +
      geom_col(fill = "#3498db", width = 0.7) +
      coord_flip() +
      labs(title = "Most common explanations (classes A and B)", x = NULL, y = "Cases") +
      theme_app() +
      theme(panel.grid.major.y = element_blank())

    dark_plotly(p)
  })

  output$geipan_table <- DT::renderDataTable({
    geipan_selection() %>%
      transmute(
        Date = date,
        Place = place,
        Region = region,
        Class = class,
        Explanation = explanation,
        Summary = summary
      )
  },
  rownames = FALSE, selection = "none",
  style = "bootstrap", class = "table-condensed table-striped table-hover",
  options = list(pageLength = 25))
}

shinyApp(ui, server)
