#Step:1:Load the necessary libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(ecotourism)

#Step:2:"Load and combine datasets from ecoutourism package

data(glowworms)
data(gouldian_finch)
data(manta_rays)
data(orchids)

all_data <- bind_rows(
  glowworms      |> mutate(organism = "Glowworms"),
  gouldian_finch |> mutate(organism = "Gouldian Finch"),
  manta_rays     |> mutate(organism = "Manta Rays"),
  orchids        |> mutate(organism = "Orchids")
)

organism_choices <- sort(unique(all_data$organism))

month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")

#Step:3:Define User-Interface (ui) of the Shiny Dashboard.Any shiny dashboard has 3 parts:Header,Sidebar and Body
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = tags$span(
      style = "font-weight:600;font-size:16px;",
      "Australian Wildlife Explorer"
    )
  ),
  
  dashboardSidebar(
    tags$head(
      tags$style(HTML("  /* your CSS unchanged */  "))
    ),
    
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "map", icon = icon("map"))
    ),
    br(),
    
    selectInput(
      "organism", "Organism",
      choices = organism_choices,
      selected = "Manta Rays"
    ),
    selectInput(
      "month", "Month",
      choices = c("All months" = 0, setNames(1:12, month_labels)),
      selected = 0
    ),
    
    
    sliderInput(
      "min_sightings",
      "Minimum sightings at a location",
      min   = 1,
      max   = 40,
      value = 5,
      step  = 1
    ),
    dateRangeInput(
      "date_range",
      "Select date",
      start = min(all_data$date, na.rm = TRUE),
      end   = max(all_data$date, na.rm = TRUE)
    )
  ),
  

  dashboardBody(
    tabItems(
      tabItem(
        tabName = "map",
        fluidPage(
          
          # KPI row
          fluidRow(
            div(class = "kpi-flat",
                valueBoxOutput("vb_total",     width = 4),
                valueBoxOutput("vb_locations", width = 4),
                valueBoxOutput("vb_years",     width = 4)
            )
          ),
          
          # insights row
          fluidRow(
            column(
              width = 12,
              box(
                title = "Insights",
                width = NULL, solidHeader = TRUE, status = "primary",
                p(textOutput("insights_text"),
                  style = "font-size:12px;margin-bottom:0;")
              )
            )
          ),
          
          # middle row
          fluidRow(
            column(
              width = 7,
              box(
                title = "Sightings map",
                width = NULL, solidHeader = TRUE, status = "primary",
                leafletOutput("map", height = 420)
              )
            ),
            column(
              width = 5,
              box(
                title = "States summary",
                width = NULL, solidHeader = TRUE, status = "primary",
                DTOutput("state_table"),
                footer = div(
                  style = "font-size:11px;color:#6b7280;margin-top:4px;",
                  "State-level counts for the current organism and month."
                )
              )
            )
          ),
          
          # bottom row
          fluidRow(
            column(
              width = 3,
              box(
                title = "Selection details",
                width = NULL, solidHeader = TRUE, status = "primary",
                verbatimTextOutput("summary_text")
              )
            ),
            column(
              width = 9,
              box(
                title = "Top states by sightings",
                width = NULL, solidHeader = TRUE, status = "primary",
                plotOutput("top_states_plot", height = 220)
              )
            )
          )
        )
      )
    )
  )
)


#Step:4:Define the Server logic

server <- function(input, output, session) {
  
  # filtered data for chosen organism + optional month
  filtered_data <- reactive({
    d <- all_data |> filter(organism == input$organism)
    if (as.integer(input$month) != 0) {
      d <- d |> filter(month == as.integer(input$month))
    }
    
    # filter by date range
    d <- d |>
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
    
    # keep only locations with at least min_sightings
    d <- d |>
      group_by(obs_lat, obs_lon) |>
      filter(n() >= input$min_sightings) |>
      ungroup()
    
    d
  })
  
  # KPI
  
  output$vb_total <- renderValueBox({
    d <- filtered_data()
    valueBox(
      value    = format(nrow(d), big.mark = ","),
      subtitle = "Total sightings",
      color    = "light-blue"
    )
  })
  
  output$vb_locations <- renderValueBox({
    d <- filtered_data()
    n <- d |> distinct(obs_lat, obs_lon) |> nrow()
    valueBox(
      value    = format(n, big.mark = ","),
      subtitle = "Unique locations",
      color    = "aqua"
    )
  })
  
  output$vb_years <- renderValueBox({
    d <- all_data |> filter(organism == input$organism)
    yrs <- if (!nrow(d)) "–" else
      paste(min(d$year, na.rm = TRUE),
            max(d$year, na.rm = TRUE),
            sep = "–")
    valueBox(
      value    = yrs,
      subtitle = "Years covered",
      color    = "navy"
    )
  })
  
  # Insights card 
  
  output$insights_text <- renderText({
    d <- filtered_data()
    if (!nrow(d)) return("No records for this selection.")
    
    st <- d |> count(obs_state, sort = TRUE) |> slice_head(n = 1)
    pm <- d |> count(month, sort = TRUE)     |> slice_head(n = 1)
    
    paste0(
      "Most sightings are in ", st$obs_state,
      ", with peak activity in ", month_labels[pm$month], "."
    )
  })
  
  # Australia's map
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3, maxZoom = 10)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = 134, lat = -25, zoom = 4) 
  })
  
  observeEvent(filtered_data(), {
    d <- filtered_data()
    if (!nrow(d)) {
      leafletProxy("map") |> clearMarkers() |> clearMarkerClusters()
      return()
    }
    
    pal <- colorNumeric(
      palette = c("#22c55e", "#3b82f6", "#f97316"),
      domain  = d$obs_lat
    )
    
    leafletProxy("map") |>
      clearMarkers() |>
      clearMarkerClusters() |>
      addCircleMarkers(
        data = d,
        lng = ~obs_lon,
        lat = ~obs_lat,
        radius = 6,
        color = ~pal(obs_lat),
        fillColor = ~pal(obs_lat),
        fillOpacity = 0.75,
        stroke = TRUE,
        weight = 1,
        clusterOptions = markerClusterOptions(),
        popup = ~paste0(
          "<b><i>", sci_name, "</i></b><br>",
          "Date: ", date, "<br>",
          "State: ", obs_state
        )
      )
  })
  
  # state-level summary table 
  
  state_summary <- reactive({
    filtered_data() |>
      group_by(obs_state) |>
      summarise(
        total_sightings = n(),
        locations       = n_distinct(paste(obs_lat, obs_lon)),
        .groups = "drop"
      ) |>
      arrange(desc(total_sightings))
  })
  
  output$state_table <- renderDT({
    d <- state_summary()
    datatable(
      d,
      colnames = c("State", "Total sightings", "# locations"),
      options = list(
        pageLength   = 6,
        searching    = TRUE,
        lengthChange = FALSE,
        dom          = "tip"
      ),
      rownames  = FALSE,
      selection = "single",
      class     = "compact"
    )
  })
  
  # selection summary text 
  
  output$summary_text <- renderPrint({
    d <- filtered_data()
    if (!nrow(d)) {
      cat("No records for this selection.")
      return()
    }
    cat("Organism:", input$organism, "\n")
    cat("Total sightings:", nrow(d), "\n")
    cat("Unique locations:",
        nrow(distinct(d, obs_lat, obs_lon)), "\n")
    cat("States:", length(unique(d$obs_state)), "\n")
  })
  
  # top states bar plot 
  
  output$top_states_plot <- renderPlot({
    d <- state_summary() |> slice_head(n = 8)
    if (!nrow(d)) return(NULL)
    
    ggplot(d, aes(x = reorder(obs_state, total_sightings),
                  y = total_sightings)) +
      geom_col(fill = "#2563eb") +
      coord_flip() +
      labs(x = NULL, y = "Sightings") +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank()
      )
  })
}
#Step:5:Launch the shiny app
shinyApp(ui, server)
