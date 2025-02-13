library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(DT)
library(shinythemes)
library(plotly)
library(leaflet.extras)


setwd("/Users/hawatoumbou/Documents/GitHub/Crime_Rate_DC/analysis")
getwd()

crime_data <- read.csv("../data/Cleaned_Crime_Incidents_2016_2024.csv")


# Convert REPORT_DAT to Date format
crime_data$REPORT_DAT <- as.Date(crime_data$REPORT_DAT)

# Create MONTH_YR column
crime_data$MONTH_YR <- format(crime_data$REPORT_DAT, "%Y-%m")
crime_data$HOUR <- hour(as.POSIXct(crime_data$REPORT_DAT))
crime_data$TIME_OF_DAY <- case_when(
  crime_data$HOUR >= 6 & crime_data$HOUR < 12 ~ "Morning",
  crime_data$HOUR >= 12 & crime_data$HOUR < 18 ~ "Afternoon",
  crime_data$HOUR >= 18 | crime_data$HOUR < 6 ~ "Night"
)

# Convert Year to numeric to fix max() issue
crime_data$Year <- as.numeric(as.character(crime_data$Year))

# Convert district to character before sorting
crime_data$DISTRICT <- as.character(crime_data$DISTRICT)

# Reapply sorting (descending)
crime_data$Year <- factor(crime_data$Year, levels = sort(unique(crime_data$Year), decreasing = TRUE))
crime_data$DISTRICT <- factor(crime_data$DISTRICT, levels = sort(unique(crime_data$DISTRICT), decreasing = TRUE))

# UI
theme_choice <- "flatly"
ui <- fluidPage(
  theme = shinytheme(theme_choice),
  titlePanel("Crime Incidents Dashboard (2016-2024)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = unique(crime_data$Year), selected = max(crime_data$Year)),
      selectInput("month_year", "Select Month-Year:", choices = unique(crime_data$MONTH_YR), selected = max(crime_data$MONTH_YR)),
      selectInput("crime_type", "Select Crime Type:", choices = c("All", unique(crime_data$OFFENSE)), selected = "All"),
      selectInput("district", "Select District:", choices = c("All", unique(crime_data$DISTRICT)), selected = "All"),
      selectInput("time_of_day", "Select Time of Day:", choices = c("All", "Morning", "Afternoon", "Night"), selected = "All"),
      textInput("search", "Search for Incident:", placeholder = "Enter keywords..."),
      checkboxInput("show_heatmap", "Show Crime Heatmap", value = TRUE),
      checkboxInput("show_markers", "Show Crime Markers", value = FALSE),
      downloadButton("download_data", "Download Filtered Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput("summary_table")),
        tabPanel("Trends", plotlyOutput("crime_trend_plot")),
        tabPanel("Map", leafletOutput("crime_map")),
        tabPanel("Data Table", DTOutput("crime_table"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    data <- crime_data %>% filter(Year == input$year & MONTH_YR == input$month_year)
    if (input$crime_type != "All") {
      data <- data %>% filter(OFFENSE == input$crime_type)
    }
    if (input$district != "All") {
      data <- data %>% filter(DISTRICT == input$district)
    }
    if (input$time_of_day != "All") {
      data <- data %>% filter(TIME_OF_DAY == input$time_of_day)
    }
    if (input$search != "") {
      data <- data %>% filter(grepl(input$search, OFFENSE, ignore.case = TRUE))
    }
    return(data)
  })
  
  output$summary_table <- renderTable({
    filtered_data() %>% group_by(OFFENSE) %>% summarise(Count = n())
  })
  
  output$crime_trend_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = REPORT_DAT)) +
      geom_histogram(binwidth = 30, fill = "blue", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Crime Trends Over Time", x = "Date", y = "Crime Count")
    ggplotly(p)
  })
  
  output$crime_map <- renderLeaflet({
    map <- leaflet(filtered_data()) %>% addTiles()
    if (input$show_heatmap) {
      map <- map %>%
        addHeatmap(lng = ~LONGITUDE, lat = ~LATITUDE, intensity = ~1, blur = 20, max = 1, radius = 15)
    }
    if (input$show_markers) {
      map <- map %>%
        addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, radius = 3, color = "red", opacity = 0.7, popup = ~paste("Crime Type:", OFFENSE))
    }
    map
  })
  
  output$crime_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("Filtered_Crime_Data_", input$year, ".csv", sep = "") },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)