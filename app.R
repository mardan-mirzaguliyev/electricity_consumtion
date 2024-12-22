library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(plotly)

source("01-import-data.R")


ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  
  # Title
  h1("Global Electricity Consumption Dashboard", class = "text-center mb-4"),
  
  # Layout
  layout_sidebar(
    # Sidebar with controls
    sidebar = sidebar(
      width = 3,
      card(
        card_header("Controls"),
        selectInput("view_type", "View Type",
                    choices = c("Total Consumption" = "total",
                                "Per Capita Consumption" = "per_capita")),
        sliderInput("top_n", "Show Top N Countries",
                    min = 5, max = 50, value = 20),
        selectInput("plot_type", "Plot Type",
                    choices = c("Bar Plot" = "bar",
                                "Bubble Plot" = "bubble"))
      )
    ),
    
    # Main panel with visualizations
    card(
      card_header("Visualization"),
      plotlyOutput("main_plot", height = "500px")
    ),
    
    card(
      card_header("Data Table"),
      DTOutput("data_table")
    )
  )
)

server <- function(input, output) {
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    if (input$view_type == "total") {
      data <- energy |>
        arrange(desc(consumption_g_wh_yr)) |>
        head(input$top_n)
    } else {
      data <- energy |>
        arrange(desc(consumption_per_capita_k_wh_yr)) |>
        head(input$top_n)
    }
    return(data)
  })
  
  # Main plot
  output$main_plot <- renderPlotly({
    data <- filtered_data()
    
    if (input$plot_type == "bar") {
      if (input$view_type == "total") {
        p <- ggplot(data, aes(x = reorder(location, consumption_g_wh_yr), 
                              y = consumption_g_wh_yr/1000)) +
          geom_bar(stat = "identity", fill = "#2C3E50") +
          coord_flip() +
          labs(x = "Country", y = "Consumption (TWh/year)") +
          theme_minimal()
      } else {
        p <- ggplot(data, aes(x = reorder(location, consumption_per_capita_k_wh_yr), 
                              y = consumption_per_capita_k_wh_yr)) +
          geom_bar(stat = "identity", fill = "#2C3E50") +
          coord_flip() +
          labs(x = "Country", y = "Consumption per Capita (kWh/year)") +
          theme_minimal()
      }
    } else {
      if (input$view_type == "total") {
        p <- ggplot(data, aes(x = population/1e6, y = consumption_g_wh_yr/1000, 
                              size = consumption_per_capita_k_wh_yr,
                              text = location)) +
          geom_point(alpha = 0.6, color = "#2C3E50") +
          labs(x = "Population (Millions)", y = "Consumption (TWh/year)",
               size = "Per Capita Consumption") +
          theme_minimal()
      } else {
        p <- ggplot(data, aes(x = population/1e6, y = consumption_per_capita_k_wh_yr,
                              size = consumption_g_wh_yr/1000,
                              text = location)) +
          geom_point(alpha = 0.6, color = "#2C3E50") +
          labs(x = "Population (Millions)", y = "Consumption per Capita (kWh/year)",
               size = "Total Consumption (TWh)") +
          theme_minimal()
      }
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  # Data table
  output$data_table <- renderDT({
    data <- filtered_data()
    datatable(data, 
              options = list(pageLength = 10,
                             scrollX = TRUE),
              rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)


