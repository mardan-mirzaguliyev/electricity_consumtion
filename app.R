library(shiny)
library(tidyverse)
library(bslib)
library(plotly)

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
        radioButtons("metric", "Select View Type",
                     choices = c("Total Consumption (TWh/year)" = "consumption_g_wh_yr",
                                 "Per Capita Consumption (kWh/year)" = "consumption_per_capita_k_wh_yr",
                                 "Weighted Composite" = "composite")),
        conditionalPanel(
          condition = "input.metric == 'composite'",
          sliderInput("weight", "Weight (Total vs Per Capita)",
                      min = 0, max = 1, value = 0.5, step = 0.1)
        ),
        sliderInput("top_n", "Show Top N Countries",
                    min = 5, max = 50, value = 20)
      )
    ),
    
    # Main panel with visualization
    card(
      card_header("Consumption Bar Plot"),
      plotlyOutput("main_plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered dataset
  filtered_data <- reactive({
    if (input$metric == "consumption_g_wh_yr") {
      data <- energy |>
        arrange(desc(consumption_g_wh_yr)) |>
        head(input$top_n)
    } else if (input$metric == "consumption_per_capita_k_wh_yr") {
      data <- energy |>
        arrange(desc(consumption_per_capita_k_wh_yr)) |>
        head(input$top_n)
    } else {
      # Calculate composite score
      data <- energy |>
        mutate(
          # Normalize both metrics to 0-1 scale
          total_normalized = (consumption_g_wh_yr - min(consumption_g_wh_yr)) / 
            (max(consumption_g_wh_yr) - min(consumption_g_wh_yr)),
          capita_normalized = (consumption_per_capita_k_wh_yr - min(consumption_per_capita_k_wh_yr)) / 
            (max(consumption_per_capita_k_wh_yr) - min(consumption_per_capita_k_wh_yr)),
          # Calculate weighted score
          composite_score = (1 - input$weight) * total_normalized + 
            input$weight * capita_normalized
        ) |>
        arrange(desc(composite_score)) |>
        head(input$top_n)
    }
    return(data)
  })
  
  # Main plot
  output$main_plot <- renderPlotly({
    data <- filtered_data()
    
    if (input$metric == "consumption_g_wh_yr") {
      # Total consumption plot
      p <- ggplot(data, aes(x = reorder(location, consumption_g_wh_yr), 
                            y = consumption_g_wh_yr/1000,
                            text = paste0("Country: ", location,
                                          "\nTotal Consumption: ", format(round(consumption_g_wh_yr/1000, 1), big.mark = ","), " TWh/year",
                                          "\nPer Capita: ", format(round(consumption_per_capita_k_wh_yr, 1), big.mark = ","), " kWh/year",
                                          "\nPopulation: ", format(population, big.mark = ",")))) +
        geom_bar(stat = "identity", fill = "#2C3E50") +
        coord_flip() +
        labs(x = "Country", 
             y = "Total Consumption (TWh/year)",
             title = "Total Electricity Consumption by Country") +
        theme_minimal()
    } else if (input$metric == "consumption_per_capita_k_wh_yr") {
      # Per capita consumption plot
      p <- ggplot(data, aes(x = reorder(location, consumption_per_capita_k_wh_yr), 
                            y = consumption_per_capita_k_wh_yr,
                            text = paste0("Country: ", location,
                                          "\nPer Capita: ", format(round(consumption_per_capita_k_wh_yr, 1), big.mark = ","), " kWh/year",
                                          "\nTotal Consumption: ", format(round(consumption_g_wh_yr/1000, 1), big.mark = ","), " TWh/year",
                                          "\nPopulation: ", format(population, big.mark = ",")))) +
        geom_bar(stat = "identity", fill = "#2C3E50") +
        coord_flip() +
        labs(x = "Country", 
             y = "Consumption per Capita (kWh/year)",
             title = "Per Capita Electricity Consumption by Country") +
        theme_minimal()
    } else {
      # Composite score plot
      p <- ggplot(data, aes(x = reorder(location, composite_score), 
                            y = composite_score,
                            text = paste0("Country: ", location,
                                          "\nComposite Score: ", round(composite_score, 3),
                                          "\nTotal Consumption: ", format(round(consumption_g_wh_yr/1000, 1), big.mark = ","), " TWh/year",
                                          "\nPer Capita: ", format(round(consumption_per_capita_k_wh_yr, 1), big.mark = ","), " kWh/year"))) +
        geom_bar(stat = "identity", fill = "#2C3E50") +
        coord_flip() +
        labs(x = "Country", 
             y = "Composite Score",
             title = paste0("Weighted Composite Score (", 
                            (1-input$weight)*100, "% Total, ", 
                            input$weight*100, "% Per Capita)")) +
        theme_minimal()
    }
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui = ui, server = server)

