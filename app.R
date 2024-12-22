library(shiny)
library(tidyverse)
library(bslib)


ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2C3E50",
    "enable-shadows" = TRUE,
    "navbar-bg" = "#ECF0F1"
  ),
  
  # Title card with subtle shadow
  card(
    class = "mb-4",
    h1("Global Electricity Consumption Dashboard", 
       class = "text-center py-3 m-0",
       style = "color: #2C3E50;")
  ),
  
  # Layout
  layout_sidebar(
    # Sidebar with controls
    sidebar = sidebar(
      width = 3,
      card(
        class = "shadow-sm",
        card_header(
          class = "bg-primary text-white",
          h4("Visualization Controls", class = "m-0")
        ),
        radioButtons(
          "metric", "Select View Type:",
          choices = c(
            "Total Consumption (TWh/year)" = "consumption_g_wh_yr",
            "Per Capita Consumption (kWh/year)" = "consumption_per_capita_k_wh_yr",
            "Weighted Composite" = "composite"
          ),
          selected = "consumption_g_wh_yr"
        ),
        conditionalPanel(
          condition = "input.metric == 'composite'",
          sliderInput(
            "weight", 
            "Weight (Total vs Per Capita)",
            min = 0, max = 1, value = 0.5, step = 0.1
          )
        ),
        sliderInput(
          "top_n", 
          "Show Top N Countries:",
          min = 5, max = 50, value = 20
        )
      ),
      # Info card
      card(
        class = "mt-3 shadow-sm",
        card_header(
          class = "bg-primary text-white",
          h4("About", class = "m-0")
        ),
        p(class = "p-3 m-0",
          "This dashboard visualizes global electricity consumption patterns,",
          "allowing you to compare countries based on total consumption,",
          "per capita usage, or a weighted combination of both metrics.")
      )
    ),
    
    # Main panel with visualization
    card(
      class = "shadow",
      card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        h4("Consumption Analysis", class = "m-0")
      ),
      card_body(
        plotOutput("main_plot", height = "600px")
      )
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
      data <- energy |>
        mutate(
          total_normalized = (consumption_g_wh_yr - min(consumption_g_wh_yr)) / 
            (max(consumption_g_wh_yr) - min(consumption_g_wh_yr)),
          capita_normalized = (consumption_per_capita_k_wh_yr - min(consumption_per_capita_k_wh_yr)) / 
            (max(consumption_per_capita_k_wh_yr) - min(consumption_per_capita_k_wh_yr)),
          composite_score = (1 - input$weight) * total_normalized + 
            input$weight * capita_normalized
        ) |>
        arrange(desc(composite_score)) |>
        head(input$top_n)
    }
    return(data)
  })
  
  # Main plot
  output$main_plot <- renderPlot({
    data <- filtered_data()
    
    # Custom theme
    custom_theme <- theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#2C3E50"),
        axis.title = element_text(size = 12, color = "#2C3E50"),
        axis.text = element_text(size = 10, color = "#34495E"),
        panel.grid.major = element_line(color = "#ECF0F1"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 10, 10, 10)
      )
    
    if (input$metric == "consumption_g_wh_yr") {
      ggplot(data, aes(x = reorder(location, consumption_g_wh_yr), 
                       y = consumption_g_wh_yr/1000)) +
        geom_col(fill = "#3498DB", width = 0.3) +
        geom_text(aes(label = sprintf("%.1f", consumption_g_wh_yr/1000)),
                  hjust = -0.2, size = 3.5) +
        coord_flip() +
        labs(x = "Country", 
             y = "Total Consumption (TWh/year)",
             title = "Total Electricity Consumption by Country") +
        custom_theme
      
    } else if (input$metric == "consumption_per_capita_k_wh_yr") {
      ggplot(data, aes(x = reorder(location, consumption_per_capita_k_wh_yr), 
                       y = consumption_per_capita_k_wh_yr)) +
        geom_col(fill = "#2ECC71", width = 0.3) +
        geom_text(aes(label = sprintf("%.0f", consumption_per_capita_k_wh_yr)),
                  hjust = -0.2, size = 3.5) +
        coord_flip() +
        labs(x = "Country", 
             y = "Consumption per Capita (kWh/year)",
             title = "Per Capita Electricity Consumption by Country") +
        custom_theme
      
    } else {
      ggplot(data, aes(x = reorder(location, composite_score), 
                       y = composite_score)) +
        geom_col(fill = "#E74C3C", width = 0.3) +
        geom_text(aes(label = sprintf("%.3f", composite_score)),
                  hjust = -0.2, size = 3.5) +
        coord_flip() +
        labs(x = "Country", 
             y = "Composite Score",
             title = sprintf("Weighted Composite Score (%.0f%% Total, %.0f%% Per Capita)", 
                             (1-input$weight)*100, input$weight*100)) +
        custom_theme
    }
  })
}

shinyApp(ui = ui, server = server)


