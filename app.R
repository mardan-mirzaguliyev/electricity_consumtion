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
  
  # Title card
  card(
    class = "mb-3",
    h1("Global Electricity Consumption Dashboard", 
       class = "text-center py-2 m-0",
       style = "color: #2C3E50; font-size: 24px;")
  ),
  
  # Layout
  layout_sidebar(
    # Sidebar with controls
    sidebar = sidebar(
      width = 250,
      padding = 1,
      card(
        class = "shadow-sm mb-2",
        card_header(
          class = "bg-primary text-white py-2",
          h4("Display Controls", class = "m-0", style = "font-size: 16px;")
        ),
        sliderInput(
          "top_n", 
          "Show Top N Countries:",
          min = 5, max = 20, value = 5  # Changed max to 30
        )
      ),
      # Info card
      card(
        class = "shadow-sm",
        card_header(
          class = "bg-primary text-white py-2",
          h4("About", class = "m-0", style = "font-size: 12px;")
        ),
        p(class = "p-2 m-0 small",
          "This dashboard compares global electricity consumption patterns,",
          "showing both total consumption and per capita usage side by side.",
          "Electric energy per capita [ in watt-hour ] = Total population electricity consumption [in kW·h/yr] × 1,000 / population.",
          "Electric power per capita [ in watt ] = Total population electricity consumption [in kW·h/yr] × 0.114077116 / population.",
          "1 kW·h/yr = 1,000 Wh/(365.25 × 24) h = 0.11408 Watt")
      )
    ),
    
    # Main panel
    layout_column_wrap(
      width = 1,
      style = "gap: 0.5rem;",
      
      # Summary Statistics Cards
      layout_column_wrap(
        width = 1/4,
        style = "gap: 0.5rem;",
        
        value_box(
          title = "Total Consumption (TWh/yr)",
          value = textOutput("total_consumption"),
          showcase = bsicons::bs_icon("lightning-charge"),
          theme_color = "primary",
          height = 80
        ),
        value_box(
          title = "Average Consumption (TWh/yr)",
          value = textOutput("avg_consumption"),
          showcase = bsicons::bs_icon("calculator"),
          theme_color = "info",
          height = 80
        ),
        value_box(
          title = "Highest Per Capita (kWh/yr)",
          value = textOutput("max_per_capita"),
          showcase = bsicons::bs_icon("person"),
          theme_color = "success",
          height = 80
        ),
        value_box(
          title = "Average Per Capita (kWh/yr)",
          value = textOutput("avg_per_capita"),
          showcase = bsicons::bs_icon("people"),
          theme_color = "warning",
          height = 80
        )
      ),
      
      # Plots
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        style = "gap: 1.5rem;",
        
        # Total Consumption Plot
        card(
          class = "shadow-sm",
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white py-2",
            h4("Total Consumption", class = "m-0", style = "font-size: 16px;")
          ),
          card_body(
            padding = 2,
            plotOutput("total_plot", height = "400px")
          )
        ),
        
        # Per Capita Consumption Plot
        card(
          class = "shadow-sm",
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white py-2",
            h4("Per Capita Consumption", class = "m-0", style = "font-size: 16px;")
          ),
          card_body(
            padding = 2,
            plotOutput("capita_plot", height = "400px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data
  selected_data <- reactive({
    energy |>
      arrange(desc(consumption_g_wh_yr)) |>
      head(input$top_n)
  })
  
  # Dynamic statistics
  output$total_consumption <- renderText({
    total <- sum(selected_data()$consumption_g_wh_yr)/1000
    sprintf("%.1f", total)
  })
  
  output$avg_consumption <- renderText({
    avg <- mean(selected_data()$consumption_g_wh_yr)/1000
    sprintf("%.1f", avg)
  })
  
  output$max_per_capita <- renderText({
    max_pc <- max(selected_data()$consumption_per_capita_k_wh_yr)
    sprintf("%.0f", max_pc)
  })
  
  output$avg_per_capita <- renderText({
    avg_pc <- mean(selected_data()$consumption_per_capita_k_wh_yr)
    sprintf("%.0f", avg_pc)
  })
  
  # Custom theme for plots
  custom_theme <- theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", color = "#2C3E50"),
      axis.title = element_text(size = 9, color = "#2C3E50"),
      axis.text = element_text(size = 8, color = "#34495E"),
      panel.grid.major = element_line(color = "#ECF0F1"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  # Total consumption plot
  output$total_plot <- renderPlot({
    ggplot(selected_data(), 
           aes(x = reorder(location, desc(rank)), 
               y = consumption_g_wh_yr)) +
      geom_col(fill = "#3498DB", width = 0.7) +
      geom_text(aes(label = sprintf("%.1f", consumption_g_wh_yr)),
                hjust = 1, vjust = 0.5,  # Changed hjust to 1 to put labels inside
                color = "white", size = 5) +  # Made text white for better visibility
      coord_flip() +
      labs(x = "Country", 
           y = "Total Consumption (TWh/year)",
           title = "Total Electricity Consumption") +
      custom_theme
  })
  
  # Per capita consumption plot
  output$capita_plot <- renderPlot({
    ggplot(selected_data(), 
           aes(x = reorder(location, consumption_per_capita_k_wh_yr),
               y = consumption_per_capita_k_wh_yr)) +
      geom_col(fill = "#2ECC71", width = 0.7) +
      geom_text(aes(label = sprintf("%.0f", consumption_per_capita_k_wh_yr)),
                hjust = 1, vjust = 0.5,  # Changed hjust to 1 to put labels inside
                color = "white", size = 2.5) +  # Made text white for better visibility
      coord_flip() +
      labs(x = "Country", 
           y = "Consumption per Capita (kWh/year)",
           title = "Per Capita Electricity Consumption") +
      custom_theme
  })
}

shinyApp(ui = ui, server = server)


