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
          h4("Display Controls", class = "m-0")
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
          "This dashboard compares global electricity consumption patterns,",
          "showing both total consumption and per capita usage side by side.")
      )
    ),
    
    # Main panel
    layout_column_wrap(
      width = 1,
      
      # Summary Statistics Cards
      layout_column_wrap(
        width = 1/4,
        
        # Total Consumption Card
        value_box(
          title = "Total Consumption (TWh/yr)",
          value = textOutput("total_consumption"),
          showcase = bsicons::bs_icon("lightning-charge"),
          theme_color = "primary"
        ),
        
        # Average Consumption Card
        value_box(
          title = "Average Consumption (TWh/yr)",
          value = textOutput("avg_consumption"),
          showcase = bsicons::bs_icon("calculator"),
          theme_color = "info"
        ),
        
        # Total Per Capita Card
        value_box(
          title = "Highest Per Capita (kWh/yr)",
          value = textOutput("max_per_capita"),
          showcase = bsicons::bs_icon("person"),
          theme_color = "success"
        ),
        
        # Average Per Capita Card
        value_box(
          title = "Average Per Capita (kWh/yr)",
          value = textOutput("avg_per_capita"),
          showcase = bsicons::bs_icon("people"),
          theme_color = "warning"
        )
      ),
      
      # Plots
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",
        
        # Total Consumption Plot
        card(
          class = "shadow",
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            h4("Total Consumption", class = "m-0")
          ),
          card_body(
            plotOutput("total_plot", height = "300px")
          )
        ),
        
        # Per Capita Consumption Plot
        card(
          class = "shadow",
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            h4("Per Capita Consumption", class = "m-0")
          ),
          card_body(
            plotOutput("capita_plot", height = "300px")
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
      plot.title = element_text(size = 12, face = "bold", color = "#2C3E50"),
      axis.title = element_text(size = 10, color = "#2C3E50"),
      axis.text = element_text(size = 8, color = "#34495E"),
      panel.grid.major = element_line(color = "#ECF0F1"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  # Total consumption plot
  output$total_plot <- renderPlot({
    ggplot(selected_data(), 
           aes(x = reorder(location, consumption_g_wh_yr), 
               y = consumption_g_wh_yr/1000)) +
      geom_col(fill = "#3498DB", width = 0.3) +
      geom_text(aes(label = sprintf("%.1f", consumption_g_wh_yr/1000)),
                hjust = -0.2, size = 2.5) +
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
      geom_col(fill = "#2ECC71", width = 0.3) +
      geom_text(aes(label = sprintf("%.0f", consumption_per_capita_k_wh_yr)),
                hjust = -0.2, size = 2.5) +
      coord_flip() +
      labs(x = "Country", 
           y = "Consumption per Capita (kWh/year)",
           title = "Per Capita Electricity Consumption") +
      custom_theme
  })
}

shinyApp(ui = ui, server = server)


