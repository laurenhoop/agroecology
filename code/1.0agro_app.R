# loading libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(tidymodels)
library(knitr)
library(pROC)
library(ggplot2)
library(patchwork)
library(dplyr)
library(plotly)
library(bslib)


# defining custom theme
agriculture_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",  # or "lumen", "minty", "sandstone" for lighter themes
  primary = "#6B8E23",    # Olive green
  secondary = "#F5F5DC",  # Light brown
  success = "#228B22",    # Forest green
  background = "#DEB887",
  base_font = font_google("Merriweather")  # or "Lora" / "Roboto Slab" for organic feel
)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = agriculture_theme,
    
    # Application title
    titlePanel("Agroecology App Draft"),

    # Sidebar with 3 drop down Selection Inputs
    sidebarLayout(
        sidebarPanel(
          selectInput("family", 
                      "Choose a Crop Family:", 
                      choices = sort(unique(Harvest_clean$family))),
          
          selectInput("time", 
                      "Choose a time:", 
                      choices = c("Bimonthly", "Yearly")),
          
          selectInput("unit", 
                      "Choose a Unit:", 
                      choices = c("Pounds", "Revenue", "Both")),
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlotly({
    #establishing correct data frame to pull from
    data_to_plot <- if (input$time == "Bimonthly") Harvest_clean else harvest_total
    
    # Filter by family input
    data_to_plot <- data_to_plot %>% filter(family == input$family)
    
    # Creating plot labels
    data_to_plot <- if (input$time == "Bimonthly") {
      Harvest_clean %>%
        filter(family == input$family) %>%
        mutate(tooltip = case_when(
          input$unit == "Pounds"  ~ paste("Vegetable:", Vegetable, "<br>Pounds:", lbs),
          input$unit == "Revenue" ~ paste("Vegetable:", Vegetable, "<br>Revenue: $", Bimonthly_Cost),
          TRUE ~ paste("Vegetable:", Vegetable)
        ))
    } else {
      harvest_total %>%
        filter(family == input$family) %>%
        mutate(tooltip = case_when(
          input$unit == "Pounds"  ~ paste("Vegetable:", Vegetable, "<br>Total Pounds:", total_lbs),
          input$unit == "Revenue" ~ paste("Vegetable:", Vegetable, "<br>Total Revenue: $", total_cost),
          TRUE ~ paste("Vegetable:", Vegetable)
        ))
    }
    
    # Selecting columns
    x_var <- if (input$time == "Bimonthly") "Bimonthly" else "Vegetable"
    
    y_var <- switch(input$unit,
                    "Pounds" = if (input$time == "Yearly") "total_lbs" else "lbs",
                    "Revenue" = if (input$time == "Yearly") "total_cost" else "Bimonthly_Cost")
    
    # Creating Plot
    pounds <- ggplot(data = data_to_plot, aes(x = !!sym(x_var), y = !!sym(y_var), fill = Vegetable, text = tooltip))+
      geom_bar(stat = "identity", color = "grey2") +
      labs(title = paste(input$unit, "Harvested", input$time),
           x = NULL,
           y = input$unit) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
    
    ggplotly(pounds, tooltip = "text")
  })
  }


# Run the application 
shinyApp(ui = ui, server = server)
