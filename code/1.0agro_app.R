# loading libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)
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
  bg = "#e4cda0",
  fg = "#a67c52",
  primary = "#3f5936",
  secondary = "#5c4a3e",
  success = "#f5efe5",
  base_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono")
)

# Define UI for application that draws a histogram
ui <- page_fillable(
  theme = shinytheme("spacelab"),
  
  # Application title
  titlePanel("Agroecology App 1.0 Draft"),
  
  #== defines first row of application ==
  layout_columns(
    # first user selection
    card(width = 2,
      card_header("Card 1"),
      pickerInput("family", 
                         "Choose Crop Families", 
                         choices = sort(unique(Harvest_clean$family)),
                         selected = "Alliaceae",
                         width = "75%",
                         multiple = TRUE,
                         options = list(
                         `actions-box` = TRUE,  # adds "Select All" and "Deselect All"
                         `live-search` = TRUE))  # optional search box),
    ),
    # second user selection
    card(width = 2,
      card_header("Card 2"),
      pickerInput("time", 
                  "Choose a time:", 
                  choices = c("Bimonthly", "Yearly"))
    ),
    # third user selection
    card(width = 2,
      card_header("Card 3"),
      pickerInput("unit", 
                  "Choose a Unit:", 
                  choices = c("Pounds", "Revenue", "Both"))
    ),
  ),
  
  #== defines second row of application ==
  layout_columns(
    ## plot output
    card(width = 12,
      card_header("Card 4"),
      plotlyOutput("plot")
    ),
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlotly({
    #establishing correct data frame to pull from
    data_to_plot <- if (input$time == "Bimonthly") Harvest_clean else harvest_total
    
    # Filter by family input
    data_to_plot <- data_to_plot |> filter(family %in% input$family)
    
    # Selecting columns
    x_var <- if (input$time == "Bimonthly") "Bimonthly" else "Vegetable"
    y_pounds <- if(input$time == "Yearly") "total_lbs" else "lbs"
    y_revenue <- if (input$time == "Yearly") "total_cost" else "Bimonthly_Cost"
    
    # Creating plot labels
    data_to_plot <- data_to_plot %>%
      mutate(
        tooltip = case_when(
          input$unit == "Pounds"  ~ paste("Vegetable:", Vegetable, "<br>Family", family, "<br>Pounds:", !!sym(y_pounds)),
          input$unit == "Revenue" ~ paste("Vegetable:", Vegetable, "<br>Family", family, "<br>Revenue: $", !!sym(y_revenue)),
          TRUE ~ paste("Vegetable:", Vegetable)
        ),
        tooltip2 = paste("Vegetable:", Vegetable, "<br>Family", family, "<br>Pounds:", !!sym(y_pounds)),
        tooltip3= paste ("Vegetable:", Vegetable, "<br>Family", family, "<br>Revenue:", !!sym(y_revenue))
      )
    
    # === CASE 1: User selects "Both" ===
    if (input$unit == "Both") {
      # Create pounds plot
      p1 <- ggplot(data_to_plot, aes(x = !!sym(x_var), y = !!sym(y_pounds), fill = Vegetable, text = tooltip2)) +
        geom_bar(stat = "identity") +
        labs(title = "Harvest shown in Pounds and Revenue", y = "Pounds", x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Create revenue plot
      y_rev <- if (input$time == "Yearly") "total_cost" else "Bimonthly_Cost"
      p2 <- ggplot(data_to_plot, aes(x = !!sym(x_var), y = !!sym(y_revenue), fill = Vegetable, text = tooltip3)) +
        geom_bar(stat = "identity") +
        labs(title = "Harvest shown in Pounds and Revenue", y = "Revenue ($)", x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
      
      # Ensures no duplicate legend + Convert ggplots to plotly format 
      p1_ly <- ggplotly(p1, tooltip = "text")
      p2_ly <- ggplotly(p2, tooltip = "text") |>
          style(showlegend = FALSE)
      
      # Combine plots with shared x-axis and one legend
      subplot(p1_ly, p2_ly, nrows = 2, shareX = TRUE, titleY = TRUE)
      
    } else {
      # === CASE 2: User selects only one unit ===
      y_var <- switch(input$unit,
                      "Pounds" = if (input$time == "Yearly") "total_lbs" else "lbs",
                      "Revenue" = if (input$time == "Yearly") "total_cost" else "Bimonthly_Cost")
      
      p <- ggplot(data_to_plot, aes(x = !!sym(x_var), y = !!sym(y_var), fill = Vegetable, text = tooltip)) +
        geom_bar(stat = "identity") +
        labs(title = paste(input$unit, "Harvested"), x = NULL, y = input$unit) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip = "text")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
