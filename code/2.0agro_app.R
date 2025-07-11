
# ===== loading libraries =====
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

# ===== Define UI for application that draws a histogram ===== 
ui <- page_fillable(
  theme = shinytheme("spacelab"),
  
  # Application title
  titlePanel("Agroecology App 2.0 Draft"),
  
  # Adds a border around plot output called "highlighted card"
  tags$style(HTML("
  .highlighted-card {
    border: 2px solid #444;
    border-radius: 8px;
    box-shadow: 2px 2px 6px rgba(0,0,0,0.1);
  }
")),
  
  # === Defines first row of app ===
  layout_column_wrap(
    # == user input 1 ==
    card(
      pickerInput("year",
                  "Choose a Harvest Year",
                  choices = c("2024", "2025"),
                  selected = "2024")
    ),
    # == user input 2 ==
    card(
      radioButtons("select_type", "Select type:", choices = c("Crop Family", "Individual Crop")),
      
      conditionalPanel(
        condition = "input.select_type == 'Crop Family'",
        pickerInput("family", 
                    "Choose Crop Family:", 
                    choices = sort(unique(Harvest_clean$family)),
                    selected = "Alliaceae",
                    multiple = TRUE,
                    options = list(
                      'actions-box' = TRUE,
                      'live-search' = TRUE))
      ),
      
      conditionalPanel(
        condition = "input.select_type == 'Individual Crop'",
        pickerInput("crop", 
                    "Choose Crop:", 
                    choices =sort(unique(Harvest_clean$Vegetable)), 
                    selected = "Basil",
                    multiple = TRUE,
                    options = list(
                      'actions-box' = TRUE,
                      'live-search' = TRUE))
      ),
      
      verbatimTextOutput("selection")
    ),
    # == user input 3 ==
    card(
      pickerInput("time",
                  "Choose a Timescale",
                  choices = c("Bimonthly", "Yearly"))
    ),
    # == user input 4 ==
    card(
      pickerInput("unit",
                  "Choose a Unit",
                  choices = c ("Pounds", "Revenue", "Both")),
      conditionalPanel(
        condition = "input.time == 'Yearly' && input.unit == 'Both'",
        radioButtons("side_stack", "Select layout:", choices = c("Side by Side", "Stacked"))
      )
    ),
  ),
  
  # === Defines second row of app ===
  layout_column_wrap(
    # == visualization output ==
    card(class = "highlighted-card",
         style = "height: 60vh;", # makes graph fill 60% of whatever screen it is being displayed on
         fill = TRUE,
         plotlyOutput("plot", height = "100%")) 
  )
)

# ===== Define server logic required to draw a histogram ===== 
server <- function(input, output) {
  output$plot <- renderPlotly({
    
    # Filter by whether user selects to plot by families or individual crops
    data_to_plot <- if (input$select_type == "Crop Family") {
      Harvest_clean |> filter(family %in% input$family)
    } else {
      Harvest_clean |> filter(Vegetable %in% input$crop)
    }
    
    # Selecting columns
    x_var <- if (input$time == "Bimonthly") "Bimonthly" else "Vegetable"
    y_var <- if (input$unit == "Pounds") "lbs" else "Cost"
    
    # Creating plot labels
    data_to_plot <- data_to_plot |>
      mutate(
        tooltip = case_when(
          input$unit == "Pounds"  ~ paste("Vegetable:", Vegetable, "<br>Family:", family, "<br>Obs Pounds:", !!sym(y_var), "<br>Total Pounds:", year_lbs),
          input$unit == "Revenue" ~ paste("Vegetable:", Vegetable, "<br>Family:", family, "<br>Obs Revenue: $", !!sym(y_var), "<br>Total Revenue:", year_cost),
          TRUE ~ paste("Vegetable:", Vegetable)
        ),
        tooltip2 = paste("Vegetable:", Vegetable, "<br>Family:", family, "<br>Obs Pounds:", lbs, "<br>Total Pounds:", year_lbs),
        tooltip3 = paste("Vegetable:", Vegetable, "<br>Family:", family, "<br>Obs Revenue:", Cost, "<br>Total Revenue:", year_cost),
        tooltip4 = paste("Vegetable:", Vegetable, "<br>Family:", family, "<br>Obs Pounds:", lbs, "<br>Total Pounds:", Bimonthly_lbs),
        tooltip5 = paste("Vegetable:", Vegetable, "<br>Family:", family, "<br>Obs Revenue:", Cost, "<br>Total Revenue:", Bimonthly_Cost)
      )
    
    # === CASE 1: User selects "Both"
    if (input$unit == "Both" && input$time == "Yearly" && input$side_stack == "Side by Side") {
      
      data_long <- data_to_plot |>
        mutate(tooltip6 = paste("Vegetable:", Vegetable, 
                                "<br>Family:", family, 
                                "<br>Revenue:", year_cost, 
                                "<br>Pounds:", year_lbs)) |>
        select(Vegetable, family, year_lbs, year_cost, tooltip6) |>
        pivot_longer(
          cols = c(year_lbs, year_cost),
          names_to = "Metric",
          values_to = "Value"
        ) |>
        mutate(Metric = recode(Metric,
                               year_lbs = "Pounds",
                               year_cost = "Revenue"))
      
      p <- ggplot(data_long, aes(x = Vegetable, y = Value, fill = Metric, text = tooltip6)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
        labs(title = "Harvest by Pounds and Revenue (Side by Side)",
             x = NULL, y = "Value", fill = "Metric") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      return(ggplotly(p, tooltip = "text"))
    }
    
    # === CASE 2: User selects "Both" + Stacked
    if (input$unit == "Both" && (!input$time == "Yearly" || input$side_stack == "Stacked")) {
      lbs_tooltip <- if (input$time == "Yearly") "tooltip2" else "tooltip4"
      rev_tooltip <- if (input$time == "Yearly") "tooltip3" else "tooltip5"
      
      # Create Pounds plot
      p2 <- ggplot(data_to_plot, aes(x = !!sym(x_var), y = lbs, fill = Vegetable, text = !!sym(lbs_tooltip))) +
        geom_bar(stat = "identity") +
        scale_x_discrete(drop = FALSE) +
        labs(title = "Harvest shown in Pounds", y = "Pounds", x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # Create Revenue plot
      p3 <- ggplot(data_to_plot, aes(x = !!sym(x_var), y = Cost, fill = Vegetable, text = !!sym(rev_tooltip))) +
        geom_bar(stat = "identity") +
        scale_x_discrete(drop = FALSE) +
        labs(title = "Harvest shown in Revenue", y = "Revenue ($)", x = NULL) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      p2_ly <- ggplotly(p2, tooltip = "text")
      p3_ly <- ggplotly(p3, tooltip = "text") |>
        style(showlegend = FALSE)
      
      return(subplot(p2_ly, p3_ly, nrows = 2, shareX = TRUE, titleY = TRUE))
    }
    
    # === CASE 3: User selects only Pounds OR Revenue
    if (input$unit != "Both") {
      p4 <- ggplot(data_to_plot, aes(x = !!sym(x_var), y = !!sym(y_var), fill = Vegetable, text = tooltip)) +
        geom_bar(stat = "identity") +
        scale_x_discrete(drop = FALSE) +
        labs(title = paste(input$unit, "Harvested"), x = NULL, y = input$unit) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      return(ggplotly(p4, tooltip = "text"))
    }
  })  # closes renderPlotly
}      # closes server

# ===== Run the application ===== 
shinyApp(ui = ui, server = server)
