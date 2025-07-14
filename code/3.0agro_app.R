
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
library(googlesheets4)

# Define UI for application that draws a histogram
ui <- page_fillable(
  theme = shinytheme("spacelab"),
  
  # Application title
  titlePanel("Agroecology App 3.0 Draft"),
  
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
                  choices = sort(unique(Year_test_clean$Year)),
                  selected = "2024",
                  multiple = TRUE,
                  options = list('actions-box' = TRUE,
                                 'live-search' = TRUE))
    ),
    # == user input 2 ==
    card(
      radioButtons("select_type", "Select type:", choices = c("Crop Family", "Individual Crop")),
      conditionalPanel(condition = "input.select_type == 'Crop Family'", uiOutput("family_ui")),
      conditionalPanel(condition = "input.select_type == 'Individual Crop'", uiOutput("crop_ui")),
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
  
# Define server logic required to draw a histogram
server <- function(input, output)  {
  
  # Dynamic UI: Crop Families
  output$family_ui <- renderUI({
    req(input$year)
    filtered <- Year_test_clean |> filter(Year %in% input$year)
    fams <- sort(unique(filtered$Family))
    
    pickerInput("family", 
                "Choose Crop Family:", 
                choices = fams,
                selected = fams[1],
                multiple = TRUE,
                options = list('actions-box' = TRUE, 'live-search' = TRUE))
  })
  
  # Dynamic UI: Crops
  output$crop_ui <- renderUI({
    req(input$year)
    filtered <- Year_test_clean |> filter(Year %in% input$year)
    crops <- sort(unique(filtered$Vegetable))
    
    pickerInput("crop", 
                "Choose Crop:", 
                choices = crops,
                selected = crops[1],
                multiple = TRUE,
                options = list('actions-box' = TRUE, 'live-search' = TRUE))
  })
  
  output$plot <- renderPlotly({
    req(input$year)
    
    data_to_plot <- Year_test_clean |>
      filter(
        Year %in% input$year,
        if (input$select_type == "Crop Family") Family %in% input$family else TRUE,
        if (input$select_type == "Individual Crop") Vegetable %in% input$crop else TRUE
      )

    # Selecting columns
    x_var <- if (input$time == "Bimonthly") "Bimonthly" else "Vegetable"
    y_var <- if (input$unit == "Pounds") "Quantity" else "Cost"
    
    # Creating plot labels
    data_to_plot <- data_to_plot |>
      mutate(
        tooltip = case_when(
          input$unit == "Pounds"  ~ paste("Vegetable:", Vegetable, "<br>Family:", Family,"<br>Year:", Year, "<br>Obs Pounds:", !!sym(y_var), "<br>Total Pounds:", Year_Quantity),
          input$unit == "Revenue" ~ paste("Vegetable:", Vegetable, "<br>Family:", Family,"<br>Year:", Year,"<br>Obs Revenue: $", !!sym(y_var), "<br>Total Revenue:", Year_Cost),
          TRUE ~ paste("Vegetable:", Vegetable)
        ),
        tooltip2 = paste("Vegetable:", Vegetable, "<br>Family:", Family,"<br>Year:", Year, "<br>Obs Pounds:", Quantity, "<br>Total Pounds:", Year_Quantity),
        tooltip3 = paste("Vegetable:", Vegetable, "<br>Family:", Family,"<br>Year:", Year, "<br>Obs Revenue:", Cost, "<br>Total Revenue:", Year_Cost),
        tooltip4 = paste("Vegetable:", Vegetable, "<br>Family:", Family,"<br>Year:", Year, "<br>Obs Pounds:", Quantity, "<br>Total Pounds:", Bimonthly_Quantity),
        tooltip5 = paste("Vegetable:", Vegetable, "<br>Family:", Family,"<br>Year:", Year, "<br>Obs Revenue:", Cost, "<br>Total Revenue:", Bimonthly_Cost)
      )
    
    # === CASE 1: User selects "Both"
    if (input$unit == "Both" && input$time == "Yearly" && input$side_stack == "Side by Side") {
      
      data_long <- data_to_plot |>
        mutate(tooltip6 = paste("Vegetable:", Vegetable, 
                                "<br>Family:", Family,
                                "<br>Year:", Year,
                                "<br>Revenue:", Year_Cost, 
                                "<br>Pounds:", Year_Quantity)) |>
        select(Vegetable, Family, Year_Quantity, Year_Cost, tooltip6) |>
        pivot_longer(
          cols = c(Year_Quantity, Year_Cost),
          names_to = "Metric",
          values_to = "Value"
        ) |>
        mutate(Metric = recode(Metric,
                               Year_Quantity = "Pounds",
                               Year_Cost = "Revenue"))
      
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
      Quantity_tooltip <- if (input$time == "Yearly") "tooltip2" else "tooltip4"
      rev_tooltip <- if (input$time == "Yearly") "tooltip3" else "tooltip5"
      
      # Create Pounds plot
      p2 <- ggplot(data_to_plot, aes(x = !!sym(x_var), y = Quantity, fill = Vegetable, text = !!sym(Quantity_tooltip))) +
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
  })}
  
# Run the application 
shinyApp(ui = ui, server = server)
