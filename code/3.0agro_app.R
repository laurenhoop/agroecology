
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

# ===== loading Google sheet =====
gs4_deauth()
Master_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1st3bYMIaT2TEjFxW_pe2pP1_yltTycQM0Si1yT2bcyo/edit?usp=sharing")

# ===== cleaning data =====
Master_sheet_clean <- Master_sheet |>
  na.omit() |> #gets rid of NA values
  mutate(
    #removing dollar sign
    Cost = as.numeric(gsub("\\$", "", Cost)) 
  ) |>
  mutate(
    #changing variable type of date variable
    Date = as.Date(Date, format = "%m/%d/%Y"), 
    #creates the bimonthly variable to be formatted as 'Early/Late' + 'Month Abbreviation'
    Bimonthly = case_when(
      day(Date) <= 15 ~ paste("Early", month(Date, label = TRUE, abbr = TRUE), Year),
      day(Date) > 15  ~ paste("Late", month(Date, label = TRUE, abbr = TRUE), Year)
    ), 
    # ensures xaxis is chronological
    Bimonthly = factor(Bimonthly, levels = unique(Bimonthly[order(Date)]))
  ) |>
  group_by(Year, Bimonthly, Vegetable) |>
  # adding total cost + quantity per vegetable per bimonthly period
  mutate(Bimonthly_Cost = sum(Cost, na.rm = TRUE),
         Bimonthly_Quantity = sum(Quantity, na.rm = TRUE)) |>
  ungroup() |>
  
  group_by(Year, Vegetable) |>
  # adding total cost + quantity per vegetable per year 
  mutate(
    Year_Quantity = sum(Quantity, na.rm = TRUE),
    Year_Cost = sum(Cost, na.rm = TRUE)
  ) |>
  ungroup()

#============== Define UI for application that draws a histogram ==============
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
                  choices = sort(unique(Master_sheet_clean$Year)),
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
                  "Choose a Harvest Unit",
                  choices = c ("Yield", "Revenue", "Both")),
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
  
#============== Define server logic required to draw a histogram ==============
server <- function(input, output)  {
  
  # Dynamic UI: Crop Families
  output$family_ui <- renderUI({
    req(input$year)
    filtered <- Master_sheet_clean |> filter(Year %in% input$year)
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
    filtered <- Master_sheet_clean |> filter(Year %in% input$year)
    crops <- sort(unique(filtered$Vegetable))
    
    pickerInput("crop", 
                "Choose Crop:", 
                choices = crops,
                selected = crops[1],
                multiple = TRUE,
                options = list('actions-box' = TRUE, 'live-search' = TRUE))
  })
  
  output$plot <- renderPlotly({
    # creates a new bimonthly variable that filters out years that are not selected
    bimonthly_levels <- Master_sheet_clean |>
      filter(Year %in% input$year) |>
      distinct(Bimonthly) |>
      pull(Bimonthly)
    
    # filters plotted observations based on year(s) selected
    req(input$year)
    data_to_plot <- Master_sheet_clean |>
      filter(
        Year %in% input$year,
        if (input$select_type == "Crop Family") Family %in% input$family else TRUE,
        if (input$select_type == "Individual Crop") Vegetable %in% input$crop else TRUE
      )

    # Selecting columns
    x_var <- if (input$time == "Bimonthly") "Bimonthly" else "Vegetable"
    y_var <- if (input$unit == "Yield") "Quantity" else "Cost"
    
    # Creating plot labels + mutating parent Bimonthly variable to only contain filtered values from above
    data_to_plot <- data_to_plot |>
      mutate(
        Bimonthly = factor(Bimonthly, levels = bimonthly_levels),
        tooltip = case_when(
          input$unit == "Yield"  ~ paste("Year:", Year, "<br>Vegetable:", Vegetable, "<br>Family:", Family, "<br>Yield Unit:", Unit, "<br>Obs Yield:", !!sym(y_var), "<br>Total Yield:", Year_Quantity),
          input$unit == "Revenue" ~ paste("Year:", Year, "<br>Vegetable:", Vegetable, "<br>Family:", Family, "<br>Yield Unit:", Unit, "<br>Obs Revenue: $", !!sym(y_var), "<br>Total Revenue:", Year_Cost),
          TRUE ~ paste("Vegetable:", Vegetable)
        ),
        tooltip2 = paste("Year:", Year, "<br>Vegetable:", Vegetable, "<br>Family:", Family, "<br>Yield Unit:", Unit, "<br>Obs Yield:", Quantity, "<br>Total Yield:", Year_Quantity),
        tooltip3 = paste("Year:", Year, "<br>Vegetable:", Vegetable, "<br>Family:", Family, "<br>Yield Unit:", Unit, "<br>Obs Revenue:", Cost, "<br>Total Revenue:", Year_Cost),
        tooltip4 = paste("Year:", Year, "<br>Vegetable:", Vegetable, "<br>Family:", Family, "<br>Yield Unit:", Unit, "<br>Obs Yield:", Quantity, "<br>Total Yield:", Bimonthly_Quantity),
        tooltip5 = paste("Year:", Year, "<br>Vegetable:", Vegetable, "<br>Family:", Family, "<br>Yield Unit:", Unit, "<br>Obs Revenue:", Cost, "<br>Total Revenue:", Bimonthly_Cost)
      )
    
    # === CASE 1: User selects "Both"
    if (input$unit == "Both" && input$time == "Yearly" && input$side_stack == "Side by Side") {
      
      data_long <- data_to_plot |>
        mutate(tooltip6 = paste("Year:", Year,
                                "<br>Vegetable:", Vegetable, 
                                "<br>Family:", Family,
                                "<br>Revenue:", Year_Cost, 
                                "<br>Yield:", Year_Quantity)) |>
        select(Vegetable, Family, Year_Quantity, Year_Cost, tooltip6) |>
        pivot_longer(
          cols = c(Year_Quantity, Year_Cost),
          names_to = "Metric",
          values_to = "Value"
        ) |>
        mutate(Metric = recode(Metric,
                               Year_Quantity = "Yield",
                               Year_Cost = "Revenue"))
      
      p <- ggplot(data_long, aes(x = Vegetable, y = Value, fill = Metric, text = tooltip6)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
        scale_x_discrete(drop = FALSE) +
        labs(title = "Harvest by Yield and Revenue (Side by Side)",
             x = NULL, y = "Value", fill = "Metric") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      return(ggplotly(p, tooltip = "text"))
    }
    
    # === CASE 2: User selects "Both" + Stacked
    if (input$unit == "Both" && (!input$time == "Yearly" || input$side_stack == "Stacked")) {
      Quantity_tooltip <- if (input$time == "Yearly") "tooltip2" else "tooltip4"
      rev_tooltip <- if (input$time == "Yearly") "tooltip3" else "tooltip5"
      
      # Create Yield plot
      p2 <- ggplot(data_to_plot, aes(x = !!sym(x_var), y = Quantity, fill = Vegetable, text = !!sym(Quantity_tooltip))) +
        geom_bar(stat = "identity") +
        scale_x_discrete(drop = FALSE) +
        labs(title = "Harvest shown in Yield", y = "Yield", x = NULL) +
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
    
    # === CASE 3: User selects only Yield OR Revenue
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
