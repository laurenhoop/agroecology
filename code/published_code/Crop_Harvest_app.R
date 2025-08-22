
# ===== loading libraries =====
library(shiny)
library(shinyBS)
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
library(lubridate)
library(googlesheets4)
library(viridis)

# ===== loading Google sheet =====
gs4_deauth()
Master_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1st3bYMIaT2TEjFxW_pe2pP1_yltTycQM0Si1yT2bcyo/edit?usp=sharing")

# ===== cleaning data =====
Master_sheet_clean <- Master_sheet |>
  filter(!is.na(Date), !is.na(Quantity), !is.na(Cost)) |>
  mutate(
    #removing dollar sign
    Cost = as.numeric(gsub("\\$", "", Cost)),
    #changing variable type of date variable
    Year = year(Date),
    Monthly = paste(month(Date, label = TRUE, abbr = TRUE), Year),
    # ensures xaxis is chronological
    Monthly = factor(Monthly, levels = unique(Monthly[order(Date)]))
  ) |>
  mutate(
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
  
  group_by(Year, Monthly, Vegetable) |>
  # adding total cost + quantity per vegetable per monthly period
  mutate(Monthly_Cost = sum(Cost, na.rm = TRUE),
         Monthly_Quantity = sum(Quantity, na.rm = TRUE)) |>
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
  tags$div(
    "Crop Harvest App",
    style = "text-align: center; background-color: #95c2de; color: black;
            padding: 10px 0; border-radius: 5px; font-weight: bold; font-size: 3rem;
            margin-bottom: 20px;"
  ),
  
  # Adds a border around plot output called "highlighted card"
  tags$style(HTML("
  .highlighted-card {
    border: 2px solid #444;
    border-radius: 8px;
    box-shadow: 2px 2px 6px rgba(0,0,0,0.1);
  }
")),
  
  # Adds a "class" of a colored background that can be added to each card/user input below
  tags$style(HTML("
  .colored-background-card {
    background-color: #d1e5f0;  /* light sage green background */
    border-radius: 8px;
    color: black;
    padding: 15px;
    box-shadow: 2px 2px 8px rgba(0,0,0,0.1);
    margin-bottom: 15px;  /* space below each card */
  }
")),
  
  # === Defines first row of app ===
  layout_column_wrap(
    # == user input 1 ==
    card(class = "colored-background-card",
         pickerInput("year",
                     tags$div("Choose a Harvest Year", style = "font-size: 18px; font-weight: bold;"),
                     choices = sort(unique(Master_sheet_clean$Year)),
                     selected = min(Master_sheet_clean$Year),
                     multiple = TRUE,
                     options = list('actions-box' = TRUE,
                                    'live-search' = TRUE))
    ),
    # == user input 2 ==
    card(class = "colored-background-card",
         conditionalPanel(condition = "input.select_type == 'Crop Family'", uiOutput("family_ui")),
         conditionalPanel(condition = "input.select_type == 'Individual Crop'", uiOutput("crop_ui")),
         radioButtons("select_type", "Select type:", choices = c("Crop Family", "Individual Crop")),
         verbatimTextOutput("selection"),
         
         # creating text that will be displayed in help icon
         bsTooltip(
           "family_help",
           "Crop Family graphic can be found on _____: ",
           placement = "right",
           trigger = "hover"
         )
    ),
    # == user input 3 ==
    card(class = "colored-background-card",
         pickerInput(
           "unit",
           label = tags$div(
             "Choose a Harvest Unit", style = "font-size: 18px; font-weight: bold;",
             # creating help icon
             tags$span(
               id = "unit_help",
               style = "color: #007BFF; margin-left: 5px; cursor: help; font-weight: bold;",
               "\u2753" # Unicode character for question mark in bubble
             )
           ),
           choices = c("Weight (lbs)", "Revenue", "Both"),
         ),
         conditionalPanel(
           condition = "input.unit == 'Both' && input.time == 'Yearly'",
           radioButtons(
             "side_stack",
             "Display Mode:",
             choices = c("Side by Side", "Stacked"),
             selected = "Side by Side"
           )
         ),
         # creating text that will be displayed in help icon
         bsTooltip(
           "unit_help",
           "Revenue is calculated using average US unit prices found on the following websites: ",
           placement = "right",
           trigger = "hover"
         )
    ),
    # == user input 4 ==
    card(class = "colored-background-card",
         pickerInput("time",
                     tags$div( "Choose a Timescale", style = "font-size: 18px; font-weight: bold;"),
                     choices = c("Monthly", "Bimonthly", "Yearly"))
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
                label = tags$div(
                  "Choose a Family", style = "font-size: 18px; font-weight: bold;",
                  # creating help icon
                  tags$span(
                    id = "family_help",
                    style = "color: #007BFF; margin-left: 5px; cursor: help; font-weight: bold;",
                    "\u2753" # Unicode character for question mark in bubble
                  )
                ),
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
                tags$div("Choose Crop:",  style = "font-size: 18px; font-weight: bold;"),
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
    
    # creates a new monthly variable that filters out years that are not selected
    monthly_levels <- Master_sheet_clean |>
      filter(Year %in% input$year) |>
      distinct(Monthly) |>
      pull(Monthly)
    
    # Selecting columns
    x_var <- if (input$time == "Bimonthly") "Bimonthly" else if (input$time == "Monthly") "Monthly" else "Vegetable"
    y_var <- if (input$unit == "Weight (lbs)") "Quantity" else "Cost"
    
    # filters plotted observations based on year(s) selected
    req(input$year)
    data_to_plot <- Master_sheet_clean |>
      filter(
        Year %in% input$year,
        if (input$select_type == "Crop Family") Family %in% input$family else TRUE,
        if (input$select_type == "Individual Crop") Vegetable %in% input$crop else TRUE
      ) 
    
    # further builds off above filter (monthly/bimonthly_levels) to ensure only selected dates are plotted on axis
    if (input$time == "Bimonthly") {
      data_to_plot$Bimonthly <- factor(data_to_plot$Bimonthly, levels = bimonthly_levels)
    } else if (input$time == "Monthly") {
      data_to_plot$Monthly <- factor(data_to_plot$Monthly, levels = monthly_levels)
    }
    
    # makes it to where plot labels show bar totals instead of individual observations    
    group_vars <- c("Year", x_var)
    
    if (x_var != "Vegetable") {
      group_vars <- c(group_vars, "Vegetable")
    }
    
    group_vars <- c(group_vars, "Family")
    
    data_to_plot <- data_to_plot |>
      group_by(across(all_of(group_vars))) |>
      summarise(
        Quantity = round(sum(Quantity, na.rm = TRUE), 2),
        Cost = round(sum(Cost, na.rm = TRUE), 2),
        Year_Quantity = round(max(Year_Quantity, na.rm = TRUE), 2),
        Year_Cost = round(max(Year_Cost, na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Creating plot labels + mutating parent Bimonthly variable to only contain filtered values from above
    data_to_plot <- data_to_plot |>
      mutate(
        tooltip = case_when(
          input$unit == "Weight (lbs)" ~ paste0(
            "Year: ", Year,
            "<br>Vegetable: ", Vegetable,
            "<br>Family: ", Family,
            "<br>Total Weight (lbs): ", Quantity
          ),
          
          input$unit == "Revenue" ~ paste0(
            "Year: ", Year,
            "<br>Vegetable: ", Vegetable,
            "<br>Family: ", Family,
            "<br>Total Revenue: $", Cost
          ),
          
          input$unit == "Both" ~ paste0(
            "Year: ", Year,
            "<br>Vegetable: ", Vegetable,
            "<br>Family: ", Family,
            "<br>Total Weight (lbs): ", Quantity,
            "<br>Total Revenue: $", Cost
          )
        )
      )
    
    # ============ CASE 1: User selects "Both" + Side by Side ============
    if (input$unit == "Both" && input$time == "Yearly" && input$side_stack == "Side by Side") {
      
      data_long <- data_to_plot |>
        select(Year, Vegetable, Family, Year_Quantity, Year_Cost, tooltip) |>
        pivot_longer(
          cols = c(Year_Quantity, Year_Cost),
          names_to = "Metric",
          values_to = "Value"
        ) |>
        mutate(
          Metric = recode(Metric, Year_Quantity = "Weight (lbs)", Year_Cost = "Revenue"),
          tooltip = paste0(
            "Year: ", Year,
            "<br>Vegetable: ", Vegetable,
            "<br>Family: ", Family,
            "<br>Total ", Metric, ": ", Value
          ))
      
      p <- ggplot(data_long, aes(x = Vegetable, y = Value, fill = Metric, text = tooltip)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
        scale_x_discrete(drop = FALSE) +
        labs(title = "Harvest by Weight (lbs) and Revenue",
             x = NULL, y = "Value", fill = "Metric") +
        scale_fill_manual(values = c("Revenue" = "#b63679",
                                     "Weight (lbs)" = "#f98e09")) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 15),
        ) 
      
      p <- ggplotly(p, tooltip = "text") |>
        layout(
          legend = list(
            orientation = "h",         # horizontal legend
            x = 0.5, xanchor = "center",
            y = -0.2,                  # below the plot area
            tracegroupgap = 3, 
            font = list(size = 16),    # legend item font size
            title = list(              # legend title styling
              font = list(size = 18))
          ),
          margin = list(b = 120),
          xaxis = list(automargin = TRUE))
      return(p)
    }
    
    # ============ CASE 2: User selects "Both" + Stacked ============
    if (input$unit == "Both" && (!input$time == "Yearly" || input$side_stack == "Stacked")) {
      
      # Create Weight (lbs) plot
      p2 <- ggplot(data_to_plot, aes(x = !!sym(x_var), y = Quantity, fill = Vegetable, text = tooltip)) +
        geom_bar(stat = "identity") +
        scale_x_discrete(drop = FALSE) +
        labs(y = "Weight (lbs)", x = NULL) +
        scale_fill_viridis_d(option = "D") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 15))
      
      # Create Revenue plot
      p3 <- ggplot(data_to_plot, aes(x = !!sym(x_var), y = Cost, fill = Vegetable, text = tooltip)) +
        geom_bar(stat = "identity") +
        scale_x_discrete(drop = FALSE) +
        labs(title = "Harvest by Weight (lbs) and Revenue", y = "Revenue ($)", x = NULL) +
        scale_fill_viridis_d(option = "D") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 15))
      
      p2_ly <- ggplotly(p2, tooltip = "text") |>
        layout(
          legend = list(
            orientation = "h",         # horizontal legend
            x = 0.5, xanchor = "center",
            y = -0.2,                  # below the plot area
            tracegroupgap = 3, 
            font = list(size = 16),    # legend item font size
            title = list(              # legend title styling
              font = list(size = 18)
            )
          ),
          margin = list(b = 90),        # make room for the bottom legend
          xaxis = list(automargin = TRUE)
          )
      
      p3_ly <- ggplotly(p3, tooltip = "text") |>
        style(showlegend = FALSE)
      
      return(subplot(p2_ly, p3_ly, nrows = 2, shareX = TRUE, titleY = TRUE))
    }
    
    # ============ CASE 3: User selects only Weight (lbs) OR Revenue ============
    if (input$unit != "Both") {
      p4 <- ggplot(data_to_plot, aes(x = !!sym(x_var), y = !!sym(y_var), fill = Vegetable, text = tooltip)) +
        geom_bar(stat = "identity") +
        scale_x_discrete(drop = FALSE) +
        labs(title = paste(input$unit, "Harvested"), x = NULL, y = input$unit) +
        scale_fill_viridis_d(option = "plasma") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 15))
      
      
      p4 <- ggplotly(p4, tooltip = "text") |>
        layout(
          legend = list(
            orientation = "h",         # horizontal legend
            x = 0.5, xanchor = "center",
            y = -0.2,                  # below the plot area
            tracegroupgap = 3, 
            font = list(size = 16),    # legend item font size
            title = list(              # legend title styling
              font = list(size = 18)
            )
          ),
          margin = list(b = 150),        # make room for the bottom legend
          xaxis = list(automargin = TRUE)
          )
      
      return(p4)
    }
  })}

# Run the application 
shinyApp(ui = ui, server = server)
