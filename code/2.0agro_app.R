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

# ===== Define UI for application that draws a histogram ===== 
ui <- page_fillable(
  theme = shinytheme("spacelab"),
  
  # Application title
  titlePanel("Agroecology App 2.0 Draft"),
  
  # === Defines first row of app ===
  layout_columns(
    # == user input 1 ==
    card(
          pickerInput("year",
                      "Choose a Harvest Year",
                      choices = c("2024", "2025"),
                      selected = "2024")
    ),
    
    # == user input 2 ==
    card(
          pickerInput("family",
                      "Choose a Crop Family",
                      choices = sort(unique(Harvest_clean$family)),
                      selected = "Alliaceae",
                      multiple = TRUE,
                      options = list(
                        'actions-box' = TRUE,
                        'live-search' = TRUE))
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
                      choices = c ("Pounds", "Revenue"))
    ),
    
    col_widths = c(3,3,3,3)
  ),
  
  # === Defines second row of app ===
  layout_columns(
    # == visualization output ==
    card(
      plotlyOutput("plot"))
    )
 
)

# ===== Define server logic required to draw a histogram ===== 
server <- function(input, output) {
  output$plot <- renderPlotly({
   
  })
}

# ===== Run the application ===== 
shinyApp(ui = ui, server = server)