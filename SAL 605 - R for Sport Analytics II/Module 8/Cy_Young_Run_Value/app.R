library(tidyverse)
library(Lahman)
library(readr)
library(ggplot2)
library(ggthemes)
library(shiny)

##reading files in for ShinyApp
control_data <- read_csv('data/control_data.csv')
cy_data <- read_csv('data/cy_data.csv')
##acting as possible pitchers and years with corresponding retroIDs
cy_young_lookup <- read_csv('data/cy_young_lookup.csv')

##creating possible pitchers combined with retroIDs and player names
possiblePitchers <- unique(cy_young_lookup$retroID)
names(possiblePitchers) <- unique(cy_young_lookup$Player)

ui <- fluidPage(
  ##title of ui
  titlePanel('Cy Young Winners Run Value by Game State - 2010-2019'),
  sidebarLayout(
    ##selections for player
    sidebarPanel(position = 'left',
                 selectInput(inputId = 'selectPitcher',
                             label = h3('Pitcher'),
                             choices = possiblePitchers,
                             ##starting with Roy Halladay
                             selected = 'hallr001')
    ),
    ##selections for year
    sidebarPanel(position = 'left',
                 selectInput(inputId = 'years',
                             label = h3('Year'),
                             ##NULL choices because reacting to player selection
                             ##code located in the server
                             choices = NULL)
    )
  ),
  ##plots
  mainPanel(position = 'right',
            ##cy young plot
            plotOutput(outputId = 'cy_plot'),
            ##control plot
            plotOutput(outputId = 'control_plot'))
)

server <- function(input, output, session){
  
  ##reactive selection for cy young chart using cy_data
  SelectedPitcherReactive.CY <- reactive({
    cy_data %>% filter(PIT_ID == input$selectPitcher)
  })
  
  ##reactive selection for control chart using control_data
  SelectedPitcherReactive.CONTROL <- reactive({
    control_data %>% filter(PIT_ID != input$selectPitcher)
  })
  
  ##code for year input based on the player input
  ##long if/else statement going through all possible options
  observeEvent(input$selectPitcher, {
    if (input$selectPitcher == 'hernf002') {
      possibleYears <- 2010
    } else if (input$selectPitcher == 'hallr001'){
      possibleYears <- 2010
    } else if (input$selectPitcher == 'verlj001') {
      possibleYears <- c(2011, 2019)
    } else if (input$selectPitcher == 'kersc001') {
      possibleYears <- c(2011, 2013, 2014)
    } else if (input$selectPitcher == 'pricd001'){
      possibleYears <- 2012
    } else if (input$selectPitcher == 'dickr001'){
      possibleYears <- 2012
    } else if (input$selectPitcher == 'schem001'){
      possibleYears <- c(2013, 2016, 2017)
    } else if (input$selectPitcher == 'klubc001'){
      possibleYears <- c(2014, 2017)
    } else if (input$selectPitcher == 'keucd001'){
      possibleYears <- 2015
    } else if (input$selectPitcher == 'arrij001'){
      possibleYears <- 2015
    } else if (input$selectPitcher == 'porcr001'){
      possibleYears <- 2016
    } else if (input$selectPitcher == 'snelb001'){
      possibleYears <- 2018
    } else if (input$selectPitcher == 'degrj001'){
      possibleYears <- c(2018, 2019)
    } else {
      possibleYears <- NULL
    }
    updateSelectInput(session, 'years', choices = possibleYears)
  })
  
  output$cy_plot <- renderPlot({
    ##filtering for the correct year
    selectedYears <- SelectedPitcherReactive.CY() %>%
      filter(YEAR_ID == input$years)
    
    ggplot(selectedYears, aes(BASES, run_value)) +
      geom_jitter(width = 0.25, alpha = 0.5, color = 'midnightblue') +
      ##horizontal line for y = 0
      geom_hline(yintercept = 0, color = 'springgreen2', size = 1.5) +
      labs(title = 'Cy Young Winner Run Value',
           x = 'Base State',
           y = 'Run Value',
           caption = 'Data from Retrosheet') +
      theme_fivethirtyeight()
  })
  
  output$control_plot <- renderPlot({
    ##filtering for correct year
    set.seed(201019) ##years examining
    selectedData <- SelectedPitcherReactive.CONTROL() %>% 
      filter(YEAR_ID == input$years) %>% 
      ##10% sample for year to compare
      slice_sample(prop = 0.1)
    
    ggplot(selectedData, aes(BASES, run_value)) +
      geom_jitter(width = 0.25, alpha = 0.5, color = 'orange2') +
      ##horizontal line for y = 0
      geom_hline(yintercept = 0, color = 'springgreen2', size = 1.5) +
      labs(title = 'Control Run Value',
           x = 'Base State',
           y = 'Run Value',
           caption = 'Data from Retrosheet') +
      theme_fivethirtyeight()
  })
  
}

shinyApp(ui = ui, server = server)