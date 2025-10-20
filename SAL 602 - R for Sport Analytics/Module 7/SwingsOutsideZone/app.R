 #
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(readr)

mydata <- read_csv("lab1data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Swings on Pitches Outside the Zone"),
  sidebarLayout(
    sliderInput(inputId = "bins",
                label = "Number of bins:",
                min = 1,
                max = 200,
                value = 50),
    mainPanel(
      plotOutput(outputId = "swingPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$swingPlot <- renderPlot({
    ggplot(mydata, aes(x = distanceFromCenter, colour = Swing)) +
      geom_histogram(bins = input$bins) +
      ggtitle("Histogram of Swings Outside the Zone") +
      xlab("Feet From Center of Zone") + ylab("n")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
