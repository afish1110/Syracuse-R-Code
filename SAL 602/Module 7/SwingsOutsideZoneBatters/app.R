#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(tidyverse)

masterID <- read_csv("masterid.csv")
lab1data <- read_csv("lab1data.csv")

lab1data %>%
  group_by(batter) %>%
  summarise() %>%
  inner_join(masterID, by = c("batter" = "mlb_id")) %>%
  select(batter, mlb_name) %>%
  arrange(mlb_name) -> possibleBatters

possibleBattersList <- possibleBatters$batter
names(possibleBattersList) <- possibleBatters$mlb_name

##creating strike zone
plate_width <- 17 + (9 / pi)
k_zone_plot <- ggplot(NULL, aes(x = plate_x, plate_z)) +
  geom_rect(xmin = -(plate_width / 2) / 12,
            xmax = (plate_width / 2) / 12,
            ymin = 1.5,
            ymax = 3.6, color = "black", alpha = 0) +
  coord_equal() +
  scale_x_continuous("Horizontal Location (ft.)",
                     limits = c(-2, 2)) +
  scale_y_continuous("Vertical Location (ft.)",
                     limits = c(0, 5))


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
  ),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "zones",
                         label = h3("Zones of Pitches"),
                         choices = list("11" = 11,
                                        "12" = 12,
                                        "13" = 13,
                                        "14" = 14),
                         selected = c(11, 12, 13, 14)),
      sidebarPanel(
        checkboxGroupInput(inputId = "swing",
                           label = h3("Batter Swing"),
                           choices = list("Yes" = TRUE,
                                          "No" = FALSE),
                           selected = TRUE)
      ),
      selectInput(inputId = "selectBatter",
                  label = h3("Select box"),
                  choices = possibleBattersList,
                  selected = 605141) ##Mookie Betts
    ),
    mainPanel(
      plotOutput(outputId = "batterChart")
    )
  )
)


server <- function(input, output) {
  SelectedBatterReactive <- reactive({
    lab1data %>% filter(batter == input$selectBatter)
  })
  
  output$batterChart <- renderPlot({
    SelectedBatterReactive() %>%
      filter(zone %in% input$zones,
             Swing %in% input$swing) -> SelectedPitchResults
    
    k_zone_plot %+% SelectedPitchResults +
      aes(color = pitch_name, shape = type) +
      geom_point(size = 3) + ggtitle("Pitch Chart")})
  
  output$swingPlot <- renderPlot({
    lab2data <- SelectedBatterReactive() %>%
      filter(zone %in% input$zones,
             Swing %in% input$swing)
    
    ggplot(lab2data, aes(x = distanceFromCenter, colour = Swing)) +
      geom_histogram(bins = input$bins) +
      ggtitle("Histogram of Swings Outside the Zone") +
      xlab("Feet From Center of Zone") + ylab("n")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
