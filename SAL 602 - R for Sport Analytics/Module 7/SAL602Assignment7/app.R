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
library(ggrepel)
library(readr)

##masterID csv
june2023 <- read_csv("june2023.csv")
masterID <- read_csv("masterid.csv")

##eliminates blocked balls and creates swing variable
june2023 %>%
  filter(description != "blocked_ball") %>%
  mutate(swing = case_when(
    type == "S" & description == "called_strike" ~ FALSE,
    type == "X" | type == "S" ~ TRUE,
    type == "B" ~ FALSE))-> mydata

mydata %>%
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
  titlePanel("Swing Rates - June 2023"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "types",
                         label = h3("Types of Outcomes"),
                         choices = list("S" = "S",
                                        "B" = "B",
                                        "X" = "X"),
                         selected = c("S", "B", "X"))
      ),
      selectInput(inputId = "selectBatter",
                  label = h3("Batter"),
                  choices = possibleBattersList,
                  selected = 646240) ##Rafael Devers
    ),
    mainPanel(
      plotOutput(outputId = "swingRateChart")
    )
  )

server <- function(input, output) {
  SelectedBatterReactive <- reactive({
    mydata %>% filter(batter == input$selectBatter)
  })
  
  output$swingRateChart <- renderPlot({
    SelectedBatterReactive() %>%
      filter(type %in% input$types) -> SelectedPitchResults
    
    zones <- SelectedPitchResults %>%
      group_by(zone) %>%
      summarize(
        N = n(),
        right_edge = min(1.5, max(plate_x)),
        left_edge = max(-1.5, min(plate_x)),
        top_edge = min(5, quantile(plate_z, 0.95, na.rm = TRUE)),
        bottom_edge = max(0, quantile(plate_z, 0.05, na.rm = TRUE)),
        swing_pct = sum(swing == TRUE) / n(),
        plate_x = mean(plate_x),
        plate_z = mean(plate_z))
    
    k_zone_plot %+% zones +
      geom_rect(aes(xmax = right_edge, xmin = left_edge,
                    ymax = top_edge, ymin = bottom_edge,
                    fill = swing_pct, alpha = swing_pct),
                color = "darkgreen") +
      geom_text_repel(size = 3, aes(label = round(swing_pct, 2),
                                    color = swing_pct < 0.5)) +
      scale_fill_gradient(low = "blue", high = "red") +
      scale_color_manual(values = c("white", "black")) +
      guides(color = FALSE, alpha = FALSE) +
      ggtitle("Swing Rates by Zone")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
