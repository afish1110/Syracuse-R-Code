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
library(ggrepel)

mvpWinners <- read_csv("mvpWinners.csv")
masterID <- read_csv("masterid.csv")

##want to have a column for exit velo now only have x,y,z components
##formula taken from fangraphs to find exit velo from components
mvpWinners %>%
  mutate(x2 = vx0 ^ 2,
         y2 = vy0 ^ 2,
         z2 = vz0 ^ 2,
         exit_velo = sqrt(x2 + y2 + z2) * 0.6818) -> mvpWinners

##possible batters for drop down menu
mvpWinners %>%
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

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Exit Velocity of MVP Winners - 2015-2024 (Excluding 2020)"),
  sidebarLayout(
    sidebarPanel(position = "left",
                  selectInput(inputId = "selectBatter",
                              label = h3("Batter"),
                              choices = possibleBattersList,
                              selected = 660271)
    ),
    sidebarPanel(position = "left",
      checkboxGroupInput(inputId = "years",
                         label = "Year",
                         choices = list("2015: Donaldson, Harper" = 2015,
                                        "2016: Trout, Bryant" = 2016,
                                        "2017: Altuve, Stanton" = 2017,
                                        "2018: Betts, Yelich" = 2018,
                                        "2019: Trout, Bellinger" = 2019,
                                        "2021: Ohtani, Harper" = 2021,
                                        "2022: Judge, Goldschmidt" = 2022,
                                        "2023: Ohtani, Acuña" = 2023,
                                        "2024: Judge, Ohtani" = 2024),
                         selected = 2024),
      helpText("To get image to show must have year selected to the year the player won MVP.
               You can have multiple years selected if player won multiple MVPs during timeframe.
               To help MVP Winners are labeled with each year.")
    )
  ),
  mainPanel(position = "right",
    plotOutput(outputId = "batterEV"),
    plotOutput(outputId = "exitveloHist")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  SelectedBatterReactive <- reactive({
    mvpWinners %>% filter(batter == input$selectBatter)
  })
  
  output$batterEV <- renderPlot({
    SelectedBatterReactive() %>%
      filter(game_year %in% input$years) -> selectedYears
    
    zones <- selectedYears %>%
      group_by(zone) %>%
      summarize(
        N = n(),
        right_edge = min(1.5, max(plate_x)),
        left_edge = max(-1.5, min(plate_x)),
        top_edge = min(5, quantile(plate_z, 0.95, na.rm = TRUE)),
        bottom_edge = max(0, quantile(plate_z, 0.05, na.rm = TRUE)),
        avg_ev = sum(exit_velo) / n(),
        plate_x = mean(plate_x),
        plate_z = mean(plate_z))
    
    k_zone_plot %+% zones +
      geom_rect(aes(xmax = right_edge, xmin = left_edge,
                    ymax = top_edge, ymin = bottom_edge,
                    fill = avg_ev, alpha = avg_ev),
                color = "darkgreen") +
      geom_text_repel(size = 4, aes(label = round(avg_ev, 2),
                                    color = avg_ev < 0.5)) +
      scale_fill_gradient(low = "blue", high = "red") +
      scale_color_manual(values = c("white", "black")) +
      guides(color = FALSE, alpha = FALSE) +
      ggtitle("Exit Velocity by Zone")
  })
  
  output$exitveloHist <- renderPlot({
    SelectedBatterReactive() %>%
      filter(game_year %in% input$years) -> selectedYears

    ggplot(selectedYears, aes(x = exit_velo, color = pitch_type)) +
      geom_histogram() + ggtitle("Exit Velocity Distribution") +
      xlab("Exit Velocity (MPH)") + ylab("n")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
