library(tidyverse)
library(plotly)
library(shiny)

# Create ui for Price Reduction tab, will be plugged into main ui
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxInput(
        "show_2017",
        label = "Include 2017 data",
        value = FALSE
      ),
      checkboxInput(
        "show_2016",
        label = "Include 2016 data",
        value = FALSE
      ),
      numericInput(
        "point_size",
        label = "Point size",
        value = 10
      )
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Create a server for Price Reduction tab, will be plugged into main server
server <- function(input, output) {
  days <- read.csv("../data/DaysOnZillow_State_final.csv",
                   stringsAsFactors = FALSE)
  cut <- read.csv("../data/State_MedianPctOfPriceReduction_AllHomes_USE.csv",
    stringsAsFactors = FALSE
  )

  all_data <- left_join(cut, days, by = "state")

  x_axis_lbl <- list(
    title = "Average Days Listed on Zillow/the market"
  )

  y_axis_lbl <- list(
    title = "Median Price Cut (%) During Time on Market"
  )

  output$plot <- renderPlotly({
    plot_1 <- plot_ly(
      data = all_data, x = ~daysavg2018, y = ~cutavg2018,
      text = ~state, type = "scatter",
      name = "2018", marker = list(size = input$point_size)
    ) %>%
      layout(
        title = "House's Time on Market and Price Cuts",
        xaxis = x_axis_lbl,
        yaxis = y_axis_lbl
      )
    if (input$show_2017) {
      plot_1 <- plot_1 %>%
        add_trace(x = ~daysavg2017, y = ~cutavg2017, name = "2017")
    }
    if (input$show_2016) {
      plot_1 <- plot_1 %>%
        add_trace(x = ~daysavg2016, y = ~cutavg2016, name = "2016")
    }
    plot_1
  })
}

shinyApp(ui, server)
