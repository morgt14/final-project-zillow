library("ggplot2")
library("shiny")
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")
library("plotly")
library("tidyverse")

# Read in data from bedroom datasets and store in variables
one <- read.csv("../data/State_Zhvi_1bedroom.csv")
two <- read.csv("../data/State_Zhvi_2bedroom.csv")
three <- read.csv("../data/State_Zhvi_3bedroom.csv")
four <- read.csv("../data/State_Zhvi_4bedroom.csv")
five_or_more <- read.csv("../data/State_Zhvi_5bedroomOrMore.csv")
days <- read.csv("../data/DaysOnZillow_State_final.csv", stringsAsFactors = FALSE)
cut <- read.csv("../data/State_MedianPctOfPriceReduction_AllHomes_USE.csv",
                stringsAsFactors = FALSE)


# Define a UI using a `fluidPage()` layout with the following content:
ui <- fluidPage(

  # A `titlePanel` with the title "Diamond Viewer"
  titlePanel("zillow home price"),

  sidebarLayout(
    sidebarPanel(
    sliderInput("date_range",
                "Choose Date Range:",
                min = as.Date("1996-04-01"),
                max = as.Date("2019-01-01"),
                value = c(as.Date("1996-04-01"), as.Date("2019-01-01"))
    ),

  # A `selectInput()` labeled "select your state". This dropdown should let
  # the user pick one of the states.
  selectInput("state",
              label = "Select your state",
              choices = select_values,
              selected = "Washington"
  ),

  # A `checkboxInput()` labeled "compare". It's default value is FALSE
  checkboxInput("compare", label = strong("Do you want to compare with another state?"), value = FALSE),

  # A `selectInput()` labeled "select another state". This dropdown should let
  # the user pick one of the state.
  selectInput("state_two",
              label = "Select another state",
              choices = select_values)
    ),
  mainPanel(

  # A plotOutput showing the 'plot' output (based on the user specifications)
  plotOutput("plot")
  )

)
)

# Define a `server` function (with appropriate arguments)
# This function should perform the following:
server <- function(input, output){
  gathered_one <- reactive({
    data <- one %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "1")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))

    data #return data
  })
  gathered_two <- reactive({
    data <- two %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "2")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data #return data
  })
  gathered_three <- reactive({
    data <- three %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "3")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data #return data
  })
  gathered_four <- reactive({
    data <- four %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "4")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data #return data
  })
  gathered_fiveplus <- reactive({
    data <- five_or_more %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "5+")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data #return data
  })

  data_combined <- reactive({
    data <- rbind(gathered_one(), gathered_two(), gathered_three(),
                         gathered_four(), gathered_fiveplus())
    data
  })

  another_one <- reactive({
    data <- one %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "compared 1")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data #return data
  })
  another_two <- reactive({
    data <- two %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "compared 2")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data #return data
  })
  another_three <- reactive({
    data <- three %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "compared 3")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data #return data
  })
  another_four <- reactive({
    data <- four %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "compared 4")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data #return data
  })
  another_fiveplus <- reactive({
    data <- five_or_more %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "compared 5+")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data #return data
  })
  # Manipulate data to use in inputs and outputs
  select_values <- sort(one$RegionName)
  all_data <- left_join(cut, days, by = "state")
  x_axis_lbl <- list(
    title = "Average Days Listed on Zillow/the market"
  )
  y_axis_lbl <- list(
    title = "Median Price Cut (%) During Time on Market"
  )

  data_combined_compared <- reactive({
    data <- rbind(another_one(), another_two(), another_three(),
                  another_four(), another_fiveplus())
    data
  })
  # Assign a reactive `renderPlot()` function to the outputted 'plot' value
  output$plot <- renderPlot({

    # This function should take the `diamonds_sample` data set and filter it by
    # the input price (remember to get both ends)!

    # Use the filtered data set to create a ggplot2 scatter plot with the carat
    # on the x-axis, the price on the y-axis, and color based on the clarity.
    # Facet the plot based on which feature the user selected to "facet by"
    #   (hint: you can just pass that string directly to `facet_wrap()`)
    # Save your plot as a variable.
    p <- ggplot(data = data_combined()) +
      geom_point(mapping = aes(x = year_month, y = price, color = bedroom)) +
      scale_color_brewer()
    # Finally, if the "trendline" checkbox is selected, you should also include
    # a geom_smooth geometry (with `se=FALSE`)
    # Hint: use an if statement to see if you need to add more geoms to the plot
    # Be sure and return the completed plot!
    if (input$compare) {
      p <- p + geom_point(data = data_combined_compared(), mapping = aes(x = year_month, y = price), alpha = 0.1)
    }

    p # return the plot
  })
  output$chart <- renderPlotly({
    chart_1 <- plot_ly(data = all_data, x = ~daysavg2018, y = ~cutavg2018,
                      text = ~state, type = "scatter" , mode = "markers",
                      name = "2018") %>%
      layout(title = "House's Time on Market and Price Cuts",
             xaxis = x_axis_lbl,
             yaxis = y_axis_lbl)
    if (input$show_2017) {
      chart_1 <- chart_1 %>%
        add_trace(x = ~daysavg2017, y = ~cutavg2017, name = "2017")
    }
    if(input$show_2016) {
      chart_1 <- chart_1 %>%
        add_trace(x = ~daysavg2016, y = ~cutavg2016, name = "2016")
    }
    chart_1
  })
}
