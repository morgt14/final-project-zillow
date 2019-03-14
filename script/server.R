library("ggplot2")
library("shiny")
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")
library("plotly")
library("tidyverse")
library("rbokeh")
library("maps")

# Read in data from bedroom datasets and store in variables
one <- read.csv("../data/State_Zhvi_1bedroom.csv")
two <- read.csv("../data/State_Zhvi_2bedroom.csv")
three <- read.csv("../data/State_Zhvi_3bedroom.csv")
four <- read.csv("../data/State_Zhvi_4bedroom.csv")
five_or_more <- read.csv("../data/State_Zhvi_5bedroomOrMore.csv")
days <- read.csv("../data/DaysOnZillow_State_final.csv", stringsAsFactors = FALSE)
cut <- read.csv("../data/State_MedianPctOfPriceReduction_AllHomes_USE.csv",
                stringsAsFactors = FALSE)
state_price <- read.csv("../data/Sale_Prices_State.csv", header = TRUE, stringsAsFactors = FALSE)
coordinates <- read.csv("../data/coordinates.csv", header = TRUE, stringsAsFactor = FALSE) %>% 
  select(Latitude, Longitude, RegionName)

# Manipulate data to use in inputs and outputs
all_data <- left_join(cut, days, by = "state")
x_axis_lbl <- list(
  title = "Average Days Listed on Zillow/the market"
)
y_axis_lbl <- list(
  title = "Median Price Cut (%) During Time on Market"
)
select_values <- sort(one$RegionName)
colnames <- names(state_price)
mean_2016 <- apply(state_price[, grepl("2016", colnames)], 1, mean, na.rm = TRUE)
mean_2017 <- apply(state_price[, grepl("2017", colnames)], 1, mean, na.rm = TRUE)
mean_2018 <- apply(state_price[, grepl("2018", colnames)], 1, mean, na.rm = TRUE)
state_price <- data.frame(RegionName = state_price$RegionName, 
                          mean_2016 = mean_2016, 
                          mean_2017 = mean_2017, 
                          mean_2018 = mean_2018) %>% 
  gather(Year, Avarage_Price, -RegionName)
state_price$Year <- gsub("mean_", "", state_price$Year)
mapDat <- left_join(state_price, coordinates, by = "RegionName")
color <- mapDat %>% group_by(RegionName) %>% summarise(Price = mean(Avarage_Price)) %>% as.data.frame()

# Create a `server` function
server <- function(input, output){
  gathered_one <- reactive({
    data <- one %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "1")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data
  })
  gathered_two <- reactive({
    data <- two %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "2")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data
  })
  gathered_three <- reactive({
    data <- three %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "3")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data
  })
  gathered_four <- reactive({
    data <- four %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "4")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data
  })
  gathered_fiveplus <- reactive({
    data <- five_or_more %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "5+")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data
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
    data
  })
  another_two <- reactive({
    data <- two %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "compared 2")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data
  })
  another_three <- reactive({
    data <- three %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "compared 3")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data
  })
  another_four <- reactive({
    data <- four %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "compared 4")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data
  })
  another_fiveplus <- reactive({
    data <- five_or_more %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price) %>%
      mutate(bedroom = "compared 5+")
    data$year_month <- as.Date(parse_date_time(gsub("[.]", "-", str_sub(data$year_month, start = 2)), "Y-m"))
    data <- filter(data, year_month >= as.Date(input$date_range[1]), year_month <= as.Date(input$date_range[2]))
    data
  })
  

  data_combined_compared <- reactive({
    data <- rbind(another_one(), another_two(), another_three(),
                  another_four(), another_fiveplus())
    data
  })
  output$map <- renderRbokeh({
    m <- suppressWarnings(figure(width = 800, height = 500, padding_factor = 0, legend_location = "top_left") %>%
                            ly_map("state") %>%
                            ly_points(
                              Longitude, 
                              Latitude, 
                              data = mapDat, 
                              size = 5,
                              color = Avarage_Price, 
                              hover = c(RegionName, Year, Avarage_Price)))
    m
  })
  # Assign a reactive `renderPlot()` function to the outputted 'plot' value
  output$plot <- renderPlot({
    p <- ggplot(data = data_combined()) +
      geom_point(mapping = aes(x = year_month, y = price, color = bedroom)) +
      scale_color_brewer()
    # Add an if statement that plots data for another state if the
    # `checkboxInput` is checked
    if (input$compare) {
      p <- p + geom_point(data = data_combined_compared(), mapping = aes(x = year_month, y = price), alpha = 0.1)
    }

    p
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
