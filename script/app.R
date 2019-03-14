# Load the libraries
library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("lubridate")
library("stringr")

# load csv file 
one <- read.csv("../data/State_Zhvi_1bedroom.csv")
two <- read.csv("../data/State_Zhvi_2bedroom.csv") 
three <- read.csv("../data/State_Zhvi_3bedroom.csv") 
four <- read.csv("../data/State_Zhvi_4bedroom.csv") 
five_or_more <- read.csv("../data/State_Zhvi_5bedroomOrMore.csv") 

# sort region name and store 
select_values <- sort(one$RegionName)


# Define a UI using a `fluidPage()` layout with the following content:
ui <- fluidPage(
  
  # title
  titlePanel("zillow home price"),
  
  sidebarLayout(
    sidebarPanel(
      # make a slider input "data range"
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
  # data wrangling
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
  
  # data combining
  data_combined <- reactive({
    data <- rbind(gathered_one(), gathered_two(), gathered_three(),
                         gathered_four(), gathered_fiveplus())
    data
  })
  
  # data wrangling
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
  
  # data combining
  data_combined_compared <- reactive({
    data <- rbind(another_one(), another_two(), another_three(),
                  another_four(), another_fiveplus())
    data
  })
  # Assign a reactive `renderPlot()` function to the outputted 'plot' value
  output$plot <- renderPlot({
    
    # make a plot
    p <- ggplot(data = data_combined()) +
      geom_point(mapping = aes(x = year_month, y = price, color = bedroom)) + 
      scale_color_brewer()
    # Finally, if the "compare" checkbox is selected, adding another graph
    if (input$compare) {
      p <- p + geom_point(data = data_combined_compared(), mapping = aes(x = year_month, y = price), alpha = 0.1) 
    }
    p # return the plot
  })
}


# Create a new `shinyApp()` using the above ui and server
shinyApp(ui = ui, server = server)
