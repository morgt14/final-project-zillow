# Load the shiny, ggplot2, and dplyr libraries
library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")

# You will once again be working with the `diamonds` data set provided by ggplot2
# Use dplyr's `sample_n()` function to get a random 3000 rows from the data set
# Store this sample in a variable `diamonds_sample`
diamonds_sample <- sample_n(diamonds, 1000)
one <- read.csv("../data/State_Zhvi_1bedroom.csv") 
two <- read.csv("../data/State_Zhvi_2bedroom.csv") 
three <- read.csv("../data/State_Zhvi_3bedroom.csv") 
four <- read.csv("../data/State_Zhvi_4bedroom.csv") 
five_or_more <- read.csv("../data/State_Zhvi_5bedroomOrMore.csv") 
# For convenience store the `range()` of values for the `price` column 
# (of the ENTIRE diamonds dataset)
price_range <- range(diamonds$price)

# For convenience, get a vector of column names from the `diamonds` data set to
# use as select inputs
select_values <- sort(one$RegionName)


# Define a UI using a `fluidPage()` layout with the following content:
ui <- fluidPage(
  
  # A `titlePanel` with the title "Diamond Viewer"
  titlePanel("zillow home price"),
  
  # A `selectInput()` labeled "select your state". This dropdown should let
  # the user pick one of the states. 
  selectInput("state",
              label = "Select your state",
              choices = select_values,
              selected = "Washington"
  ),
  
  # A `checkboxInput()` labeled "compare". It's default value is FALSE
  checkboxInput("compare", label = strong("Do you want to cpmpare with another state?"), value = FALSE),
  
  # A `selectInput()` labeled "select another state". This dropdown should let
  # the user pick one of the state. 
  selectInput("state_two",
              label = "Select another state",
              choices = select_values
  ),
  
  # A plotOutput showing the 'plot' output (based on the user specifications)
  plotOutput("plot")
  
)

# Define a `server` function (with appropriate arguments)
# This function should perform the following:
server <- function(input, output){
  gathered_one <- reactive({
    data <- one %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price)
    data$year_month <- str_sub(data$year_month, start = 2)
    #data$year_month <- gsub("[.]", "-", data$year_month)
    data #return data
  })
  gathered_two <- reactive({
    data <- two %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price)
    data$year_month <- str_sub(data$year_month, start = 2)
    #data$year_month <- gsub("[.]", "-", data$year_month)
    data #return data
  })
  gathered_three <- reactive({
    data <- three %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price)
    data$year_month <- str_sub(data$year_month, start = 2)
    #data$year_month <- gsub("[.]", "-", data$year_month)
    data #return data
  })
  gathered_four <- reactive({
    data <- four %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price)
    data$year_month <- str_sub(data$year_month, start = 2)
    #data$year_month <- gsub("[.]", "-", data$year_month)
    data #return data
  })
  gathered_fiveplus <- reactive({
    data <- five_or_more %>%
      filter(RegionName == input$state) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price)
    data$year_month <- str_sub(data$year_month, start = 2)
    #data$year_month <- gsub("[.]", "-", data$year_month)
    data #return data
  })
  
  another_one <- reactive({
    data <- one %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price)
    data$year_month <- str_sub(data$year_month, start = 2)
    #data$year_month <- gsub("[.]", "-", data$year_month)
    data #return data
  })
  another_two <- reactive({
    data <- two %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price)
    data$year_month <- str_sub(data$year_month, start = 2)
    #data$year_month <- gsub("[.]", "-", data$year_month)
    data #return data
  })
  another_three <- reactive({
    data <- three %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price)
    data$year_month <- str_sub(data$year_month, start = 2)
    #data$year_month <- gsub("[.]", "-", data$year_month)
    data #return data
  })
  another_four <- reactive({
    data <- four %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price)
    data$year_month <- str_sub(data$year_month, start = 2)
    #data$year_month <- gsub("[.]", "-", data$year_month)
    data #return data
  })
  another_fiveplus <- reactive({
    data <- five_or_more %>%
      filter(RegionName == input$state_two) %>%
      gather(key = "year_month", value = "price", -1, -2, -3) %>%
      select(year_month, price)
    data$year_month <- str_sub(data$year_month, start = 2)
    #data$year_month <- gsub("[.]", "-", data$year_month)
    data #return data
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
    p <- ggplot() +
      geom_point(data = gathered_one(), mapping = aes_string(x = "year_month", y = "price"), color = "cornflowerblue") +
      geom_point(data = gathered_two(), mapping = aes_string(x = "year_month", y = "price"), color = "turquoise2") +
      geom_point(data = gathered_three(), mapping = aes_string(x = "year_month", y = "price"), color = "turquoise3") +
      geom_point(data = gathered_four(), mapping = aes_string(x = "year_month", y = "price"), color = "turquoise4") +
      geom_point(data = gathered_fiveplus(), mapping = aes_string(x = "year_month", y = "price"), color = "forestgreen") 
      
    
    # Finally, if the "trendline" checkbox is selected, you should also include 
    # a geom_smooth geometry (with `se=FALSE`)
    # Hint: use an if statement to see if you need to add more geoms to the plot
    # Be sure and return the completed plot!
    if (input$compare) {
      p <- p +  geom_point(data = another_one(), mapping = aes_string(x = "year_month", y = "price"), color = "darkorchid1") +
        geom_point(data = another_two(), mapping = aes_string(x = "year_month", y = "price"), color = "darkorchid2") +
        geom_point(data = another_three(), mapping = aes_string(x = "year_month", y = "price"), color = "darkorchid3") +
        geom_point(data = another_four(), mapping = aes_string(x = "year_month", y = "price"), color = "darkorchid4") +
        geom_point(data = another_fiveplus(), mapping = aes_string(x = "year_month", y = "price"), color = "darkred") 
      
    }
    
    p # return the plot
  })
}


# Create a new `shinyApp()` using the above ui and server
shinyApp(ui = ui, server = server)
