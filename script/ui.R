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

# Create variable to use in `selectInput`
select_values <- sort(one$RegionName)

ui <- shinyUI(navbarPage(
  "Zillow Data",
  includeCSS("styles.css"),
  # A `tabPanel` for data about bedrooms and home values
  tabPanel(
    "Bedrooms",
  # A `titlePanel` with the title "Number of Bedrooms vs. Home Values"
  titlePanel("Number of Bedrooms vs. Home Values"),
  sidebarLayout(
    sidebarPanel(
  # A `selectInput()` labeled "select your state". This dropdown should let
  # the user pick one of the states. 
  selectInput("state",
              label = "Select your state",
              choices = select_values,
              selected = "Washington"
  ),
  # A `checkboxInput()` labeled "compare". Its default value is FALSE
  checkboxInput("compare", label = strong("Do you want to compare with another state?"), value = FALSE),
  
  # A `selectInput()` labeled "select another state". This dropdown should let
  # the user pick one of the state. 
  selectInput("state_two",
              label = "Select another state",
              choices = select_values
  )),
  # A `mainPanel` with a plotOutput showing the 'plot' output
  mainPanel(
    plotOutput("plot")
  )
)),
  # A `tabPanel` about Price Reduction and its relation to time listed on Zillow
  tabPanel(
    "Price Reduction",
    titlePanel("Price Reduction vs. Time on Zillow"),
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
        )
      ),
      mainPanel(
        plotlyOutput("chart")
      )
    )
  )
  
  
)
)