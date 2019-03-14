library("shiny")
library("lubridate")
library("plotly")
library("rbokeh")
library("maps")
library("ggplot2")
library("dplyr")
library("tidyr")

# Read in data from bedroom datasets and store in variables
one <- read.csv("../data/State_Zhvi_1bedroom.csv")
two <- read.csv("../data/State_Zhvi_2bedroom.csv")
three <- read.csv("../data/State_Zhvi_3bedroom.csv")
four <- read.csv("../data/State_Zhvi_4bedroom.csv")
five_or_more <- read.csv("../data/State_Zhvi_5bedroomOrMore.csv")
days <- read.csv("../data/DaysOnZillow_State_final.csv",
                 stringsAsFactors = FALSE)
cut <- read.csv("../data/State_MedianPctOfPriceReduction_AllHomes_USE.csv",
                stringsAsFactors = FALSE)
State_Price <- read.csv("../data/Sale_Prices_State.csv", header = TRUE,
                         stringsAsFactors = FALSE)
Coordinates <- read.csv("../data/coordinates.csv", header = TRUE,
                        stringsAsFactor = FALSE) %>%
  select(Latitude, Longitude, RegionName)

select_values <- sort(one$RegionName)
state_price <- read.csv("../data/Sale_Prices_State.csv",
                        header = TRUE, stringsAsFactors = FALSE)
colnames <- names(state_price)
mean_2016 <- apply(
  state_price[, grepl("2016", colnames)], 1, mean, na.rm = TRUE)
mean_2017 <- apply(
  state_price[, grepl("2017", colnames)], 1, mean, na.rm = TRUE)
mean_2018 <- apply(
  state_price[, grepl("2018", colnames)], 1, mean, na.rm = TRUE)

# Create a user interface
ui <- shinyUI(navbarPage(
  "Zillow Data",
  includeCSS("styles.css"),
  tabPanel(
    "Overview",
    h1("Project Summary"),
    p("This project reports on data about the US housing market, with the
      objective of identifying certain trends in the data. The project looks
      at factors such as housing prices by state and reasons why a house may
      spend an extended period of time listed on Zillow. We want home buyers
      to be as well-informed aas possible when making decisions about buying a
      house."),
    h2("Data"),
    p("This project sourced data from the",
      a("Zillow Research Data Website",
        href = "https://www.zillow.com/research/data/"),
      ", which has a variety of datasets for home purchases and rentals. These
      datasets contain data, some of which is collected by Zillow's website,
      and other data that they gather from state and local governments. Our
      project use datasets for sale prices, home values for different types of
      homes over time, time listed on Zillow, and price reduction."),
    h2("Audience"),
    p("The intended audience for this project is any prospective home buyer who
      is looking for more information on home prices. Our results can give the
      audience more knowledge about trends in the housing market that will
      allow them to make smarter decisions."),
    h2("Questions to Answer"),
    p("Our project is aimed toward answering the following questions:"),
    p("- Which states have the most suitable prices for home buyers?"),
    p("- How have the home values of different types of homes changed in
      recent years?"),
    p("- What is the relationship between a home's price reduction and the
      amount of time it remains listed on Zillow?"),
    h2("Layout"),
    p("The first tab displays an interactive map that shows the average sale
      price of homes by state in the years 2016-2018. The second tab charts the
      trends of home values over the last two decades based on the number of
      bedrooms in the house. The final tab charts the possible relationship
      between the amount of time a house spends listed on Zillow and how much
      its price falls before being sold."),
    h2("Creators"),
    p("Yuki Ono, Morgan Taylor, Grady Thompson, and Jiaxing Wang")
  ),
  tabPanel(
    "Home Values",
    titlePanel("Home Values by State"),
    sidebarLayout(
      sidebarPanel(
        selectInput("year",
                    label = "Select Year",
                    choices = list("2018" = mean_2018,
                                   "2017" = mean_2017,
                                   "2016" = mean_2016),
                    selected = "2018")
      ),
      mainPanel(
        rbokehOutput("map"),
        p("Note: Montana, Wyoming and New Mexico did not have data for this
          dataset")
      )
    )
  ),
  # A `tabPanel` for data about bedrooms and home values
  tabPanel(
    "Bedrooms",
  # A `titlePanel` with the title "Number of Bedrooms vs. Home Values"
  titlePanel("Number of Bedrooms vs. Home Values"),
  sidebarLayout(
    sidebarPanel(
  # A `selectInput()` labeled "select your state". This dropdown should let
  # the user pick one of the states.
      sliderInput("date_range",
                  "Choose Date Range:",
                  min = as.Date("1996-04-01"),
                  max = as.Date("2019-01-01"),
                  value = c(as.Date("1996-04-01"), as.Date("2019-01-01"))
      ),
      selectInput("state",
              label = "Select your state",
              choices = select_values,
              selected = "Washington"
  ),
  # A `checkboxInput()` labeled "compare". Its default value is FALSE
  checkboxInput("compare",
                label = strong("Do you want to compare with another state?"),
                value = FALSE),

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
    titlePanel("2018 Price Reduction vs. Time on Zillow"),
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
          value = 8
        )
      ),
      mainPanel(
        plotlyOutput("chart")
      )
    )
  )


)
)
