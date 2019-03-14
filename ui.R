library("shiny")
library("lubridate")
library("plotly")
library("rbokeh")
library("maps")
library("ggplot2")
library("dplyr")
library("tidyr")
library("stringr")

# Read in data from bedroom datasets and store in variables
one <- read.csv("data/state_zhvi_1bedroom.csv")
two <- read.csv("data/state_zhvi_2bedroom.csv")
three <- read.csv("data/state_zhvi_3bedroom.csv")
four <- read.csv("data/state_zhvi_4bedroom.csv")
five_or_more <- read.csv("data/state_zhvi_5bedroom_or_more.csv")
days <- read.csv("data/days_on_zillow_state_final.csv",
                 stringsAsFactors = FALSE
)
cut <- read.csv("data/state_medianpctofpricereduction_allhomes_use.csv",
                stringsAsFactors = FALSE
)
state_price <- read.csv("data/sale_prices_state.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE
)
coordinates <- read.csv("data/coordinates.csv",
                        header = TRUE,
                        stringsAsFactor = FALSE
) %>%
  select(Latitude, Longitude, RegionName)

# Manipulate data for use in inputs and outputs
select_values <- sort(one$RegionName)
colnames <- names(state_price)
mean_2016 <- apply(
  state_price[, grepl("2016", colnames)], 1, mean,
  na.rm = TRUE
)
mean_2017 <- apply(
  state_price[, grepl("2017", colnames)], 1, mean,
  na.rm = TRUE
)
mean_2018 <- apply(
  state_price[, grepl("2018", colnames)], 1, mean,
  na.rm = TRUE
)
state_price <- data.frame(
  RegionName = state_price$RegionName,
  mean_2016 = mean_2016,
  mean_2017 = mean_2017,
  mean_2018 = mean_2018
)
mapdat <- left_join(state_price, coordinates, by = "RegionName")

# Create a user interface with CSS styles
ui <- shinyUI(navbarPage(
  theme = "styles.css",
  "Zillow Data",
  # Create an Overview tab that summarizes the project
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
    p(
      "This project sourced data from the",
      a("Zillow Research Data Website",
        href = "https://www.zillow.com/research/data/"
      ),
      ", which has a variety of datasets for home purchases and rentals. These
      datasets contain data, some of which is collected by Zillow's website,
      and other data that they gather from state and local governments. Our
      project use datasets for sale prices, home values for different types of
      homes over time, time listed on Zillow, and price reduction."
    ),
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
  # Create a Home Values tab that displays an interactive map and contains a
  # `selectIUnput` widget for year
  tabPanel(
    "Sale Prices",
    titlePanel("Sale Prices by State"),
    sidebarLayout(
      sidebarPanel(
        selectInput("year",
          label = "Select Year",
          choices = list(
            "2018" = "mean_2018",
            "2017" = "mean_2017",
            "2016" = "mean_2016"
          ),
          selected = "2018"
        )
      ),
      mainPanel(
        p("In this map, we can get a solid idea of which states have the lowest
          home prices. States in the Great Plains and the Midwest tend to have
          the lowest prices, while most coastal states have higher sale
          prices."),
        rbokehOutput("map"),
        p("Note: Montana, Wyoming, New Mexico, Maine, New Hampshire, Vermont,
          Rhode Island, Mississippi and Hawaii did not have data for this
          dataset.")
      )
    )
  ),
  # Create a Bedrooms tab for a plot of number of bedrooms and home values,
  # containing a `sliderInput` for date, a `cheackboxInput` for a second state
  # option, and two `selectInput` widgets for states.
  tabPanel(
    "Bedrooms",
    # A `titlePanel` with the title "Number of Bedrooms vs. Home Values"
    titlePanel("Number of Bedrooms vs. Home Values"),
    sidebarLayout(
      sidebarPanel(
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
        checkboxInput("compare",
          label = strong("Do you want to compare with another state?"),
          value = FALSE
        ),
        selectInput("state_two",
          label = "Select another state",
          choices = select_values
        )
      ),
      mainPanel(
        p("Unsuprisingly, the plot shows us that a home has more value if the
          home has more bedrooms. However, there are not any other notable
          trends relating to the number of bedrooms. Homes of a certain number
          of bedrooms may become more or less popular for a short period of
          time, but there is no pattern to these shifts. Overall, homes of
          every number of bedrooms follow the same trends as the general
          housing market."),
        plotOutput("plot")
      )
    )
  ),
  # A `tabPanel` that graphs Price Reduction and its relation to time listed
  # on Zillow, contains two `checkboxInput` widgets for showing 2016 and 2017
  # data.
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
        p("This graph demonstrates that there is clearly a correlation between
          the amount of days a home is listed and the % reduction in price.
          As a home sits on the market for a longer period of time, the price
          of that home falls."),
        plotlyOutput("graph")
      )
    )
  )
))
