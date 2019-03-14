##############################################################################
############################### Loading R pacakges ###########################
##############################################################################

# tidyverse: for data cleaning
# plotly: for interactive plot such as boxplot, scatter plot and histogram et al.
# DT: for interactive data table
# rbokeh: for interactive maps
library(tidyverse)
library(maps)
library(rbokeh)
library(plotly)
library(dplyr)

##############################################################################
################# Import shooting data into R workspace ######################
##############################################################################
state_price <- read.csv("../data/Sale_Prices_State.csv", header = TRUE,
                        stringsAsFactors = FALSE)
colnames <- names(state_price)

# Manipulate data for use in inputs and outputs
mean_2016 <- apply(state_price[, grepl("2016", colnames)], 1,
                   mean, na.rm = TRUE)
mean_2017 <- apply(state_price[, grepl("2017", colnames)], 1,
                   mean, na.rm = TRUE)
mean_2018 <- apply(state_price[, grepl("2018", colnames)], 1,
                   mean, na.rm = TRUE)

state_price <- data.frame(
  RegionName = state_price$RegionName,
  mean_2016 = mean_2016,
  mean_2017 = mean_2017,
  mean_2018 = mean_2018
)

coordinates <- read.csv("../data/coordinates.csv", header = TRUE,
                        stringsAsFactor = FALSE) %>%
  select(Latitude, Longitude, RegionName)

mapDat <- left_join(state_price, coordinates, by = "RegionName")

color <- mapdat %>% group_by(RegionName) %>%
  summarise(Price = mean(Avarage_Price)) %>%
  as.data.frame()


suppressWarnings(figure(width = 800, height = 500, padding_factor = 0,
                        legend_location = "top_left") %>%
  ly_map("state") %>%
  ly_points(
    Longitude,
    Latitude,
    data = mapdat,
    size = 5,
    color = mean_2018,
    hover = c(RegionName, mean_2018)
  ))
