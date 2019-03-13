##############################################################################################
####################################### Loading R pacakges ###################################
##############################################################################################

# tidyverse: for data cleaning
# plotly: for interactive plot such as boxplot, scatter plot and histogram et al.
# DT: for interactive data table
# rbokeh: for interactive maps
library(tidyverse)
library(maps)
library(rbokeh)
library(plotly)

############################################################################################
######################## Import shooting data into R workspace #############################
############################################################################################
State_Price <- read.csv("../data/Sale_Prices_State.csv", header = TRUE, stringsAsFactors = FALSE)
ColNames <- names(State_Price)

Mean_2016 <- apply(State_Price[, grepl("2016", ColNames)], 1, mean, na.rm = TRUE)
Mean_2017 <- apply(State_Price[, grepl("2017", ColNames)], 1, mean, na.rm = TRUE)
Mean_2018 <- apply(State_Price[, grepl("2018", ColNames)], 1, mean, na.rm = TRUE)

State_Price <- data.frame(RegionName = State_Price$RegionName, 
                          Mean_2016 = Mean_2016, 
                          Mean_2017 = Mean_2017, 
                          Mean_2018 = Mean_2018) %>% 
  gather(Year, Avarage_Price, -RegionName)



State_Price$Year <- gsub("Mean_", "", State_Price$Year)


Coordinates <- read.csv("../data/coordinates.csv", header = TRUE, stringsAsFactor = FALSE) %>% 
  select(Latitude, Longitude, RegionName)

MapDat <- left_join(State_Price, Coordinates, by = "RegionName")

Color <- MapDat %>% group_by(RegionName) %>% summarise(Price = mean(Avarage_Price)) %>% as.data.frame()


suppressWarnings(figure(width = 800, height = 500, padding_factor = 0, legend_location = "top_left") %>%
                   ly_map("state") %>%
                   ly_points(
                     Longitude, 
                     Latitude, 
                     data = MapDat, 
                     size = 5,
                     color = Avarage_Price, 
                     hover = c(RegionName, Year, Avarage_Price))
)

