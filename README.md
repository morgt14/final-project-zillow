# Final Project - Zillow Home Data
_Winter 2019, INFO 201 @ University of Washington_

**Group BA3:** Yuki Ono ([@yukisea](https://github.com/yukisea)), Morgan Taylor ([@morgt14](https://github.com/morgt14)), Grady Thompson ([@gradyat](https://github.com/gradyat)), Jiaxing Wang ([@Jiaxiw6](https://github.com/Jiaxiw6))

## Project Description

### Dataset

We will be working in Zillow’s datasets. Some of their data is calculated by Zillow, such as their valuations of homes. While Zillow collects some data from their own website, Zillow also collects other data from local governments and real estate companies and agents, such as sale prices. Information about specific houses, such as square footage and number of bedrooms, also come from these different sources (local governments/counties).

### Target Audience

The target audience for this project will be prospective home buyers and renters. The goal of our visualizations will be to help our audience be more well-informed when making decisions about buying or renting a residence.

### Questions Our Data will Seek to Answer
Our data will attempt to answer the following questions:
- Which states have the most suitable prices for home buyers?
- How have the home values of different types of homes changed in recent years?
- What is the relationship between a home's price reduction and the amount of time it remains listed on Zillow?



## Technical Description

We will be using reading in our data from .csv files posted on the [Zillow Research Data website](https://www.zillow.com/research/data/). To create our maps, we will need to use scripts to convert ZIP codes, states, cities, etc. to latitude and longitude coordinates. For our map, we will use the `leaflet` package. For our interactive charts, we will use the `plotly` package. For interaction and web hosting, we will use the `shiny` and shinyapps.io. We do not anticipate any _major_ issues; however, we may face other challenges, such as issues with Git collaboration, making our data readable in our charts and maps, and creating interactions with `shiny` and in our map.
