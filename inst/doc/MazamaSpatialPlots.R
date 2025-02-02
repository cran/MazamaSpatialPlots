## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 7
)

## ----stateMap, warning = FALSE, message = FALSE-------------------------------
library(MazamaSpatialPlots)

# Look at input data
head(example_US_stateObesity)

# Create the map
stateMap(
  data = example_US_stateObesity,
  parameter = 'obesityRate',
  breaks = seq(20, 38, 3),
  palette = 'brewer.bu_pu',
  stateBorderColor = 'white',
  title = "Obesity Rate in U.S. States"
)


## ----countyMap, warning = FALSE, message = FALSE------------------------------
# Look at the input data
head(example_US_countyCovid)

# Create the map
countyMap(
  data = example_US_countyCovid,
  parameter = "cases",
  breaks = c(0,100,200,500,1000,2000,5000,10000,20000,50000,1e6),
  countyBorderColor = "white",
  title = "COVID19 Cases in U.S. States"
)

