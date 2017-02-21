library(neuralnet)
library(data.table)
library(googleVis)

price_data = fread("twtr-all.csv")
nr = NROW(price_data)

price_data$Date = as.Date(price_data$Date , "%Y-%m-%d")
test = price_data[1:ceiling(nr * .3),] 
train = price_data[(ceiling(nr * .3)+1):nr, ]
