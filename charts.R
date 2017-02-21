library(data.table)
library(googleVis)
library(pracma)

price_data = fread("aapl-all.csv")
price_data$id <- seq.int(from = nrow(price_data), to = 1, by = -1)
nr = NROW(price_data)

price_data$Date = as.Date(price_data$Date , "%Y-%m-%d")
test = price_data[1:ceiling(nr * .3),] 
train = price_data[(ceiling(nr * .3)+1):nr, ]

y = detrend(test$Close, 'constant')

df2 = data.frame(dated = test$Date, price = test$Close)
Line2 <- gvisLineChart(df2, options = list(height = 600))
plot(Line2)

df1 = data.frame(dated = test$Date, price = y)
Line1 <- gvisLineChart(df1, options = list(height = 600))
plot(Line1)