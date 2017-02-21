library(xgboost)
library(data.table)
library(googleVis)

price_data = fread("../stock-data/var-pred/GOOG.csv",
                   col.names = c('timestamp', 'price', 'volume'))
symbol = "GOOG"
# price_data = price_data[timestamp<"2017-01-24 09:30"]
price_data$id <- seq.int(from = 1,
                         to = nrow(price_data),
                         by = 1)
price_data$Close = detrend(price_data$price, 'constant')
nr = NROW(price_data)

lag1 = price_data[, .(id = id + 1, price)]
lag1$lagprice1 = lag1[, price]
lag1$price <- NULL
price_data = merge(price_data, lag1, all.x = T, by = c("id"))
price_data$lagprice1 = log1p(price_data$lagprice1)

lag2 = price_data[, .(id = id + 2, price)]
lag2$lagprice2 = lag2[, price]
lag2$price <- NULL
price_data = merge(price_data, lag2, all.x = T, by = c("id"))
price_data$lagprice2 = log1p(price_data$lagprice2)

lag3 = price_data[, .(id = id + 3, price)]
lag3$lagprice3 = lag3[, price]
lag3$price <- NULL
price_data = merge(price_data, lag3, all.x = T, by = c("id"))
price_data$lagprice3 = log1p(price_data$lagprice3)

lag4 = price_data[, .(id = id + 4, price)]
lag4$lagprice4 = lag4[, price]
lag4$price <- NULL
price_data = merge(price_data, lag4, all.x = T, by = c("id"))
price_data$lagprice4 = log1p(price_data$lagprice4)

lag5 = price_data[, .(id = id + 5, price)]
lag5$lagprice5 = lag5[, price]
lag5$price <- NULL
price_data = merge(price_data, lag5, all.x = T, by = c("id"))
price_data$lagprice5 = log1p(price_data$lagprice5)

lag6 = price_data[, .(id = id + 6, price)]
lag6$lagprice6 = lag6[, price]
lag6$price <- NULL
price_data = merge(price_data, lag6, all.x = T, by = c("id"))
price_data$lagprice6 = log1p(price_data$lagprice6)

test = price_data[(ceiling(nr * .7) + 1):nr,]
train = price_data[4:ceiling(nr * .7), ]

train$price = log1p(train$price)
test$price = log1p(test$price)

features = names(train)[!(names(train) %in% c('id', 'timestamp', 'price', 'volume'))]

matxgb = xgb.DMatrix(data = data.matrix(train[, features, with = F]),
                     label = data.matrix(train[, price]),
                     missing = NA)

model <- xgb.train(
  params = list(
    objective = "reg:linear",
    booster = "gbtree",
    eta = 0.15,
    max_depth = 8,
    subsample = 0.7,
    colsample_bytree = 0.9,
    nthread = 8
  ) ,
  data = matxgb,
  watchlist = list(wl_mat = matxgb),
  nrounds = 200,
  early_stopping_rounds = 10
)

pred = predict(model, xgb.DMatrix(data.matrix(test[, features, with = F]), missing = NA))

test$pred = expm1(pred)
train$price = expm1(train$price)
test$price = expm1(test$price)

dttest = rbindlist(list(train, test), fill = TRUE)

opts = list(
  height = 600,
  title = symbol,
  hAxes = "[{title:'Timestamp'}]",
  vAxes = "[{title:'Stock Price'}]"
  # series = "[{title:'Actual Price'}, {title:'Predicted Price']"
)

df = data.frame(
  dated = dttest$timestamp,
  price = dttest$price,
  predicted = dttest$pred
)
Line <- gvisLineChart(df, options = opts)
plot(Line)

df2 = data.frame(
  dated = test$timestamp,
  price = test$price,
  predicted = test$pred
)
Line2 <- gvisLineChart(df2, options = opts)
plot(Line2)

rmse = sqrt(mean((test$pred - test$price) ^ 2, na.rm = TRUE))
print(rmse)
