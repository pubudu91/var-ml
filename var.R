library(xgboost)
library(data.table)
library(googleVis)

price_data = fread("ibm-all.csv")
symbol = "IBM"

price_data$Date = as.Date(price_data$Date , "%Y-%m-%d")
# price_data = price_data[Date > "1999-12-31"]
nr = NROW(price_data)
price_data$id <- seq.int(from = nrow(price_data),
                         to = 1,
                         by = -1)
# price_data$Close = detrend(price_data$Close, 'constant')

lag1 = price_data[, .(id = id + 1, Close)]
lag1$lagClose1 = lag1[, Close]
lag1$Close <- NULL
price_data = merge(price_data, lag1, all.x = T, by = c("id"))
price_data$lagClose1 = log1p(price_data$lagClose1)

lag2 = price_data[, .(id = id + 2, Close)]
lag2$lagClose2 = lag2[, Close]
lag2$Close <- NULL
price_data = merge(price_data, lag2, all.x = T, by = c("id"))
price_data$lagClose2 = log1p(price_data$lagClose2)

lag3 = price_data[, .(id = id + 3, Close)]
lag3$lagClose3 = lag3[, Close]
lag3$Close <- NULL
price_data = merge(price_data, lag3, all.x = T, by = c("id"))
price_data$lagClose3 = log1p(price_data$lagClose3)

lag4 = price_data[, .(id = id + 4, Close)]
lag4$lagClose4 = lag4[, Close]
lag4$Close <- NULL
price_data = merge(price_data, lag4, all.x = T, by = c("id"))
price_data$lagClose4 = log1p(price_data$lagClose4)

lag5 = price_data[, .(id = id + 5, Close)]
lag5$lagClose5 = lag5[, Close]
lag5$Close <- NULL
price_data = merge(price_data, lag5, all.x = T, by = c("id"))
price_data$lagClose5 = log1p(price_data$lagClose5)

# lag6 = price_data[, .(id = id + 6, Close)]
# lag6$lagClose6 = lag6[, Close]
# lag6$Close <- NULL
# price_data = merge(price_data, lag6, all.x = T, by = c("id"))
# price_data$lagClose6 = log1p(price_data$lagClose6)

test = price_data[(ceiling(nr * .7) + 1):nr, ]
train = price_data[6:ceiling(nr * .7),]

train$Close = log1p(train$Close)
test$Close = log1p(test$Close)

features = names(train)[!(names(train) %in% c(
  'id',
  'Date',
  'Open',
  'High',
  'Low',
  'Close',
  'Volume',
  'Adj Close'
))]

matxgb = xgb.DMatrix(data = data.matrix(train[, features, with = F]),
                     label = data.matrix(train[, Close]),
                     missing = NA)

model <- xgb.train(
  params = list(
    objective = "reg:linear",
    booster = "gbtree",
    eta = 0.3,
    max_depth = 15,
    #   subsample = 0.7,
    #   colsample_bytree = 0.8,
    nthread = 8
  ) ,
  data = matxgb,
  watchlist = list(wl_mat = matxgb),
  nrounds = 50,
  early_stopping_rounds = 10
)

pred = predict(model, xgb.DMatrix(data.matrix(test[, features, with = F]), missing = NA))

test$pred = expm1(pred)
train$Close = expm1(train$Close)
test$Close = expm1(test$Close)

combined = rbindlist(list(train, test), fill = TRUE)
opts = list(
  height = 600,
  title = symbol,
  hAxes = "[{title:'Timestamp'}]",
  vAxes = "[{title:'Stock Price'}]"
  # series = "[{title:'Actual Price'}, {title:'Predicted Price']"
)

df = data.frame(
  dated = test$Date,
  actual = test$Close,
  predicted = test$pred
)
Line <- gvisLineChart(df, options = opts)
plot(Line)

df2 = data.frame(
  dated = combined$Date,
  price = combined$Close,
  predicted = combined$pred
)
Line2 <- gvisLineChart(df2, options = opts)
plot(Line2)

rmse = sqrt(mean((test$pred - test$Close) ^ 2, na.rm = TRUE))
print(rmse)