library(data.table)
library(googleVis)

results = fread(input = "results/ibm-nn-results.csv", header = TRUE)
data = fread(input = "ibm-all-nn.csv", header = TRUE)
symbol = "IBM"

data = data[ceiling(nrow(data)*.66):nrow(data),]
data$id = seq(1:nrow(data))

# merged = merge(x = results, y = data, by.x = c("inst#"), by.y = c("id"))
results$date = data$Date

opts = list(
  height = 600,
  title = symbol,
  hAxes = "[{title:'Timestamp'}]",
  vAxes = "[{title:'Stock Price'}]"
  # series = "[{title:'Actual Price'}, {title:'Predicted Price']"
)

df = data.frame(
  dated = results$date,
  price = results$actual,
  predicted = results$predicted
)
Line <- gvisLineChart(df, options = opts)
plot(Line)

# df2 = data.frame(
#   dated = combined$Date,
#   price = combined$Close,
#   predicted = combined$pred
# )
# Line2 <- gvisLineChart(df2, options = opts)
# plot(Line2)

