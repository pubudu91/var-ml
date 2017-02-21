library(data.table)

data = fread("ibm-all.csv", header = TRUE)
filename = "ibm-all-nn.csv"
data = data[order(Date)]
data$id = seq(1:nrow(data))

nn_table = data.table(id = 1:nrow(data))

nn_table$Bias = 1
# nn_table$id = NULL

nn_table$Date = data$Date

lag5 = data[, .(id = id + 5, Close)]
lag5$lagClose5 = lag5[, Close]
lag5$Close <- NULL
nn_table = merge(nn_table, lag5, all.x = T, by = c("id"))

lag4 = data[, .(id = id + 4, Close)]
lag4$lagClose4 = lag4[, Close]
lag4$Close <- NULL
nn_table = merge(nn_table, lag4, all.x = T, by = c("id"))

lag3 = data[, .(id = id + 3, Close)]
lag3$lagClose3 = lag3[, Close]
lag3$Close <- NULL
nn_table = merge(nn_table, lag3, all.x = T, by = c("id"))

lag2 = data[, .(id = id + 2, Close)]
lag2$lagClose2 = lag2[, Close]
lag2$Close <- NULL
nn_table = merge(nn_table, lag2, all.x = T, by = c("id"))

lag1 = data[, .(id = id + 1, Close)]
lag1$lagClose1 = lag1[, Close]
lag1$Close <- NULL
nn_table = merge(nn_table, lag1, all.x = T, by = c("id"))

nn_table$price = data$Close

nn_table = nn_table[id > 5]

nn_table$id = NULL

write.csv(format(nn_table, digits=3, nsmall=3), file = filename, row.names = FALSE)
