setwd("~/Documents")

library(DataCombine)

# Load the stocks data set into a data frame.
stock_data <- read.csv(file = 'train_data.csv')
head(stock_data)
dim(stock_data)

################################################################################
# Exploratory Data Analysis ####################################################
################################################################################

# Inspect some basic summary statistics.
summary(stock_data)
unique(stock_data$symbol)
unique(stock_data$day)
count_per_symbol = c()
for (ticker in c("A","B","C","D","E","F","G","H","I","J")) {
  count = length(which(stock_data$symbol == ticker))
  count_per_symbol = append(count_per_symbol, count)
}

# Add weekday column  (values range from [0, 5]) to the stock_data data frame.
stock_data$weekday <- (stock_data$day %% 5)

# Add a 'time_index' column to stock_A data frame. This indicates the current 
# time in units of (fractional) days relative to the earliest time in the 
# dataset (e.g.: 06:00:00 PT) for plotting purposes.
stock_data$date_time = stock_data$day + as.difftime(stock_data$time, units="days")

################################################################################

# Split data into train/validation splits.
train_ind = stock_data$day <= 77
stock_data_train = stock_data[train_ind,]
stock_data_test = stock_data[!train_ind,] # 9 days worth of test data.

################################################################################

# Split stock_data data frame by symbol.
split_df = function(stock_df) {
  split(stock_df, stock_df$symbol)
}

stock_data_tr = split_df(stock_data_train)

stock_A_tr = stock_data_tr$A
stock_B_tr = stock_data_tr$B
stock_C_tr = stock_data_tr$C
stock_D_tr = stock_data_tr$D
stock_E_tr = stock_data_tr$E
stock_F_tr = stock_data_tr$F
stock_G_tr = stock_data_tr$G
stock_H_tr = stock_data_tr$H
stock_I_tr = stock_data_tr$I
stock_J_tr = stock_data_tr$J

stock_data_te = split_df(stock_data_test)

stock_A_te = stock_data_te$A
stock_B_te = stock_data_te$B
stock_C_te = stock_data_te$C
stock_D_te = stock_data_te$D
stock_E_te = stock_data_te$E
stock_F_te = stock_data_te$F
stock_G_te = stock_data_te$G
stock_H_te = stock_data_te$H
stock_I_te = stock_data_te$I
stock_J_te = stock_data_te$J

################################################################################
# Plots for Stock A ############################################################
################################################################################

# Let's work with stock A.

head(stock_A_tr)

# Plot open over row index for stock A. 
plot(stock_A_tr$open, 
     typ = "l", xlab="time", ylab="open", main="Plot of open vs time")

# Plot open over date_time.
plot(stock_A_tr$date_time, stock_A_tr$open,
     typ = "l", xlab="time", ylab="open", main="Plot of open vs days")
xtick<-seq(0, 80, by=10)
axis(side=1, at=xtick, labels = TRUE)

# For Stock A, print the average open/high/low/close stats per day.
aggregate(stock_A_tr[,2:6], list(stock_A_tr$day), mean)
aggregate(stock_A_tr[,2], list(stock_A_tr$day), 
          FUN = function(x) c(mean = mean(x), min = min(x), max = max(x)))

tail(stock_A_tr)
head(stock_A_te)

tail(stock_F_tr)
head(stock_F_te)

################################################################################
# Create lagging indicators/features ###########################################
################################################################################

# Should we try to add some lagging indicators?
stock_A_tr <- slide(stock_A_tr, "open", NewVar = "open_Lag1", slideBy = -1)
stock_A_tr <- slide(stock_A_tr, "open", NewVar = "open_Lag2", slideBy = -2)
stock_A_tr <- slide(stock_A_tr, "open", NewVar = "open_Lag3", slideBy = -3)
stock_A_tr <- slide(stock_A_tr, "open", NewVar = "open_Lag4", slideBy = -4)
stock_A_tr <- slide(stock_A_tr, "open", NewVar = "open_Lag5", slideBy = -5)
stock_A_tr <- slide(stock_A_tr, "open", NewVar = "open_Lag6", slideBy = -6)
stock_A_tr <- slide(stock_A_tr, "open", NewVar = "open_Lag7", slideBy = -7)
stock_A_tr <- slide(stock_A_tr, "open", NewVar = "open_Lag8", slideBy = -8)
stock_A_tr <- slide(stock_A_tr, "open", NewVar = "open_Lag9", slideBy = -9)
stock_A_tr <- slide(stock_A_tr, "open", NewVar = "open_Lag10", slideBy = -10)

stock_A_tr$open_Lag1 <- ifelse(is.na(stock_A_tr$open_Lag1),
                               stock_A_tr$open, stock_A_tr$open_Lag1)
stock_A_tr$open_Lag2 <- ifelse(is.na(stock_A_tr$open_Lag2),
                               stock_A_tr$open, stock_A_tr$open_Lag2)
stock_A_tr$open_Lag3 <- ifelse(is.na(stock_A_tr$open_Lag3),
                               stock_A_tr$open, stock_A_tr$open_Lag3)
stock_A_tr$open_Lag4 <- ifelse(is.na(stock_A_tr$open_Lag4),
                               stock_A_tr$open, stock_A_tr$open_Lag4)
stock_A_tr$open_Lag5 <- ifelse(is.na(stock_A_tr$open_Lag5),
                               stock_A_tr$open, stock_A_tr$open_Lag5)
stock_A_tr$open_Lag6 <- ifelse(is.na(stock_A_tr$open_Lag6),
                               stock_A_tr$open, stock_A_tr$open_Lag6)
stock_A_tr$open_Lag7 <- ifelse(is.na(stock_A_tr$open_Lag7),
                               stock_A_tr$open, stock_A_tr$open_Lag7)
stock_A_tr$open_Lag8 <- ifelse(is.na(stock_A_tr$open_Lag8),
                               stock_A_tr$open, stock_A_tr$open_Lag8)
stock_A_tr$open_Lag9 <- ifelse(is.na(stock_A_tr$open_Lag9),
                               stock_A_tr$open, stock_A_tr$open_Lag9)
stock_A_tr$open_Lag10 <- ifelse(is.na(stock_A_tr$open_Lag10),
                               stock_A_tr$open, stock_A_tr$open_Lag10)

# Lag by n rows in the past.
running_average = function(x, n){
  cx = cumsum(x)
  if (n + 1 < length(cx) - 1) {
    rsum = (cx[(n+1):(length(cx)-1)] - cx[1:(length(cx)-n-1)]) / n
  }
  else {
    rsum = c() 
  }
  c(cx[1], cx[1:n]/c(1:n), rsum)
}

# TODO(vishankar): Write another function that helps lag by n days?

stock_A_tr$open_rm5 = running_average(stock_A_tr$open, n=5)
stock_A_tr$open_rm12 = running_average(stock_A_tr$open, n=12)
stock_A_tr$open_rm720 = running_average(stock_A_tr$open, n=720)
stock_A_tr$open_rm5040 = running_average(stock_A_tr$open, n=5040)

################################################################################
# Modeling #####################################################################
################################################################################

# Fit a basic linear regression model to stock A
# lm.fit = lm(open ~ weekday+average, data=stock_A)
lm.fit = lm(open ~ open_Lag1+open_Lag2+open_Lag3+open_Lag4+open_Lag5+open_Lag6
            +open_Lag7+open_Lag8+open_Lag9+open_Lag10,
            data=stock_A_tr)
lm.fit = lm(open ~ weekday+open_Lag1+open_rm5, data=stock_A_tr)
lm.fit = lm(open ~ weekday+I(open_rm5^2)+log(open_rm12)+open_rm720+open_rm5040,
            data=stock_A_tr)
summary(lm.fit)
train_preds = predict(lm.fit)
train_mse = mean((train_preds - stock_A_tr$open)^2)

plot(stock_A_tr$open, xlab="time", ylab="open", main="Plot of open vs days")
lines(train_preds, col="blue")

RSS = sum((predict(lm.fit) - stock_A_tr$open)^2)
TSS = sum((stock_A_tr$open - mean(stock_A_tr$open))^2)
R2 = 1 - (RSS/TSS)

# Fit a basic linear regression model on all the stock data.
# Why does this get R^2 = 0.974
lm.fit = lm(open ~ symbol+weekday, data=stock_data)
summary(lm.fit)

################################################################################
# Predictions ##################################################################
################################################################################

# Predict on the test set and compute a MSE

get_openrm = function(lag, i, preds, stock_A_tr_col) {
  if (i <= lag) {
    prev_openrm_n_tr = tail(stock_A_tr_col, n=(lag-i+1))
    prev_openrm_n_te = preds[1:(i-1)]
    open_rm_n = mean(c(prev_openrm_n_tr, prev_openrm_n_te))
  } else {
    open_rm_n = mean(preds[(i-lag):(i-1)])
  }
  return (open_rm_n)
}

get_openlag = function(lag, i, preds, stock_A_tr_col) {
  if (i <= lag) {
    open_lag_n = stock_A_tr[nrow(stock_A_tr)-lag+i,]$open
  } else {
    open_lag_n = preds[length(preds)-lag+1]
  }
  return (open_lag_n)
}

# TODO(vishankar): Make it easier to test arbitrary lagged variables.
test_preds = c()
for (i in 1:nrow(stock_A_te)) {
  open_rm5 = get_openrm(5, i, test_preds, stock_A_tr$open_rm5)
  open_rm12 = get_openrm(12, i, test_preds, stock_A_tr$open_rm12)
  open_rm720 = get_openrm(720, i, test_preds, stock_A_tr$open_rm720)
  open_rm5040 = get_openrm(5040, i, test_preds, stock_A_tr$open_rm5040)
  
  open_Lag1 = get_openlag(1, i, test_preds, stock_A_tr$open)
  open_Lag2 = get_openlag(2, i, test_preds, stock_A_tr$open)
  open_Lag3 = get_openlag(3, i, test_preds, stock_A_tr$open)
  open_Lag4 = get_openlag(4, i, test_preds, stock_A_tr$open)
  open_Lag5 = get_openlag(5, i, test_preds, stock_A_tr$open)
  open_Lag6 = get_openlag(6, i, test_preds, stock_A_tr$open)
  open_Lag7 = get_openlag(7, i, test_preds, stock_A_tr$open)
  open_Lag8 = get_openlag(8, i, test_preds, stock_A_tr$open)
  open_Lag9 = get_openlag(9, i, test_preds, stock_A_tr$open)
  open_Lag10 = get_openlag(10, i, test_preds, stock_A_tr$open)
  
  weekday = stock_A_te[i,]$weekday
  test_df = data.frame(weekday, open_Lag1, open_rm5, open_rm12, open_rm720, open_rm5040)
  # test_df = data.frame(open_Lag1, open_Lag2, open_Lag3, open_Lag4, open_Lag5,
  #                      open_Lag6, open_Lag7, open_Lag8, open_Lag9, open_Lag10)
  open_pred = predict(lm.fit, test_df)
  test_preds = append(test_preds, open_pred)
}

test_mse = mean((test_preds - stock_A_te$open)^2)

test_RSS = sum((stock_A_te$open - test_preds)^2)
test_TSS = sum((stock_A_te$open - mean(stock_A_te$open))^2)
test_R2 = 1 - (test_RSS/test_TSS)

# Plot testing data and predictions vs. index
plot(stock_A_te$open, xlab="time", ylab="open", main="Plot of open vs days")
lines(test_preds, col="blue")

# Plot both training/testing data + predictions vs. index.
plot(c(stock_A_tr$open, stock_A_te$open), xlab="time", ylab="open", main="Plot of open vs days")
lines(c(train_preds, test_preds), col="blue")

################################################################################
# Questions ####################################################################
################################################################################

# Questions: 
# 1) Should we build lagging indicators (similar to stock data set from HW2)?
# 2) Should we build SEPARATE models for each stock ticker symbol?
# 3) Normally, it would be best to RANDOMLY split data into train/val/test 
# splits, but would it be better in this case to reserve the last 9 days of data
# for validation?

# Next Steps:
# 1) Split the data into a training and validation set.
# 2) Look at Piazza posts about project. Lecture 8 (has some project details).
# 3) Look at textbook (Stock Market Data), page: 154 Lab 4.6
# Textbook: Chapter 3, page: 94 correlation of error terms.
# ESL: page 206 autoregressive time series models.
# 3) Look at sample projects (from past years?)
# 4) Any fancier techniques (clustering + regression + classification models)?
# 5) Look online for examples of stock price prediction.
# 6) LSTMs? The idea here is to predict the subsequent day with the previous day. 
# Can we use the model's previous predictions as features for next predictions?
# 7) What are the best practices for working with time-series data? How can we 
# perform cross validation?