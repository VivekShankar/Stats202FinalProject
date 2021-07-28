setwd("~/Documents")
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

# Split data into train/validation splits.
train_ind = stock_data$day <= 77
stock_data_train = stock_data[train_ind,]
stock_data_test = stock_data[!train_ind,] # 9 days worth of test data.

# Split stock_data dataframe by symbol.
stock_data_split = split(stock_data, stock_data$symbol)
num_rows_per_symbol = c(nrow(stock_data_split$A), nrow(stock_data_split$B), nrow(stock_data_split$C),
                        nrow(stock_data_split$D), nrow(stock_data_split$E), nrow(stock_data_split$F),
                        nrow(stock_data_split$G), nrow(stock_data_split$H), nrow(stock_data_split$I),
                        nrow(stock_data_split$J))
stock_A = stock_data_split$A
head(stock_A)
unique(stock_A)

# Plot open over index for stock A. 
# Need to add compute 'time' from the data to display on the x-axis.
plot(stock_A$open, 
     typ = "l", xlab="time", ylab="open", main="Plot of open vs time")
# Plot open over index (only the first 100 rows)
plot(stock_A$open[1:100],
     typ = "l", xlab="time", ylab="open", main="Plot of open vs time")

# Add a 'time_index' column to stock_A data frame. This indicates the current 
# time in units of (fractional) days relative to the earliest time in the 
# dataset (e.g.: 06:00:00 PT) for plotting purposes.
# Method 1 (use 'standardized' seconds)
seconds = as.difftime(stock_A$time, units="secs")
seconds = unclass(seconds)
standardized_time = (seconds - min(seconds))/(max(seconds) - min(seconds))
stock_A$time_index1 = stock_A$day + standardized_time
# Method 2 (use fractional days)
days = as.difftime(stock_A$time, units="days")
stock_A$time_index2 = stock_A$day + days

# Plot open over days (use time_index1 or time_index2)
plot(stock_A$time_index2, stock_A$open,
     typ = "l", xlab="time", ylab="open", main="Plot of open vs days")
xtick<-seq(0, 80, by=10)
axis(side=1, at=xtick, labels = TRUE)

# For Stock A, print the average open/high/low/close stats per day.
aggregate(stock_A[,2:6], list(stock_A$day), mean)
aggregate(stock_A[,2], list(stock_A$day), 
          FUN = function(x) c(mean = mean(x), min = min(x), max = max(x)))

# Should we try to add some lagging indicators?
myDf <- slide(stock_data, "open", NewVar = "open_Lag1", slideBy = -1)
stock_A$open_rm5 = running_average(stock_A$open, n=5)

running_average = function(x, n){
  cx = cumsum(x)
  if (n + 1 < length(cx) - 1) {
    rsum = (cx[(n+1):(length(cx)-1)] - cx[1:(length(cx)-n-1)]) / n
  }
  else {
    rsum = c() 
  }
  c(0, cx[1:n]/c(1:n), rsum)
}

# Fit a basic linear regression model to stock A
# lm.fit = lm(open ~ weekday+average, data=stock_A)
lm.fit = lm(open ~ weekday+open_rm5, data=stock_A)
summary(lm.fit)

# Fit a basic linear regression model. Why does this get R^2 = 0.974
lm.fit = lm(open ~ symbol+weekday, data=stock_data)
summary(lm.fit)

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