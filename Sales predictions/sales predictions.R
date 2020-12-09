library(tidyverse)
library(DataExplorer)
library(caTools)
library(RcppRoll)
library(stringr)
library(xts)

test <- read.csv("C:\\Users\\coleh\\OneDrive\\Desktop\\BYU\\Fall '20\\STAT 495\\Sales predictions\\test.csv")
train <- read.csv("C:\\Users\\coleh\\OneDrive\\Desktop\\BYU\\Fall '20\\STAT 495\\Sales predictions\\train.csv")

# train_2 <- sample(1:nrow(train), 0.8 * nrow(train))
# test_idx <- setdiff(1:nrow(train), train_2)
# 
# X_train <- train[train_2, ]
# y_train <- train[train_2, "sales"]

class(train$date)
#need to convert to string first so we can extract the month, day, year
train$date <- as.character(train$date)

train$year <- as.numeric(substr(train$date, 1, 4))
train$month <- as.numeric(substr(train$date, 6, 7))
train$day <- as.numeric(as.factor(weekdays(as.Date(train$date)))) #getting day of the week
train$day_num <- as.numeric(substr(train$date, 9, 10))

train$date <- as.Date(str_remove_all(train$date, "-"), "%Y%m%d")

class(train$date)

plot_histogram(train)

hist(train$sales)

# ggplot(X_train, aes(x = year, y = sales)) +
#   geom_line(col = "hotpink") + ylim(0, 800) +
#   scale_x_date(date_labels = "%y %b", date_breaks = "2 month") +
#   theme_bw() + theme(legend.title = element_blank(),
#                      axis.text.x  = element_text(angle=45, vjust=0.5))

#maybe a log transformation is neccessary 
hist(log(train$sales))

#view summary statistics of target vairable
summary(y_train)

#creating rolling lags by 15 days, 30 days, 60 days, and 90 days
# X_train <- X_train %>% 
#   group_by(store, item) %>% 
#   mutate(avg_1 = lag(sales, 1),
#          avg_7 = lag(roll_meanr(sales, 7), 1),
#          avg_14 = lag(roll_meanr(sales, 14), 1),
#          avg_30 = lag(roll_meanr(sales, 30), 1)
#          )

item_data <- train %>% 
  group_by(store, item, month, year) %>% 
  mutate(month_sales = sum(sales))

#Convert dataframe to time series object
# tsdf_train <- xts(X_train, order.by = X_train$date)

item_1_df <- train %>% 
  filter(train$item == 1 & train$store == 1)

sales_item_1_ts <- ts(item_1_df$sales)
sales_item_1_ts

plot.ts(sales_item_1_ts)
