# Load libraries
library(randomForest)
library(DataExplorer)
library(forecast)
library(tidyverse)
library(tseries)
library(ggplot2)

setwd("C:\\Users\\coleh\\OneDrive\\Desktop\\BYU\\Fall '20\\STAT 495\\Sales predictions")

# Read in data
train <- read.csv("train.csv")
test <- read.csv("test.csv")
fulldata <- bind_rows(train, test)

# Do some transformations
fulldata$date <- as.Date(fulldata$date)
fulldata <- fulldata %>% mutate_at(vars(store, item), factor)

# Break into train and test again
train <- fulldata %>% filter(is.na(sales) == FALSE)
test <- fulldata %>% filter(is.na(sales) == TRUE)

# Initialize dataframe to save all predictions
preds <- data.frame()

# Create nested for loop to subset, train, and predict
for (s in 1:10) {
  for (i in 1:50) {
    # Subset
    temp_train <- train %>% filter((store == s) & (item == i)) %>% select(date, sales)
    temp_test <- test %>% filter((store == s) & (item == i))
    
    ## Prepare to fit ARIMA model
    ts_train <- ts(temp_train[,2], start=c(2013,1), frequency = 365)
    # Decomposing data
    #decomp <- stl(ts_train, s.window="periodic")
    #deseasonal <- seasadj(decomp) # Remove seasonality
    # Fit model
    fit <- auto.arima(ts_train, seasonal=TRUE)#, max.p = 15)
    # Predict
    seas_fcast <- forecast(fit, h=90)
    
    # Join predictions to subset of test data
    temp_test$sales <- as.vector(seas_fcast$mean)
    
    
    # Save predictions
    preds <- rbind(preds, temp_test)
    
    # Tracking
    print(paste("Store number:", s, "Item number:", i))
  }
}

dim(preds)
head(preds)

preds_final <- preds %>% select(id, sales)
head(preds_final)
write_csv(preds_final, 'submission.csv', col_names = TRUE)