#Score update so far
#xgb_linear with target variable not factor and boruta = 0.702
#xgb_linear with boruta = 0.719
#random forest with boruta = 0.764

library(DataExplorer)
library(tidyverse)
library(caret)
library(xgboost)
library(glmnet)
library(Matrix)
library(randomForest)

train <- read.csv("C:\\Users\\coleh\\OneDrive\\Desktop\\BYU\\Fall '20\\STAT 495\\Dont overfit\\train.csv")
dim(train)

test <- read.csv("C:\\Users\\coleh\\OneDrive\\Desktop\\BYU\\Fall '20\\STAT 495\\Dont overfit\\test.csv\\test.csv")
dim(test)

introduce(train)

plot_intro(train)

#plot_qq(test)

#only run this if we want classification, in this case we want probabilties so I am not including these
# train$target <- as.factor(train$target)
# is.factor(train$target)

myControl <- trainControl(method = "repeatedcv", 
                          number = 5,
                          repeats = 4)

my.grid <- expand.grid(nrounds = 100, 
                       lambda = 0, 
                       alpha = 1,
                       eta = c(0.3, 0.4))
xgb_lin <- train(target~. -id,
             data = train,
             method = "xgbLinear",
             tuneGrid = my.grid,
             trControl = myControl
)

xgb_lin

pred <- predict(xgb_lin, test)
head(pred)

# data_test <- data.frame(ID=test[,ID],
#                         TARGET=pred_test)

output <- data.frame(ID = test$id, TARGET=pred)

write.csv(output, "C:\\Users\\coleh\\OneDrive\\Desktop\\BYU\\Fall '20\\STAT 495\\Dont overfit\\submission.csv", row.names = FALSE)

# ridge <- train(target~. -id,
#                data = train,
#                method = "ORFridge",
#                tuneGrid = my.grid,
#                trControl = myControl)


#Variable selection hopefully
library(Boruta)
#use na.omit(input_data) if data has missing values
boruta_output <- Boruta(target ~ . -id, data = train, doTrace = 2, maxRuns = 250)

#cex.axis adjusts the xaxis font size
plot(boruta_output, las = 2, cex.axis = 0.5)

plotImpHistory(boruta_output)

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c('Confirmed', "Tentative")])
print(boruta_signif)

#shows statistics for each variable might be useful to save it as dataframe
variable_stats <-  data.frame(attStats(boruta_output))
head(variable_stats)

#Very handy function to get both confrimed and tentative
getNonRejectedFormula(boruta_output)

#to get only confirmed variables use this
getConfirmedFormula(boruta_output)

# boruta_output$finalDecision

xgb_boruta <- train(getNonRejectedFormula(boruta_output),
                 data = train,
                 method = "xgbLinear",
                 tuneGrid = my.grid,
                 trControl = myControl
)

xgb_boruta

pred_boruta <- predict(xgb_boruta, test)
head(pred_boruta)

output_boruta <- data.frame(ID = test$id, TARGET=pred_boruta)

write.csv(output_boruta, "C:\\Users\\coleh\\OneDrive\\Desktop\\BYU\\Fall '20\\STAT 495\\Dont overfit\\submission_boruta.csv", row.names = FALSE)

#Random Forest 
rand_forest <- randomForest(getConfirmedFormula(boruta_output), data = train, type = 'prob')
rf_predictions <- predict(rand_forest, test)

head(rf_predictions)
rf_out <- data.frame(ID = test$id, TARGET=rf_predictions)
write.csv(rf_out, "C:\\Users\\coleh\\OneDrive\\Desktop\\BYU\\Fall '20\\STAT 495\\Dont overfit\\submission_rf.csv", row.names = FALSE)

