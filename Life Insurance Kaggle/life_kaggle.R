library(parallel)
library(MASS)

starts <- rep(100, 40)
fx <- function(nstart) kmeans(Boston, 4, nstart=nstart)
numCores <- detectCores()
numCores

library(tidyverse)
library(DataExplorer)
library(Boruta)
# library(mice)
# library(missForest)
library(Hmisc)

train <- read_csv("C:\\Users\\cherndon\\Documents\\Life Insurance Kaggle\\train.csv")
test <- read_csv("C:\\Users\\cherndon\\Documents\\Life Insurance Kaggle\\test.csv")

introduce(train)
plot_intro(train)

plot_missing(train)
plot_missing(test)

dim(train)
dim(test)

glimpse(train)
head(train)

sum(is.na(train$Medical_History_10))
sum(is.na(train$Medical_History_32))

train$Train_Flag <- 1 #Add in a flag to identify if observations fall in train data, 1 train, 0 test
test$Train_Flag <- 0 #Add in a flag to identify if observations fall in train data, 1 train, 0 test
test$Response <- NA #Add in a column for Response in the test data and initialize to NA


#concatenate train and test together, any features we create will be on both data sets with the same code. This will make scoring easy
All_Data <- rbind(train,test) #79,146 observations, 129 variables 



#Define variables as either numeric or factor, Data_1 - Numeric Variables, Data_2 - factor variables
Data_1 <- All_Data[,names(All_Data) %in% c("Product_Info_4",	"Ins_Age",	"Ht",	"Wt",	"BMI",	"Employment_Info_1",	"Employment_Info_4",	"Employment_Info_6",	"Insurance_History_5",	"Family_Hist_2",	"Family_Hist_3",	"Family_Hist_4",	"Family_Hist_5",	"Medical_History_1",	"Medical_History_15",	"Medical_History_24",	"Medical_History_32",paste("Medical_Keyword_",1:48,sep=""))]
Data_2 <- All_Data[,!(names(All_Data) %in% c("Product_Info_4",	"Ins_Age",	"Ht",	"Wt",	"BMI",	"Employment_Info_1",	"Employment_Info_4",	"Employment_Info_6",	"Insurance_History_5",	"Family_Hist_2",	"Family_Hist_3",	"Family_Hist_4",	"Family_Hist_5",	"Medical_History_1",	"Medical_History_15",	"Medical_History_24",	"Medical_History_32",paste("Medical_Keyword_",1:48,sep="")))]
Data_2<- data.frame(apply(Data_2, 2, as.factor))

All_Data <- cbind(Data_1,Data_2) #79,146 observations, 129 variables

#We don't need Data_1,Data_2,train or test anymore
rm(Data_1,Data_2,train,test)


str(All_Data)

colnames(All_Data)[colSums(is.na(All_Data)) > 0]


#dropping columns with over 90% missing data
undesired <- c("Medical_History_10", "Medical_History_32", "Medical_History_24")
All_Data[ , !names(All_Data) %in% c("Medical_History_10", "Medical_History_32", "Medical_History_24")]

#need to impute the missing values in our dataframe
#impute missing values, using all parameters as default values
library(imputeMissings)
#randomForest method
values <- compute(All_Data, method="randomForest")
#median/mode method
values2 <- compute(All_Data)

imp_data <- impute(All_Data, object = values2)

summary(imp_data$Train_Flag)
colSums(is.na(imp_data))

train <- imp_data %>% filter(Train_Flag == 1) #59,381, 131 variables
test <- imp_data %>%  filter(Train_Flag == 0) #19,765, 131 variables

set.seed(1234)
train$random <- runif(nrow(train))


##############################################################
#Step 4: Model building - Build a GBM on a random 70% of train and validate on the other 30% of train.
#        This will be an iterative process where you should add/refine/remove features
##########################################################


train_70 <- train %>% filter(random <= 0.7) #41,561 obs
train_30 <- train %>% filter(random > 0.7) #17,820 obs




# boruta_output <- Boruta(Response ~ . -c(Id, Train_Flag, random), data = train_70, doTrace = 2, maxRuns = 100)

boruta_output$timeTaken
boruta_output$finalDecision
boruta_output$call

confirmed_formula <- getConfirmedFormula(boruta_output)
non_reject_formula <- getNonRejectedFormula(boruta_output)

# Response ~ Product_Info_4 + Ins_Age + Ht + Wt + BMI + Employment_Info_1 + 
#   Employment_Info_4 + Employment_Info_6 + Insurance_History_5 + 
#   Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + Family_Hist_5 + 
#   Medical_History_1 + Medical_History_15 + Medical_History_24 + 
#   Medical_History_32 + Medical_Keyword_1 + Medical_Keyword_3 + 
#   Medical_Keyword_4 + Medical_Keyword_5 + Medical_Keyword_6 + 
#   Medical_Keyword_9 + Medical_Keyword_10 + Medical_Keyword_12 + 
#   Medical_Keyword_14 + Medical_Keyword_15 + Medical_Keyword_16 + 
#   Medical_Keyword_18 + Medical_Keyword_19 + Medical_Keyword_21 + 
#   Medical_Keyword_22 + Medical_Keyword_23 + Medical_Keyword_24 + 
#   Medical_Keyword_25 + Medical_Keyword_27 + Medical_Keyword_28 + 
#   Medical_Keyword_31 + Medical_Keyword_33 + Medical_Keyword_34 + 
#   Medical_Keyword_35 + Medical_Keyword_36 + Medical_Keyword_37 + 
#   Medical_Keyword_38 + Medical_Keyword_40 + Medical_Keyword_41 + 
#   Medical_Keyword_42 + Medical_Keyword_43 + Medical_Keyword_47 + 
#   Medical_Keyword_48 + Product_Info_1 + Product_Info_2 + Product_Info_3 + 
#   Product_Info_5 + Product_Info_7 + Employment_Info_2 + Employment_Info_3 + 
#   Employment_Info_5 + InsuredInfo_1 + InsuredInfo_2 + InsuredInfo_3 + 
#   InsuredInfo_4 + InsuredInfo_5 + InsuredInfo_6 + InsuredInfo_7 + 
#   Insurance_History_1 + Insurance_History_2 + Insurance_History_3 + 
#   Insurance_History_4 + Insurance_History_7 + Insurance_History_8 + 
#   Insurance_History_9 + Family_Hist_1 + Medical_History_2 + 
#   Medical_History_3 + Medical_History_4 + Medical_History_5 + 
#   Medical_History_6 + Medical_History_7 + Medical_History_8 + 
#   Medical_History_9 + Medical_History_10 + Medical_History_11 + 
#   Medical_History_12 + Medical_History_13 + Medical_History_14 + 
#   Medical_History_16 + Medical_History_17 + Medical_History_18 + 
#   Medical_History_19 + Medical_History_20 + Medical_History_21 + 
#   Medical_History_22 + Medical_History_23 + Medical_History_27 + 
#   Medical_History_28 + Medical_History_29 + Medical_History_30 + 
#   Medical_History_31 + Medical_History_33 + Medical_History_35 + 
#   Medical_History_37 + Medical_History_38 + Medical_History_39 + 
#   Medical_History_40 + Medical_History_41

library(deepboost)

class(train_70$Response)


dpb <- deepboost(confirmed_formula, data = train_70,
                 num_iter = 3,
                 beta = 0.1,
                 tree_depth = 5,
                 lambda = .05,
                 loss_type = "l")

library(gbm)
library(caret)

objControl <- trainControl(method='cv', 
                           number=3, 
                           returnResamp='none',
                           summaryFunction = twoClassSummary, 
                           classProbs = TRUE)

gbm_model <- gbm(confirmed_formula, 
                 data = train_70,
                 n.trees=50,
                 distribution = "multinomial",
                 interaction.depth=5,
                 n.minobsinnode=40,
                 shrinkage=0.1,
                 cv.folds=0,
                 n.cores=1,
                 train.fraction=1,
                 bag.fraction=0.7,
                 verbose=T)
                 )


gmb_model$opt_tree <- gbm.perf(gmb_model, method="OOB") #Use the OOB method to determine the optimal number of trees
summary(gmb_model,n.trees=gmb_model$opt_tree)

Prediction_Object <- predict(gbm_model,test,gbm_model$opt_tree,type="response")

#an array with probability of falling into each class for each observation
#We want to classify each application, a trivial approach would be to take the class with the highest predicted probability for each application

test$Response <- apply(Prediction_Object, 1, which.max)

round(table(test$Response)/nrow(test),2)
# 1      2     5     6      7    8 
#0.06  0.04   0.06  0.21  0.05  0.57


submission_file <- test[,c("Id","Response")] #19,765 obs, 2 variables

write.csv(submission_file,"Submission_file.csv",row.names=FALSE)
