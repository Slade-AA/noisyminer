library(tidyverse)
library(caret)
library(randomForest)
library(yardstick)

# Best Indices Across Time Periods ----

#Load data generated from bootstrap correlations
BestIndicesPerTimePeriod <- readRDS("outputs/figures/bootstrapcorrelations/BestIndicesPerTimePeriod.RDS")

BestIndicesPerTimePeriod <- BestIndicesPerTimePeriod %>% filter(Year != '2019')

#split into train and test & filter to acoustic index columns
train <- createDataPartition(BestIndicesPerTimePeriod$Threshold40m, p = 0.8, list = F)

trainData <- BestIndicesPerTimePeriod[train,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))
testData <- BestIndicesPerTimePeriod[-train,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))

trainData$Threshold40m <- factor(trainData$Threshold40m, levels = c('0', '1'))
testData$Threshold40m <- factor(testData$Threshold40m, levels = c('0', '1'))


fit_control <- trainControl(method = "cv",
                            number = 10)

rf_model <- train(Threshold40m~.,
                  data = trainData,
                  method = "rf",
                  metric = "Kappa",
                  tuneGrid = expand.grid(mtry = c(2,6,10,14,18,22)),
                  trControl = fit_control)

#Make predictions on test set
predictions_rf <- rf_model %>% predict(testData)

#observed and predicted
obs_pred_rf <- data.frame(observed = testData$Threshold40m, predicted = predictions_rf)

#Calculate model accuracy
custom_metrics <- metric_set(accuracy, bal_accuracy, kap, f_meas, precision, recall)

performance_rf <- obs_pred_rf %>% custom_metrics(truth = observed, estimate = predicted, event_level = "second")
