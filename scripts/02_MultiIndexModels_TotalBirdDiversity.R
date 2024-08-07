#Script to fit multi-index models against Total Bird Diversity across # of recording days
library(tidyverse)
library(caret)
library(randomForest)
library(mgcv)
library(nnet)
library(yardstick)

#Performance metrics ----
MAE = function(pred, obs){
  mean(abs(pred - obs))
}

RMSE = function(pred, obs){
  sqrt(mean((pred - obs)^2))
}

R = function(pred, obs){
  sum((obs - mean(obs))*(pred - mean(pred))) / sqrt(sum((obs - mean(obs))^2)*sum((pred - mean(pred))^2))
}

allSummary <- readRDS("outputs/Indices_Summary_2023-11-10")


# Fit models ----

prediction_results <- data.frame()

#Progress bar
pb = txtProgressBar(min = 0, max = 8*10, initial = 0, style = 3); k <- 0

set.seed(1234)
for (numDays in seq(1, 8)) {
  
  rf_data <- allSummary %>% filter(type == 'dawn' & audioDays == numDays)
  
  for (i in 1:10) {
    
    
    
    #split into train and test & filter to acoustic index columns
    train <- createDataPartition(rf_data$Detected40, p = 0.8, list = F)
    
    trainData <- rf_data[train,] %>% dplyr::select(c('Detected40', ends_with(c("median")))) # 
    testData <- rf_data[-train,] %>% dplyr::select(c('Detected40', ends_with(c("median")))) #'Site', 
    
    fit_control <- trainControl(method = "cv",
                                number = 10)
    
    rf_model <- train(Detected40~.,
                      data = trainData,
                      method = "rf",
                      metric = "RMSE",
                      tuneGrid = expand.grid(mtry = c(2,4,6,8,10)),
                      ntree = 500,
                      trControl = fit_control)
    
    #print(rf_model)
    
    #Make predictions on test set
    predictions_rf <- rf_model %>% predict(testData)
    
    #observed and predicted
    obs_pred_rf <- data.frame(observed = testData$Detected40, predicted = predictions_rf)
    
    obs_pred_rf_train <- data.frame(observed = rf_model$trainingData$.outcome, predicted = rf_model$finalModel$predicted)
    
    prediction_results <- bind_rows(prediction_results,
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "rf",
                                          performance = "test",
                                          DescTools::CCC(obs_pred_rf$observed, obs_pred_rf$predicted)$rho.c,
                                          MAE = MAE(obs_pred_rf$predicted, obs_pred_rf$observed),
                                          RMSE = RMSE(obs_pred_rf$predicted, obs_pred_rf$observed)),
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "rf", 
                                          performance = "train",
                                          DescTools::CCC(obs_pred_rf_train$observed, obs_pred_rf_train$predicted)$rho.c,
                                          MAE = MAE(obs_pred_rf_train$predicted, obs_pred_rf_train$observed),
                                          RMSE = RMSE(obs_pred_rf_train$predicted, obs_pred_rf_train$observed)))
    
    
    #nnet model
    fit_control <- trainControl(method = "cv",
                                number = 10)
    
    nnet_model <- train(Detected40 ~ .,
                        data = trainData,
                        method = "nnet",
                        metric = "RMSE",
                        preProcess = c('center', 'scale'),
                        trControl = fit_control,
                        linout = 1,
                        trace = F,
                        tuneGrid = expand.grid(decay = c(0.5, 0.1, 1e-2, 1e-4, 1e-6),
                                               size = c(3, 5, 7, 9, 11)))
    
    #Make predictions on test set
    predictions_nnet <- nnet_model %>% predict(testData)
    
    #observed and predicted
    obs_pred_nnet <- data.frame(observed = testData$Detected40, predicted = predictions_nnet)
    
    obs_pred_nnet_train <- data.frame(observed = nnet_model$trainingData$.outcome, predicted = nnet_model$finalModel$fitted.values)
    
    prediction_results <- bind_rows(prediction_results,
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "nnet",
                                          performance = "test",
                                          DescTools::CCC(obs_pred_nnet$observed, obs_pred_nnet$predicted)$rho.c,
                                          MAE = MAE(obs_pred_nnet$predicted, obs_pred_nnet$observed),
                                          RMSE = RMSE(obs_pred_nnet$predicted, obs_pred_nnet$observed)),
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "nnet", 
                                          performance = "train",
                                          DescTools::CCC(obs_pred_nnet_train$observed, obs_pred_nnet_train$predicted)$rho.c,
                                          MAE = MAE(obs_pred_nnet_train$predicted, obs_pred_nnet_train$observed),
                                          RMSE = RMSE(obs_pred_nnet_train$predicted, obs_pred_nnet_train$observed)))
    
    #glm model - all features
    glm_model <- train(Detected40 ~ .,
                       data = trainData,
                       method = "glm",
                       family = "poisson",
                       trControl = trainControl(method = "none"))
    
    predictions_glm <- glm_model %>% predict(testData)
    obs_pred_glm <- data.frame(observed = testData$Detected40, predicted = predictions_glm)
    
    obs_pred_glm_train <- data.frame(observed = glm_model$finalModel$y, predicted = glm_model$finalModel$fitted.values)
    
    prediction_results <- bind_rows(prediction_results,
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "glm",
                                          performance = "test", 
                                          DescTools::CCC(obs_pred_glm$observed, obs_pred_glm$predicted)$rho.c,
                                          MAE = MAE(obs_pred_glm$predicted, obs_pred_glm$observed),
                                          RMSE = RMSE(obs_pred_glm$predicted, obs_pred_glm$observed)),
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "glm",
                                          performance = "train", 
                                          DescTools::CCC(obs_pred_glm_train$observed, obs_pred_glm_train$predicted)$rho.c,
                                          MAE = MAE(obs_pred_glm_train$predicted, obs_pred_glm_train$observed),
                                          RMSE = RMSE(obs_pred_glm_train$predicted, obs_pred_glm_train$observed)))
    
    #glm model - reduced features
    
    glm_model_reduced <- train(Detected40 ~ HighFreqCover_median + 
                                 MidFreqCover_median + 
                                 AcousticComplexity_median + 
                                 ClusterCount_median + 
                                 Ndsi_median + 
                                 SptDensity_median,
                               data = trainData,
                               method = "glm",
                               family = "poisson",
                               trControl = trainControl(method = "none"))
    
    predictions_glm_reduced <- glm_model_reduced %>% predict(testData)
    obs_pred_glm_reduced <- data.frame(observed = testData$Detected40, predicted = predictions_glm_reduced)
    
    obs_pred_glm_reduced_train <- data.frame(observed = glm_model_reduced$finalModel$y, predicted = glm_model_reduced$finalModel$fitted.values)
    
    prediction_results <- bind_rows(prediction_results,
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "glm_reduced",
                                          performance = "test", 
                                          DescTools::CCC(obs_pred_glm_reduced$observed, obs_pred_glm_reduced$predicted)$rho.c,
                                          MAE = MAE(obs_pred_glm_reduced$predicted, obs_pred_glm_reduced$observed),
                                          RMSE = RMSE(obs_pred_glm_reduced$predicted, obs_pred_glm_reduced$observed)),
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "glm_reduced",
                                          performance = "train", 
                                          DescTools::CCC(obs_pred_glm_reduced_train$observed, obs_pred_glm_reduced_train$predicted)$rho.c,
                                          MAE = MAE(obs_pred_glm_reduced_train$predicted, obs_pred_glm_reduced_train$observed),
                                          RMSE = RMSE(obs_pred_glm_reduced_train$predicted, obs_pred_glm_reduced_train$observed)))
    
    #GAM
    
    gam_model <- train(Detected40 ~ .,
                       data = trainData,
                       method = "gam",
                       family = "poisson",
                       trControl = trainControl(method = "none"),
                       tuneGrid = data.frame(method = "GCV.Cp", select = FALSE))
    
    predictions_gam <- gam_model %>% predict(testData)
    obs_pred_gam <- data.frame(observed = testData$Detected40, predicted = predictions_gam)
    
    obs_pred_gam_train <- data.frame(observed = gam_model$finalModel$y, predicted = gam_model$finalModel$fitted.values)
    
    prediction_results <- bind_rows(prediction_results,
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "gam",
                                          performance = "test", 
                                          DescTools::CCC(obs_pred_gam$observed, obs_pred_gam$predicted)$rho.c,
                                          MAE = MAE(obs_pred_gam$predicted, obs_pred_gam$observed),
                                          RMSE = RMSE(obs_pred_gam$predicted, obs_pred_gam$observed)),
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "gam",
                                          performance = "train", 
                                          DescTools::CCC(obs_pred_gam_train$observed, obs_pred_gam_train$predicted)$rho.c,
                                          MAE = MAE(obs_pred_gam_train$predicted, obs_pred_gam_train$observed),
                                          RMSE = RMSE(obs_pred_gam_train$predicted, obs_pred_gam_train$observed)))
    
    #GAM - reduced
    gam_model_reduced <- train(Detected40 ~ HighFreqCover_median + 
                                 MidFreqCover_median + 
                                 AcousticComplexity_median + 
                                 ClusterCount_median + 
                                 Ndsi_median + 
                                 SptDensity_median,
                               data = trainData,
                               method = "gam",
                               family = "poisson",
                               trControl = trainControl(method = "none"),
                               tuneGrid = data.frame(method = "GCV.Cp", select = FALSE))
    
    predictions_gam_reduced <- gam_model_reduced %>% predict(testData)
    obs_pred_gam_reduced <- data.frame(observed = testData$Detected40, predicted = predictions_gam_reduced)
    
    obs_pred_gam_reduced_train <- data.frame(observed = gam_model_reduced$finalModel$y, predicted = gam_model_reduced$finalModel$fitted.values)
    
    prediction_results <- bind_rows(prediction_results,
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "gam_reduced",
                                          performance = "test", 
                                          DescTools::CCC(obs_pred_gam_reduced$observed, obs_pred_gam_reduced$predicted)$rho.c,
                                          MAE = MAE(obs_pred_gam_reduced$predicted, obs_pred_gam_reduced$observed),
                                          RMSE = RMSE(obs_pred_gam_reduced$predicted, obs_pred_gam_reduced$observed)),
                                    cbind(type = 'dawn',
                                          audioDays = numDays,
                                          method = "gam_reduced",
                                          performance = "train", 
                                          DescTools::CCC(obs_pred_gam_reduced_train$observed, obs_pred_gam_reduced_train$predicted)$rho.c,
                                          MAE = MAE(obs_pred_gam_reduced_train$predicted, obs_pred_gam_reduced_train$observed),
                                          RMSE = RMSE(obs_pred_gam_reduced_train$predicted, obs_pred_gam_reduced_train$observed)))
    
    k <- k+1; setTxtProgressBar(pb, k)
    
  }
}

## Models started to fail at 9 days of audio data (Model has more coefficients than data)


ggplot(prediction_results[prediction_results$audioDays < 9,], 
       aes(x = factor(audioDays, levels = c(1,2,3,4,5,6,7,8)), y = MAE, fill = method)) + 
  geom_boxplot() +
  facet_wrap(~performance) +
  theme_bw()

ggplot(prediction_results %>% filter(performance == 'train', audioDays < 9), 
       aes(x = factor(audioDays, levels = c(1,2,3,4,5,6,7,8)), y = MAE, fill = method)) + 
  geom_boxplot() +
  theme_bw()

ggplot(prediction_results, aes(x = factor(audioDays, levels = c(1,2,3,4,5,6,7,8)), y = MAE, fill = performance)) + 
  geom_boxplot() +
  facet_wrap(~method) +
  theme_bw()



ggplot(data = prediction_results, aes(x = factor(audioDays, levels = c(1,2,3,4,5,6,7,8)), y = est, fill = method)) + 
  geom_boxplot() +
  facet_wrap(~performance) +
  theme_bw()




# Conditional random forest model ----

library(party)
library(permimp)
control_cforest <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verbose = FALSE, savePredictions = TRUE)
tunegrid_cforest <- expand.grid(.mtry=c(2:10))


RandomForestFits_cforest <- list()
RandomForestPerformance_cforest <- data.frame()
RandomForestImportance_cforest <- list()
RandomForestImportance_cforest_conditional <- list()
RandomForestPredictions_cforest <- list()

pb = txtProgressBar(min = 0, max = 8 * 2, initial = 0, style = 3); k <- 0

set.seed(1234)
for (numDays in seq(1, 8)) {
  
  for (siteVariable in c("no_site", "site")) {
    
    if (siteVariable == "no_site") {
      rf_data <- allSummary %>% 
        filter(type == 'dawn' & audioDays == numDays) %>% 
        dplyr::select(c('Detected40', ends_with(c("median"))))
    } else {
      rf_data <- allSummary %>% 
        filter(type == 'dawn' & audioDays == numDays) %>% 
        dplyr::select(c('Detected40', 'Site', ends_with(c("median")))) %>% 
        mutate(Site = as.factor(Site))
    }
    
    #Fit model
    RandomForestFits_cforest[[paste0("dawn_", siteVariable, "_", numDays)]] <- train(Detected40~.,
                                                                                     data = rf_data,
                                                                                     method = "cforest",
                                                                                     tuneGrid = tunegrid_cforest,
                                                                                     trControl = control_cforest,
                                                                                     controls = cforest_unbiased(ntree = 500))
    
    #Extract performance of best tune
    RandomForestPerformance_cforest <- bind_rows(RandomForestPerformance_cforest,
                                                 data.frame(cbind(type = 'dawn',
                                                                  audioDays = numDays,
                                                                  site = siteVariable,
                                                                  method = "rf_conditional",
                                                                  RandomForestFits_cforest[[paste0("dawn_", siteVariable, "_", numDays)]]$results[RandomForestFits_cforest[[paste0("dawn_", siteVariable, "_", numDays)]]$results$mtry == RandomForestFits_cforest[[paste0("dawn_", siteVariable, "_", numDays)]]$bestTune[[1]],])))
    
    #Calculate variable importance
    RandomForestImportance_cforest[[paste0("dawn_", siteVariable, "_", numDays)]] <- varImp(RandomForestFits_cforest[[paste0("dawn_", siteVariable, "_", numDays)]])
    RandomForestImportance_cforest_conditional[[paste0("dawn_", siteVariable, "_", numDays)]] <- permimp(RandomForestFits_cforest[[paste0("dawn_", siteVariable, "_", numDays)]]$finalModel, conditional = TRUE, progressBar = FALSE, scaled = TRUE)
    
    #Extract observed vs predicted data
    RandomForestPredictions_cforest[[paste0("dawn_", siteVariable, "_", numDays)]] <- data.frame(predictions = predict(RandomForestFits_cforest[[paste0("dawn_", siteVariable, "_", numDays)]]$finalModel),
                                                                                                 observations = rf_data$Detected40,
                                                                                                 type = 'dawn',
                                                                                                 audioDays = numDays,
                                                                                                 site = siteVariable,
                                                                                                 stringsAsFactors = FALSE)
    
    k <- k+1; setTxtProgressBar(pb, k)
  }
}

save.image(file = "workspaces/MultiIndexModels_TotalBirdDiversity.RData")