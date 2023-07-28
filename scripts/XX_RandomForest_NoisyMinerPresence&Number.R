#Script for building random forest models to predict Noisy Miner presence and Number

library(tidyverse)
library(caret)


# Load in summary indices with biodiversity data ----

#Summary indices from Analysis Programs
Indices_Summary <- readRDS(file = "outputs/Indices_Summary_2023-07-18")

#Analysis Programs spectral indices (ACI, CVR, ENT, PMN) aggregated across different frequency bands
Indices_SpectralAggregated <- readRDS(file = "outputs/Indices_SpectralAggregated_2023-07-18")


# Fit conditional Random Forest models ----

library(party)
library(permimp)

MySummary  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

control_cforest_classification <- trainControl(method = "cv",
                                               number = 10,
                                               savePredictions = TRUE,
                                               summaryFunction = MySummary,
                                               classProbs = TRUE)

control_cforest <- trainControl(method = "cv", 
                                number = 10, 
                                verbose = FALSE, 
                                savePredictions = TRUE)

tunegrid_cforest <- expand.grid(.mtry = seq(2,16,2))


RF_Performance_Classification <- data.frame()

RF_Performance_Regression <- data.frame()
RF_Predictions_Regression <- list()


#Progress bar
pb = txtProgressBar(min = 0, max = 5*9*4, initial = 0, style = 3); k <- 0

#Loop through models
for (measure in c("NMPresent", "Threshold20m", "Threshold40m", "MeanMiner20m", "MeanMiner40m")) {  
  for (numDays in seq(1,9)) {
    for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
      rf_data <- Indices_SpectralAggregated %>% 
        filter(type == timeDay, audioDays == numDays) %>% 
        dplyr::select(c(measure, ends_with(c("median"))))
      
      rf_formula <- as.formula(paste0(measure, "~."))
      
      if (typeof(rf_data[,1]) == "integer") {
        rf_data[,1] <- factor(rf_data[,1], levels = c("0", "1"))
        rf_data[,1] <- factor(rf_data[,1], labels = make.names(levels(rf_data[,1])))
        
        rf_data[,1] <- factor(rf_data[,1], levels=rev(levels(rf_data[,1])))
        
        rf_model <- train(rf_formula,
                          data = rf_data,
                          method = "cforest",
                          metric = "AUC",
                          tuneGrid = tunegrid_cforest,
                          trControl = control_cforest_classification,
                          controls = cforest_unbiased(ntree = 500))
        
        RF_Performance_Classification <- bind_rows(RF_Performance_Classification,
                                                   bind_cols(data.frame(measure = measure,
                                                                        audioDays = numDays,
                                                                        timeDay = timeDay,
                                                                        indices = "SpectralAggregated"),
                                                             rf_model$results[rf_model$results$mtry == rf_model$bestTune$mtry,c(2:19)]))
        
        k <- k+1; setTxtProgressBar(pb, k)
      }
      
      if (typeof(rf_data[,1]) == "double") {
        rf_model <- train(rf_formula,
                          data = rf_data,
                          method = "cforest",
                          metric = "RMSE",
                          tuneGrid = tunegrid_cforest,
                          trControl = control_cforest,
                          controls = cforest_unbiased(ntree = 500))
        
        RF_Performance_Regression <- bind_rows(RF_Performance_Regression,
                                                   bind_cols(data.frame(measure = measure,
                                                                        audioDays = numDays,
                                                                        timeDay = timeDay,
                                                                        indices = "SpectralAggregated"),
                                                             rf_model$results[rf_model$results$mtry == rf_model$bestTune$mtry,c(2:7)]))
        
        #Predicted vs observed
        RF_Predictions_Regression[[paste0(measure, "_", timeDay, "_", numDays)]] <- data.frame(preds = predict(rf_model$finalModel),
                                                                                               obs = rf_data[,1],
                                                                                               measure = measure,
                                                                                               type = timeDay,
                                                                                               audioDays = numDays)
        
        k <- k+1; setTxtProgressBar(pb, k)
      }
    }
  }
}


save.image(file = "workspaces/RandomForest_NoisyMinerPresence&Number.RData")

DescTools::CCC(RF_Predictions_Regression$MeanMiner20m_dawn_1$.outcome, RF_Predictions_Regression$MeanMiner20m_dawn_1$obs)$rho.c


# Fit models using indices from multiple day periods (e.g. dawn, solarNoon and dusk) ----


Indices_Summary_wider <- Indices_Summary %>% 
  filter(type %in% c("dawn", "solarNoon", "dusk")) %>% 
  dplyr::select(c("Site", "SurveyIDR12", "NMPresent", "Threshold20m", "Threshold40m", "MeanMiner20m", "MeanMiner40m", "type", "audioDays", ends_with(c("median")))) %>% 
  pivot_wider(names_from = type, values_from = ends_with(c("median"))) %>% 
  drop_na()


#MeanMiner20m
rf_data_MeanMiner20m <- Indices_Summary_wider %>% 
  filter(audioDays == 1) %>% 
  dplyr::select(c("MeanMiner20m", contains("dawn"), contains("dusk"), contains("solarNoon")))

rf_model_MeanMiner20m <- train(MeanMiner20m~.,
                               data = rf_data_MeanMiner20m,
                               method = "cforest",
                               metric = "RMSE",
                               tuneGrid = tunegrid_cforest,
                               trControl = control_cforest,
                               controls = cforest_unbiased(ntree = 1000))

#MeanMiner40m
rf_data_MeanMiner40m <- Indices_Summary_wider %>% 
  filter(audioDays == 1) %>% 
  dplyr::select(c("MeanMiner40m", contains("dawn"), contains("dusk"), contains("solarNoon")))

rf_model_MeanMiner40m <- train(MeanMiner40m~.,
                               data = rf_data_MeanMiner40m,
                               method = "cforest",
                               metric = "RMSE",
                               tuneGrid = tunegrid_cforest,
                               trControl = control_cforest,
                               controls = cforest_unbiased(ntree = 1000))