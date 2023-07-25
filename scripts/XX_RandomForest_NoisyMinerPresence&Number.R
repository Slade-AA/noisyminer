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

control_cforest <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verbose = FALSE, savePredictions = TRUE)
tunegrid_cforest <- expand.grid(.mtry=c(2:10))




train(Detected40~.,
      data = rf_data,
      method = "cforest",
      tuneGrid = tunegrid_cforest,
      trControl = control_cforest,
      controls = cforest_unbiased(ntree = 1000))