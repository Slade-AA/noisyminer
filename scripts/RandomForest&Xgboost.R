# Load packages ----

library(tidyverse)
library(tidymodels)
library(caret)
library(randomForest)
library(xgboost)
library(party)
library(permimp)
library(foreach)
library(doParallel)
library(ggpubr)
library(cowplot)

# Load indices and biodiversity data ----

acousticIndices_richness <- readRDS("outputs/data/acousticIndices_richness.RDS")
acousticIndices_richness_repscombined <- readRDS("outputs/data/acousticIndices_richness_repscombined.RDS")

# ├ Create region column ----
acousticIndices_richness$Region <- gsub("[0-9]", "", acousticIndices_richness$Site)
acousticIndices_richness_repscombined$Region <- gsub("[0-9]", "", acousticIndices_richness_repscombined$Site)

#Vector of indices that are specifcally to be used for training Noisy miner models and not general bird models
noisyMinerIndices <- c('CVR_1166_3646_mean', 'CVR_0_1166_3646_11025_mean', 'CVR_ND_mean', #CVR indices based on frequency range of 'chur' vocalisation
                       'ENT_1166_3646_mean', 'ENT_0_1166_3646_11025_mean', 'ENT_ND_mean') #ENT indices based on frequency range of 'chur' vocalisation

# Fit conditional random forest models ----

control_rf <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verbose = FALSE, savePredictions = TRUE)
tunegrid_rf <- expand.grid(.mtry=c(2:4))

# ├ Spatial cross-validation ----
# Iteratively leave out one region at a time to serve as a test set - estimating how well model transfers to new regions

# ├├ Fit random forest models ----
RandomForestOutputs_spatial <- list()
for (region in unique(acousticIndices_richness$Region)) {
  
  for (timeDay in c('dawnChorus')) { #, 'solarNoon', 'eveningChorus', 'day'
    trainData <- acousticIndices_richness[acousticIndices_richness$Region != region & acousticIndices_richness$type == timeDay,]
    testData <- acousticIndices_richness[acousticIndices_richness$Region == region & acousticIndices_richness$type == timeDay,]
    
    cl <- makeCluster(8)
    registerDoParallel(cl)
    
    results <- foreach (measure = c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner'), 
                        .final = function(x) setNames(x, paste(measure = c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner'), timeDay, region, sep = "_")),
                        .packages = c("caret", "party", "tidymodels", "tidyverse", 'permimp'),
                        .inorder = TRUE) %dopar% {
                          
                          #only use noisy miner specific indices in training model for 'NumberNoisyMiner'
                          if (measure == 'NumberNoisyMiner') {
                            formula <- as.formula(paste0(measure, " ~ ", paste(colnames(select(acousticIndices_richness, ends_with(c("mean")))), collapse = " + ")))
                          } else {
                            formula <- as.formula(paste0(measure, " ~ ", paste(grep(paste(noisyMinerIndices, collapse = "|"), colnames(select(acousticIndices_richness, ends_with(c("mean")))), invert = TRUE, value = TRUE), collapse = " + ")))
                          }
                          
                          fit <- train(form = formula,
                                       data = trainData,
                                       method = "cforest",
                                       trControl = control_rf,
                                       tuneGrid = tunegrid_rf,
                                       controls = cforest_unbiased(ntree = 50))
                          predictions <- bind_cols(testData[measure], .pred = predict(fit, newdata = testData))						  
                          
                          performance <- predictions %>% metrics(truth = measure, estimate = .pred)
                          performance <- data.frame(rbind(performance,
                                                          data.frame(.metric = 'norm_rmse', 
                                                                     .estimator = 'custom', 
                                                                     .estimate = performance$.estimate[performance$.metric == 'rmse']/(max(testData[measure])-min(testData[measure]))),
                                                          data.frame(.metric = 'norm_mae', 
                                                                     .estimator = 'custom', 
                                                                     .estimate = performance$.estimate[performance$.metric == 'mae']/(max(testData[measure])-min(testData[measure])))))
                          
                          variableimportance <- permimp(fit$finalModel, conditional = TRUE, progressBar = FALSE, scaled = TRUE)
                          
                          outputs <- list(fit = fit,
                                          predictions = predictions,
                                          performance = performance,
                                          variableimportance = variableimportance)
                          outputs
                        }
    stopCluster(cl)
    
    RandomForestOutputs_spatial <- c(RandomForestOutputs_spatial, results)
  }
}


# ├├ Plot model performance ----
#Model performance boxplots
AllPerformance_rf_spatial <- lapply(RandomForestOutputs_spatial, function(x) x[['performance']]) %>% bind_rows(.id = "Iteration") %>% mutate(Measure = gsub("_.*", "", Iteration),
                                                                                                                                     TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration))

Plot_ModelPerformance_rf_spatial <- ggplot(data = AllPerformance_rf_spatial[AllPerformance_rf_spatial$.metric %in% c('mae', 'rmse', 'norm_mae', 'norm_rmse'),], 
                                   aes(x = Measure, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge()) +
  scale_fill_viridis_d() +
  facet_wrap(~factor(.metric, levels = c('mae', 'rmse', 'norm_mae', 'norm_rmse')), scales = "free") +
  labs(x = "Biodiversity Measure", y = "Performance Estimate") +
  theme_bw() +
  theme(legend.position = "none")

legend_bottom <- get_legend(
  Plot_ModelPerformance_rf_spatial + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)

Plot_ModelPerformance_rf_spatial <- plot_grid(Plot_ModelPerformance_rf_spatial, legend_bottom,
                                      ncol = 1, 
                                      rel_heights = c(1, 0.1))

ggsave(filename = paste0("outputs/figures/modelperformance/rf_spatial.png"),
       Plot_ModelPerformance_rf_spatial,
       width = 18, height = 18, units = "cm", dpi = 800)

#Scatterplots of test set observed vs predicted values
AllPredictions_rf_spatial <- lapply(RandomForestOutputs_spatial, function(x) x[['predictions']]) %>% lapply(function(x) x %>% rename(obs = 1)) %>%  
  bind_rows(.id = "Iteration") %>% mutate(Measure = gsub("_.*", "", Iteration),
                                          TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration),
                                          Model = "rf",
                                          Region = gsub(".*_[A-Za-z]*_([A-Z]{1,2})", "\\1", Iteration))

Plots_Observed_Predicted_rf_spatial <- list()
for (measure in unique(AllPredictions_rf_spatial$Measure)) {
  Plots_Observed_Predicted_rf_spatial[[measure]] <- ggplot(data = AllPredictions_rf_spatial[AllPredictions_rf_spatial$Measure == measure,],
                                                   aes(x = .pred, y = obs)) +
    geom_point() +
    geom_abline(slope = 1, linetype = 'dashed') +
    scale_x_continuous(limits = c(min(AllPredictions_rf_spatial[AllPredictions_rf_spatial$Measure == measure,c(2:3)]), 
                                  max(AllPredictions_rf_spatial[AllPredictions_rf_spatial$Measure == measure,c(2:3)]))) +
    scale_y_continuous(limits = c(min(AllPredictions_rf_spatial[AllPredictions_rf_spatial$Measure == measure,c(2:3)]), 
                                  max(AllPredictions_rf_spatial[AllPredictions_rf_spatial$Measure == measure,c(2:3)]))) +
    labs(x = "Predicted", y = "Observed") +
    facet_wrap(~Region) +
    theme_bw()
  
  ggsave(filename = paste0("outputs/figures/observedvspredicted/rf_spatial_", measure, ".png"),
         Plots_Observed_Predicted_rf_spatial[[measure]],
         width = 18, height = 18, units = "cm", dpi = 800)
}


# ├ Temporal cross-validation ----
# Iteratively leave out one season at a time to serve as a test set - estimating how well model transfer to new time periods

# ├├ Fit random forest models ----
RandomForestOutputs_temporal <- list()
for (seasonyear in unique(acousticIndices_richness$seasonYear)) {
  
  for (timeDay in c('dawnChorus')) { #, 'solarNoon', 'eveningChorus', 'day'
    trainData <- acousticIndices_richness[acousticIndices_richness$seasonYear != seasonyear & acousticIndices_richness$type == timeDay,]
    testData <- acousticIndices_richness[acousticIndices_richness$seasonYear == seasonyear & acousticIndices_richness$type == timeDay,]
    
    cl <- makeCluster(8)
    registerDoParallel(cl)
    
    results <- foreach (measure = c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner'), 
                        .final = function(x) setNames(x, paste(measure = c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner'), timeDay, seasonYear, sep = "_")),
                        .packages = c("caret", "party", "tidymodels", "tidyverse", 'permimp'),
                        .inorder = TRUE) %dopar% {
                          
                          #only use noisy miner specific indices in training model for 'NumberNoisyMiner'
                          if (measure == 'NumberNoisyMiner') {
                            formula <- as.formula(paste0(measure, " ~ ", paste(colnames(select(acousticIndices_richness, ends_with(c("mean")))), collapse = " + ")))
                          } else {
                            formula <- as.formula(paste0(measure, " ~ ", paste(grep(paste(noisyMinerIndices, collapse = "|"), colnames(select(acousticIndices_richness, ends_with(c("mean")))), invert = TRUE, value = TRUE), collapse = " + ")))
                          }
                          
                          fit <- train(form = formula,
                                       data = trainData,
                                       method = "cforest",
                                       trControl = control_rf,
                                       tuneGrid = tunegrid_rf,
                                       controls = cforest_unbiased(ntree = 50))
                          predictions <- bind_cols(testData[measure], .pred = predict(fit, newdata = testData))						  
                          
                          performance <- predictions %>% metrics(truth = measure, estimate = .pred)
                          performance <- data.frame(rbind(performance,
                                                          data.frame(.metric = 'norm_rmse', 
                                                                     .estimator = 'custom', 
                                                                     .estimate = performance$.estimate[performance$.metric == 'rmse']/(max(testData[measure])-min(testData[measure]))),
                                                          data.frame(.metric = 'norm_mae', 
                                                                     .estimator = 'custom', 
                                                                     .estimate = performance$.estimate[performance$.metric == 'mae']/(max(testData[measure])-min(testData[measure])))))
                          
                          variableimportance <- permimp(fit$finalModel, conditional = TRUE, progressBar = FALSE, scaled = TRUE)
                          
                          outputs <- list(fit = fit,
                                          predictions = predictions,
                                          performance = performance,
                                          variableimportance = variableimportance)
                          outputs
                        }
    stopCluster(cl)
    
    RandomForestOutputs_temporal <- c(RandomForestOutputs_temporal, results)
  }
}

# ├├ Plot model performance ----
#Model performance boxplots
AllPerformance_rf_temporal <- lapply(RandomForestOutputs_temporal, function(x) x[['performance']]) %>% 
  bind_rows(.id = "Iteration") %>% 
  mutate(Measure = gsub("_.*", "", Iteration),
         TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration),
         Model = "rf",
         seasonYear = gsub(".*_[A-Za-z]*_([A-Z]{1,2})", "\\1", Iteration))

Plot_ModelPerformance_rf_temporal <- ggplot(data = AllPerformance_rf_temporal[AllPerformance_rf_temporal$.metric %in% c('mae', 'rmse', 'norm_mae', 'norm_rmse'),], 
                                            aes(x = Measure, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge()) +
  scale_fill_viridis_d() +
  facet_wrap(~factor(.metric, levels = c('mae', 'rmse', 'norm_mae', 'norm_rmse')), scales = "free") +
  labs(x = "Biodiversity Measure", y = "Performance Estimate") +
  theme_bw() +
  theme(legend.position = "none")

legend_bottom <- get_legend(
  Plot_ModelPerformance_rf_temporal + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)

Plot_ModelPerformance_rf_temporal <- plot_grid(Plot_ModelPerformance_rf_temporal, legend_bottom,
                                               ncol = 1, 
                                               rel_heights = c(1, 0.1))

ggsave(filename = paste0("outputs/figures/modelperformance/rf_temporal.png"),
       Plot_ModelPerformance_rf_temporal,
       width = 18, height = 18, units = "cm", dpi = 800)



# Fit xgboost models ----

# ├ Spatial cross-validation ----
# Iteratively leave out one region at a time to serve as a test set - estimating how well model transfers to new regions

# ├├ Fit xgboost model ----

control_xgb <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verbose = FALSE, savePredictions = TRUE, allowParallel = TRUE)
tunegrid_xgb <- expand.grid(nrounds = 50,
                            eta = c(0.025, 0.05),#, 0.1, 0.3),
                            max_depth = seq(5,5),
                            gamma = 0,
                            colsample_bytree = 1,
                            min_child_weight = 1,
                            subsample = 1)

xgboostOutputs_spatial <- list()
for (region in unique(acousticIndices_richness$Region)) {
  
  for (timeDay in c('dawnChorus')) { #, 'solarNoon', 'eveningChorus', 'day'
    trainData <- acousticIndices_richness[acousticIndices_richness$Region != region & acousticIndices_richness$type == timeDay,]
    testData <- acousticIndices_richness[acousticIndices_richness$Region == region & acousticIndices_richness$type == timeDay,]
    
    cl <- makeCluster(8)
    registerDoParallel(cl)
    
    results <- foreach (measure = c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner'), 
                        .final = function(x) setNames(x, paste(measure = c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner'), timeDay, region, sep = "_")),
                        .packages = c("caret", "xgboost", "tidymodels", "tidyverse", 'permimp'),
                        .inorder = TRUE) %dopar% {
                          
                          #only use noisy miner specific indices in training model for 'NumberNoisyMiner'
                          if (measure == 'NumberNoisyMiner') {
                            formula <- as.formula(paste0(measure, " ~ ", paste(colnames(select(acousticIndices_richness, ends_with(c("mean")))), collapse = " + ")))
                          } else {
                            formula <- as.formula(paste0(measure, " ~ ", paste(grep(paste(noisyMinerIndices, collapse = "|"), colnames(select(acousticIndices_richness, ends_with(c("mean")))), invert = TRUE, value = TRUE), collapse = " + ")))
                          }
                          
                          fit <- train(form = formula,
                                       data = trainData,
                                       method = "xgbTree",
                                       trControl = control_xgb,
                                       tuneGrid = tunegrid_xgb)
                          predictions <- bind_cols(testData[measure], .pred = predict(fit, newdata = testData))						  
                          
                          performance <- predictions %>% metrics(truth = measure, estimate = .pred)
                          performance <- data.frame(rbind(performance,
                                                          data.frame(.metric = 'norm_rmse', 
                                                                     .estimator = 'custom', 
                                                                     .estimate = performance$.estimate[performance$.metric == 'rmse']/(max(testData[measure])-min(testData[measure]))),
                                                          data.frame(.metric = 'norm_mae', 
                                                                     .estimator = 'custom', 
                                                                     .estimate = performance$.estimate[performance$.metric == 'mae']/(max(testData[measure])-min(testData[measure])))))
                          
                          
                          outputs <- list(fit = fit,
                                          predictions = predictions,
                                          performance = performance)
                          outputs
                        }
    stopCluster(cl)
    
    xgboostOutputs_spatial <- c(xgboostOutputs_spatial, results)
  }
}

# ├├ Plot model performance ----

#Boxplot of test set performance metrics
AllPerformance_xgb_spatial <- lapply(xgboostOutputs_spatial, function(x) x[['performance']]) %>% bind_rows(.id = "Iteration") %>% mutate(Measure = gsub("_.*", "", Iteration),
                                                                                                                                         TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration),
                                                                                                                                         Model = "xgboost",
                                                                                                                                         Region = gsub(".*_[A-Za-z]*_([A-Z]{1,2})", "\\1", Iteration))

Plot_ModelPerformance_xgb_spatial <- ggplot(data = AllPerformance_xgb_spatial[AllPerformance_xgb$.metric %in% c('mae', 'rmse', 'norm_mae', 'norm_rmse'),], 
                                            aes(x = Measure, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge()) +
  scale_fill_viridis_d() +
  facet_wrap(~factor(.metric, levels = c('mae', 'rmse', 'norm_mae', 'norm_rmse')), scales = "free") +
  labs(x = "Biodiversity Measure", y = "Performance Estimate") +
  theme_bw() +
  theme(legend.position = "none")

legend_bottom <- get_legend(
  Plot_ModelPerformance_xgb_spatial + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)

Plot_ModelPerformance_xgb_spatial <- plot_grid(Plot_ModelPerformance_xgb_spatial, legend_bottom,
                                               ncol = 1, 
                                               rel_heights = c(1, 0.1))

ggsave(filename = paste0("outputs/figures/modelperformance/xgboost_spatial.png"),
       Plot_ModelPerformance_xgb_spatial,
       width = 18, height = 18, units = "cm", dpi = 800)

#Scatterplots of test set observed vs predicted values
AllPredictions_xgb_spatial <- lapply(xgboostOutputs_spatial, function(x) x[['predictions']]) %>% lapply(function(x) x %>% rename(obs = 1)) %>%  
  bind_rows(.id = "Iteration") %>% mutate(Measure = gsub("_.*", "", Iteration),
                                          TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration),
                                          Model = "xgboost",
                                          Region = gsub(".*_[A-Za-z]*_([A-Z]{1,2})", "\\1", Iteration))


Plots_Observed_Predicted_xgb_spatial <- list()
for (measure in unique(AllPredictions_xgb_spatial$Measure)) {
  CCClabels <- AllPredictions_xgb_spatial %>% group_by(Region) %>% summarise(CCC = round(DescTools::CCC(.pred[Measure == measure],obs[Measure == measure])$rho.c$est, 2))
  
  Plots_Observed_Predicted_xgb_spatial[[measure]] <- ggplot(data = AllPredictions_xgb_spatial[AllPredictions__xgb$Measure == measure,],
                                                            aes(x = .pred, y = obs)) +
    geom_point() +
    geom_abline(slope = 1, linetype = 'dashed') +
    scale_x_continuous(limits = c(min(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)]), 
                                  max(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)]))) +
    scale_y_continuous(limits = c(min(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)]), 
                                  max(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)]))) +
    geom_text(data = CCClabels, aes(x = 0.8*(max(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)]) -
                                                                   min(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)])) + 
                                                          min(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)]), 
                                                        y = min(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)])), 
              label = CCC) +
    #annotate(geom = "text", 
    #         x = 0.8*(max(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)]) -
    #                    min(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)])) + 
    #           min(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)]), 
    #         y = min(AllPredictions_xgb_spatial[AllPredictions_xgb_spatial$Measure == measure,c(2:3)]), 
    #         vjust = 0, size = 3,
    #         label = paste0("CCC: ", AllPredictions_xgb_spatial %>% group_by(Region) %>% summarise(CCC = DescTools::CCC(.pred[Measure == measure],obs[Measure == measure])$rho.c$est) %>% select(CCC) %>% unlist() %>% as.numeric() %>% round(digits = 2))) +
    labs(x = "Predicted", y = "Observed") +
    facet_wrap(~Region) +
    theme_bw()
  
  ggsave(filename = paste0("outputs/figures/observedvspredicted/xgboost_spatial_", measure, ".png"),
         Plots_Observed_Predicted_xgb_spatial[[measure]],
         width = 18, height = 18, units = "cm", dpi = 800)
}

# ├ Temporal cross-validation ----
# Iteratively leave out one season at a time to serve as a test set - estimating how well model transfer to new time periods



# Compare rf and xgboost performance ----

# ├ Spatial ----


# ├ Temporal ----


