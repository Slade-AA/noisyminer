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


# Fit conditional random forest models ----

control_rf <- trainControl(method = "repeatedcv", number = 10, repeats = 1, verbose = FALSE, savePredictions = TRUE)
tunegrid_rf <- expand.grid(.mtry=c(2:4))

# ├ Spatial cross-validation ----
# Iteratively leave out one region at a time to serve as a test set - estimating how well model transfers to new regions

# ├├ Fit random forest models ----
RandomForestOutputs <- list()
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
                          
                          formula <- as.formula(paste0(measure, " ~ ", paste(colnames(select(acousticIndices_richness, ends_with(c("mean")))), collapse = " + ")))
                          
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
    
    RandomForestOutputs <- c(RandomForestOutputs, results)
  }
}


# ├├ Plot model performance ----
#Model performance boxplots
AllPerformance_rf <- lapply(RandomForestOutputs, function(x) x[['performance']]) %>% bind_rows(.id = "Iteration") %>% mutate(Measure = gsub("_.*", "", Iteration),
                                                                                                                             TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration))

Plot_ModelPerformance_rf <- ggplot(data = AllPerformance_rf[AllPerformance_rf$.metric %in% c('mae', 'rmse', 'norm_mae', 'norm_rmse'),], 
                                   aes(x = Measure, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge()) +
  scale_fill_viridis_d() +
  facet_wrap(~factor(.metric, levels = c('mae', 'rmse', 'norm_mae', 'norm_rmse')), scales = "free") +
  labs(x = "Biodiversity Measure", y = "Performance Estimate") +
  theme_bw() +
  theme(legend.position = "none")

legend_bottom <- get_legend(
  Plot_ModelPerformance_rf + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)

Plot_ModelPerformance_rf <- plot_grid(Plot_ModelPerformance_rf, legend_bottom,
                                      ncol = 1, 
                                      rel_heights = c(1, 0.1))

ggsave(filename = paste0("outputs/figures/modelperformance/rf.png"),
       Plot_ModelPerformance_rf,
       width = 18, height = 18, units = "cm", dpi = 800)

#Scatterplots of test set observed vs predicted values
AllPredictions_rf <- lapply(RandomForestOutputs, function(x) x[['predictions']]) %>% lapply(function(x) x %>% rename(obs = 1)) %>%  
  bind_rows(.id = "Iteration") %>% mutate(Measure = gsub("_.*", "", Iteration),
                                          TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration),
                                          Model = "rf",
                                          Region = gsub(".*_[A-Za-z]*_([A-Z]{1,2})", "\\1", Iteration))

Plots_Observed_Predicted_rf <- list()
for (measure in unique(AllPredictions_rf$Measure)) {
  Plots_Observed_Predicted_rf[[measure]] <- ggplot(data = AllPredictions_rf[AllPredictions_rf$Measure == measure,],
                                                    aes(x = .pred, y = obs)) +
    geom_point() +
    geom_abline(slope = 1, linetype = 'dashed') +
    scale_x_continuous(limits = c(min(AllPredictions_rf[AllPredictions_rf$Measure == measure,c(2:3)]), 
                                  max(AllPredictions_rf[AllPredictions_rf$Measure == measure,c(2:3)]))) +
    scale_y_continuous(limits = c(min(AllPredictions_rf[AllPredictions_rf$Measure == measure,c(2:3)]), 
                                  max(AllPredictions_rf[AllPredictions_rf$Measure == measure,c(2:3)]))) +
    labs(x = "Predicted", y = "Observed") +
    facet_wrap(~Region) +
    theme_bw()
  
  ggsave(filename = paste0("outputs/figures/observedvspredicted/rf_", measure, ".png"),
         Plots_Observed_Predicted_rf[[measure]],
         width = 18, height = 18, units = "cm", dpi = 800)
}


#tidymodels version?
rf_fit <- rand_forest(mode = "regression", mtry = 3, trees = 1000) %>%
  set_engine("randomForest") %>%
  fit(
    formula,
    data = trainData
  )
  
  test_results <- 
  testData %>%
  bind_cols(
    predict(rf_fit, new_data = testData[, preds])
  )
  
  # summarize performance
test_results %>% metrics(truth = measure, estimate = .pred)



# ├ Temporal cross-validation ----
# Iteratively leave out one season at a time to serve as a test set - estimating how well model transfer to new time periods




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

xgboostOutputs <- list()
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
                          
                          formula <- as.formula(paste0(measure, " ~ ", paste(colnames(select(acousticIndices_richness, ends_with(c("mean")))), collapse = " + ")))
                          
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
    
    xgboostOutputs <- c(xgboostOutputs, results)
  }
}

# ├├ Plot model performance ----

#Boxplot of test set performance metrics
AllPerformance_xgb <- lapply(xgboostOutputs, function(x) x[['performance']]) %>% bind_rows(.id = "Iteration") %>% mutate(Measure = gsub("_.*", "", Iteration),
                                                                                                                         TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration),
                                                                                                                         Model = "xgboost",
                                                                                                                         Region = gsub(".*_[A-Za-z]*_([A-Z]{1,2})", "\\1", Iteration))

Plot_ModelPerformance_xgb <- ggplot(data = AllPerformance_xgb[AllPerformance_xgb$.metric %in% c('mae', 'rmse', 'norm_mae', 'norm_rmse'),], 
                                    aes(x = Measure, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge()) +
  scale_fill_viridis_d() +
  facet_wrap(~factor(.metric, levels = c('mae', 'rmse', 'norm_mae', 'norm_rmse')), scales = "free") +
  labs(x = "Biodiversity Measure", y = "Performance Estimate") +
  theme_bw() +
  theme(legend.position = "none")

legend_bottom <- get_legend(
  Plot_ModelPerformance_xgb + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)

Plot_ModelPerformance_xgb <- plot_grid(Plot_ModelPerformance_xgb, legend_bottom,
                                       ncol = 1, 
                                       rel_heights = c(1, 0.1))

ggsave(filename = paste0("outputs/figures/modelperformance/xgboost.png"),
       Plot_ModelPerformance_xgb,
       width = 18, height = 18, units = "cm", dpi = 800)

#Scatterplots of test set observed vs predicted values
AllPredictions__xgb <- lapply(xgboostOutputs, function(x) x[['predictions']]) %>% lapply(function(x) x %>% rename(obs = 1)) %>%  
  bind_rows(.id = "Iteration") %>% mutate(Measure = gsub("_.*", "", Iteration),
                                          TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration),
                                          Model = "xgboost",
                                          Region = gsub(".*_[A-Za-z]*_([A-Z]{1,2})", "\\1", Iteration))


Plots_Observed_Predicted_xgb <- list()
for (measure in unique(AllPredictions__xgb$Measure)) {
  Plots_Observed_Predicted_xgb[[measure]] <- ggplot(data = AllPredictions__xgb[AllPredictions__xgb$Measure == measure,],
                                                    aes(x = .pred, y = obs)) +
    geom_point() +
    geom_abline(slope = 1, linetype = 'dashed') +
    scale_x_continuous(limits = c(min(AllPredictions__xgb[AllPredictions__xgb$Measure == measure,c(2:3)]), 
                                  max(AllPredictions__xgb[AllPredictions__xgb$Measure == measure,c(2:3)]))) +
    scale_y_continuous(limits = c(min(AllPredictions__xgb[AllPredictions__xgb$Measure == measure,c(2:3)]), 
                                  max(AllPredictions__xgb[AllPredictions__xgb$Measure == measure,c(2:3)]))) +
    labs(x = "Predicted", y = "Observed") +
    facet_wrap(~Region) +
    theme_bw()
  
  ggsave(filename = paste0("outputs/figures/observedvspredicted/xgboost_", measure, ".png"),
         Plots_Observed_Predicted_xgb[[measure]],
         width = 18, height = 18, units = "cm", dpi = 800)
}