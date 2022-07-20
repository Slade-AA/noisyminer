library(tidyverse)
library(factoextra) #pca tools
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

# Spatial cross-validation ----

lm_pca_spatial <- list()
for (region in unique(acousticIndices_richness$Region)) {
  
  for (timeDay in c('dawnChorus', 'solarNoon', 'eveningChorus', 'day')) {
    
    for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner')) {
      
      #split into train and test & filter to acoustic index columns
      trainData <- acousticIndices_richness[acousticIndices_richness$Region != region & acousticIndices_richness$type == timeDay,] %>% dplyr::select(c(measure, ends_with(c("mean"))))
      testData <- acousticIndices_richness[acousticIndices_richness$Region == region & acousticIndices_richness$type == timeDay,] %>% dplyr::select(c(measure, ends_with(c("mean"))))
      
      #perform PCA on train
      PCA.train <- prcomp(trainData[2:ncol(trainData)], scale = TRUE)
      
      #select number of PCs to use (eigenvalues greater than 1? PCs to obtain >80 variance explained?)
      eig.val <- get_eigenvalue(PCA.train)
      
      #extract PCs
      train.PCA <- data.frame(trainData[1], PCA.train$x[,which(eig.val$eigenvalue >= 1)])
      
      #use same PCA on test?
      test.PCA <- data.frame(testData[1], predict(PCA.train, newdata = testData[2:ncol(testData)])[,which(eig.val$eigenvalue >= 1)])
      
      #Fit linear model
      f <- as.formula(paste(measure, paste(colnames(train.PCA)[2:ncol(train.PCA)], collapse = " + "), sep = " ~ "))
      
      model <- lm(f, data = train.PCA)
      
      #Make predictions on test set
      predictions <- model %>% predict(test.PCA)
      
      #Calculate model accuracy
      performance <- data.frame(rbind(data.frame(.metric = 'mae',
                                                 .estimate = yardstick::mae_vec(truth = testData[[1]], estimate = predictions)),
                                      data.frame(.metric = 'rmse',
                                                 .estimate = yardstick::rmse_vec(truth = testData[[1]], estimate = predictions))))
      
      results <- list(model = model,
                      predictions = predictions,
                      performance = performance)
      
      lm_pca_spatial[[paste0(measure, "_", timeDay, "_", region)]] <- results
    }
  }
}

AllPerformance_lm_pca_spatial <- lapply(lm_pca_spatial, function(x) x[['performance']]) %>% bind_rows(.id = "Iteration") %>% mutate(Measure = gsub("_.*", "", Iteration),
                                                                                                                                    TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration),
                                                                                                                                    Model = "lm_pca",
                                                                                                                                    Region = gsub(".*_[A-Za-z]*_([A-Z]{1,2})", "\\1", Iteration))

Performance_spatial <- ggplot(data = AllPerformance_lm_pca_spatial, 
                              aes(x = Measure, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = .10)) +
  scale_fill_viridis_d() +
  labs(x = "Time of Day", y = "Performance Estimate") +
  facet_wrap(~.metric) +
  theme_bw() +
  theme(legend.position = "none")


legend_bottom <- get_legend(
  Performance_spatial + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)

Plot_ModelPerformance_spatial <- plot_grid(Performance_spatial, legend_bottom,
                                           ncol = 1, 
                                           rel_heights = c(1, 0.1))

ggsave(filename = paste0("outputs/figures/modelperformance_lmpca/spatial.png"),
       Plot_ModelPerformance_spatial,
       width = 24, height = 12, units = "cm", dpi = 800)


# Temporal cross-validation ----

lm_pca_temporal <- list()
for (seasonyear in unique(acousticIndices_richness$seasonYear)) {
  
  for (timeDay in c('dawnChorus', 'solarNoon', 'eveningChorus', 'day')) {
    
    for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner')) {
      
      #split into train and test & filter to acoustic index columns
      trainData <- acousticIndices_richness[acousticIndices_richness$seasonYear != seasonyear & acousticIndices_richness$type == timeDay,] %>% dplyr::select(c(measure, ends_with(c("mean"))))
      testData <- acousticIndices_richness[acousticIndices_richness$seasonYear == seasonyear & acousticIndices_richness$type == timeDay,] %>% dplyr::select(c(measure, ends_with(c("mean"))))
      
      
      #perform PCA on train
      PCA.train <- prcomp(trainData[2:ncol(trainData)], scale = TRUE)
      
      #select number of PCs to use (eigenvalues greater than 1? PCs to obtain >80 variance explained?)
      eig.val <- get_eigenvalue(PCA.train)
      
      #extract PCs
      train.PCA <- data.frame(trainData[1], PCA.train$x[,which(eig.val$eigenvalue >= 1)])
      
      #use same PCA on test?
      test.PCA <- data.frame(testData[1], predict(PCA.train, newdata = testData[2:ncol(testData)])[,which(eig.val$eigenvalue >= 1)])
      
      #Fit linear model
      f <- as.formula(paste(measure, paste(colnames(train.PCA)[2:ncol(train.PCA)], collapse = " + "), sep = " ~ "))
      
      model <- lm(f, data = train.PCA)
      
      #Make predictions on test set
      #predictions <- model %>% predict(test.PCA)
      
      predictions <- bind_cols(testData[measure], .pred = predict(model, newdata = test.PCA))
      
      #Calculate model accuracy
      performance <- predictions %>% yardstick::metrics(truth = measure, estimate = .pred)
      
      #performance <- data.frame(rbind(data.frame(.metric = 'mae',
      #                                           .estimate = yardstick::mae_vec(truth = testData[[1]], estimate = predictions)),
      #                                data.frame(.metric = 'rmse',
      #                                           .estimate = yardstick::rmse_vec(truth = testData[[1]], estimate = predictions))))
      
      results <- list(model = model,
                      predictions = predictions,
                      performance = performance)
      
      lm_pca_temporal[[paste0(measure, "_", timeDay, "_", seasonyear)]] <- results
    }
  }
}

AllPerformance_lm_pca_temporal <- lapply(lm_pca_temporal, function(x) x[['performance']]) %>% bind_rows(.id = "Iteration") %>% mutate(Measure = gsub("_.*", "", Iteration),
                                                                                                                                      TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration),
                                                                                                                                      Model = "lm_pca",
                                                                                                                                      seasonYear = gsub(".*_[A-Za-z]*_([A-Z]{1,2})", "\\1", Iteration))

Performance_temporal <- ggplot(data = AllPerformance_lm_pca_temporal %>% filter(.metric != 'rsq'), 
                               aes(x = Measure, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = .10)) +
  scale_fill_viridis_d() +
  labs(x = "Time of Day", y = "Performance Estimate") +
  facet_wrap(~.metric) +
  theme_bw() +
  theme(legend.position = "none")


legend_bottom <- get_legend(
  Performance_temporal + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)

Plot_ModelPerformance_temporal <- plot_grid(Performance_temporal, legend_bottom,
                                           ncol = 1, 
                                           rel_heights = c(1, 0.1))

ggsave(filename = paste0("outputs/figures/modelperformance_lmpca/temporal.png"),
       Plot_ModelPerformance_temporal,
       width = 24, height = 12, units = "cm", dpi = 800)

#Scatterplots of test set observed vs predicted values
AllPredictions_lm_pca_temporal <- lapply(lm_pca_temporal, function(x) x[['predictions']]) %>% lapply(function(x) x %>% rename(obs = 1)) %>%  
  bind_rows(.id = "Iteration") %>% mutate(Measure = gsub("_.*", "", Iteration),
                                          TimePeriod = gsub(".*_([A-Za-z]*)_.*", "\\1", Iteration),
                                          Model = "lm_pca",
                                          seasonYear = gsub(".*_[A-Za-z]*_([A-Z]{1,2})", "\\1", Iteration))

Plots_Observed_Predicted_lm_pca_temporal <- list()
for (measure in unique(AllPredictions_lm_pca_temporal$Measure)) {
  for (timeDay in c('dawnChorus', 'solarNoon', 'eveningChorus', 'day')) {
    
    tmpdata <- AllPredictions_lm_pca_temporal %>%
      filter(TimePeriod == timeDay & Measure == measure)
    
    CCClabels <- tmpdata %>% 
      group_by(seasonYear) %>% 
      summarise(CCC = round(DescTools::CCC(.pred, obs)$rho.c$est, 2))
    
    Plots_Observed_Predicted_lm_pca_temporal[[paste0(measure, "_", timeDay)]] <- ggplot(data = tmpdata,
                                                                                    aes(x = .pred, y = obs)) +
      geom_point() +
      geom_abline(slope = 1, linetype = 'dashed') +
      scale_x_continuous(limits = c(min(tmpdata[,c(2:3)]), 
                                    max(tmpdata[,c(2:3)]))) +
      scale_y_continuous(limits = c(min(tmpdata[,c(2:3)]), 
                                    max(tmpdata[,c(2:3)]))) +
      geom_text(data = CCClabels, aes(x = 0.8*(max(tmpdata[,c(2:3)]) -
                                                 min(tmpdata[,c(2:3)])) + 
                                        min(tmpdata[,c(2:3)]), 
                                      y = min(tmpdata[,c(2:3)]), 
                                      label = CCC)) +
      labs(x = "Predicted", y = "Observed") +
      facet_wrap(~seasonYear) +
      theme_bw()
    
    ggsave(filename = paste0("outputs/figures/observedvspredicted_lmpca/temporal_", measure, "_", timeDay, ".png"),
           Plots_Observed_Predicted_lm_pca_temporal[[paste0(measure, "_", timeDay)]],
           width = 18, height = 18, units = "cm", dpi = 800) 
  }
}
