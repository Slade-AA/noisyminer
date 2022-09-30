library(tidyverse)
library(MASS) #lda function
library(factoextra) #pca tools
library(yardstick) #classification performance metrics
library(ggpubr)
library(cowplot)

# Load indices and biodiversity data ----

acousticIndices_biodiversity <- readRDS("outputs/data/acousticIndices_biodiversity.RDS")

# ├ Create region column ----
acousticIndices_biodiversity$R1Only$Region <- gsub("[0-9]", "", acousticIndices_biodiversity$R1Only$Site)
acousticIndices_biodiversity$R1R2Combined$Region <- gsub("[0-9]", "", acousticIndices_biodiversity$R1R2Combined$Site)

#Vector of indices that are specifcally to be used for training Noisy miner models and not general bird models
noisyMinerIndices <- c('CVR_1166_3646_mean', 'CVR_0_1166_3646_11025_mean', 'CVR_ND_mean', #CVR indices based on frequency range of 'chur' vocalisation
                       'ENT_1166_3646_mean', 'ENT_0_1166_3646_11025_mean', 'ENT_ND_mean') #ENT indices based on frequency range of 'chur' vocalisation

# ├├ Custom performance metric set ----
custom_metrics <- metric_set(accuracy, bal_accuracy, kap, precision, recall, f_meas)

# PCA plots per Time of Day ----

# ├ R1Only ----
for (measure in c("NMPresent", "DetectedMiner20", "DetectedMiner40", "Threshold20m", "Threshold40m")) {
  PCAPlots_R1Only <- list()
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    data <- acousticIndices_biodiversity$R1Only %>% 
      filter(type == timeDay) %>% 
      dplyr::select(c(measure, ends_with(c("mean"))))
    
    PCA <- prcomp(data[2:ncol(data)], scale = TRUE)
    
    PCAPlots_R1Only[[timeDay]] <- fviz_pca_ind(PCA, habillage = data[[measure]], label = "none", addEllipses = TRUE) + 
      scale_color_brewer(palette = "Dark2") + 
      theme_minimal() +
      theme(plot.title = element_blank())
  }
  
  plot_grid(plotlist = PCAPlots_R1Only,
            labels = "AUTO") %>% 
    annotate_figure(top = paste0("R1Only - ", measure)) %>% 
    ggsave(filename = paste0("outputs/figures/pca/", "R1Only - ", measure, ".png"))
}

# ├├ Per Year ----
for (measure in c("Threshold40m")) {
  for (year in c("2019", "2020", "2021")) {
    PCAPlots_R1Only <- list()
    for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
      data <- acousticIndices_biodiversity$R1Only %>% 
        filter(type == timeDay & Year == year) %>% 
        dplyr::select(c(measure, ends_with(c("mean"))))
      
      PCA <- prcomp(data[2:ncol(data)], scale = TRUE)
      
      PCAPlots_R1Only[[timeDay]] <- fviz_pca_ind(PCA, habillage = data[[measure]], label = "none", addEllipses = TRUE) + 
        scale_color_brewer(palette = "Dark2") + 
        theme_minimal() +
        theme(plot.title = element_blank())
    }
    
    plot_grid(plotlist = PCAPlots_R1Only,
              labels = "AUTO") %>% 
      annotate_figure(top = paste0("R1Only - ", year, " - ", measure)) %>% 
      ggsave(filename = paste0("outputs/figures/pca/", "R1Only - ", year, "_", measure, ".png"))
  }
}


# ├├ Reduced set of acoutic indices ----
indicesToUse <- c("BI_mean", "H_mean", "ADI_mean", "NDSI_soundecology_mean", "ACI_soundecology_mean",
                  "CVR_1166_3646_mean", "SptDensity_mean", "LowFreqCover_mean", "MidFreqCover_mean", "HighFreqCover_mean")

for (measure in c("NMPresent", "DetectedMiner20", "DetectedMiner40", "Threshold20m", "Threshold40m")) {
  PCAPlots_R1Only <- list()
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    data <- acousticIndices_biodiversity$R1Only %>% 
      filter(type == timeDay) %>% 
      dplyr::select(c(measure, any_of(indicesToUse)))
    
    PCA <- prcomp(data[2:ncol(data)], scale = TRUE)
    
    PCAPlots_R1Only[[timeDay]] <- fviz_pca_ind(PCA, habillage = data[[measure]], label = "none", addEllipses = TRUE) + 
      scale_color_brewer(palette = "Dark2") + 
      theme_minimal() +
      theme(plot.title = element_blank())
  }
  
  plot_grid(plotlist = PCAPlots_R1Only,
            labels = "AUTO") %>% 
    annotate_figure(top = paste0("R1Only - ", measure)) %>% 
    ggsave(filename = paste0("outputs/figures/pca/", "R1Only - ReducedSet - ", measure, ".png"))
}



# ├ R1R2Combined ----
for (measure in c("NMPresent", "Threshold20m", "Threshold40m")) {
  PCAPlots_R1R2Combined <- list()
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    data <- acousticIndices_biodiversity$R1R2Combined %>% 
      filter(type == timeDay) %>% 
      dplyr::select(c(measure, ends_with(c("mean"))))
    
    PCA <- prcomp(data[2:ncol(data)], scale = TRUE)
    
    PCAPlots_R1R2Combined[[timeDay]] <- fviz_pca_ind(PCA, habillage = data[[measure]], label = "none", addEllipses = TRUE) + 
      scale_color_brewer(palette = "Dark2") + 
      theme_minimal() +
      theme(plot.title = element_blank())
  }
  
  plot_grid(plotlist = PCAPlots_R1R2Combined,
            labels = "AUTO") %>% 
    annotate_figure(top = paste0("R1R2Combined - ", measure)) %>% 
    ggsave(filename = paste0("outputs/figures/pca/", "R1R2Combined - ", measure, ".png"))
}

# ├├ Per Year ----
for (measure in c("Threshold40m")) {
  for (year in c("2019", "2020", "2021")) {
    PCAPlots_R1R2Combined <- list()
    for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
      data <- acousticIndices_biodiversity$R1R2Combined %>% 
        filter(type == timeDay & Year == year) %>% 
        dplyr::select(c(measure, ends_with(c("mean"))))
      
      PCA <- prcomp(data[2:ncol(data)], scale = TRUE)
      
      PCAPlots_R1R2Combined[[timeDay]] <- fviz_pca_ind(PCA, habillage = data[[measure]], label = "none", addEllipses = TRUE) + 
        scale_color_brewer(palette = "Dark2") + 
        theme_minimal() +
        theme(plot.title = element_blank())
    }
    
    plot_grid(plotlist = PCAPlots_R1R2Combined,
              labels = "AUTO") %>% 
      annotate_figure(top = paste0("R1R2Combined - ", year, " - ", measure)) %>% 
      ggsave(filename = paste0("outputs/figures/pca/", "R1R2Combined - ", year, "_", measure, ".png"))
  }
}


# ├├ Per Region ----
for (measure in c("NMPresent", "Threshold20m", "Threshold40m")) {
  PCAPlots_R1R2Combined <- list()
  for (region in unique(acousticIndices_biodiversity$R1R2Combined$Region)) {
    data <- acousticIndices_biodiversity$R1R2Combined %>% 
      filter(type == 'solarNoon' & Region == region) %>% 
      dplyr::select(c(measure, ends_with(c("mean"))))
    
    PCA <- prcomp(data[2:ncol(data)], scale = TRUE)
    
    PCAPlots_R1R2Combined[[region]] <- fviz_pca_ind(PCA, habillage = data[[measure]], label = "none", addEllipses = TRUE) + 
      scale_color_brewer(palette = "Dark2") + 
      theme_minimal() +
      theme(plot.title = element_blank())
  }
  
  plot_grid(plotlist = PCAPlots_R1R2Combined,
            labels = "AUTO") %>% 
    annotate_figure(top = paste0("R1R2Combined - ", measure)) %>% 
    ggsave(filename = paste0("outputs/figures/pca/", "R1R2Combined - Regions - ", measure, ".png"))
}



# Spatial cross-validation ----

# ├ R1Only ----

lda_pca_spatial_R1Only <- list()
for (region in unique(acousticIndices_biodiversity$R1Only$Region)) {
  
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    
    #split into train and test & filter to acoustic index columns
    trainData <- acousticIndices_biodiversity$R1Only[acousticIndices_biodiversity$R1Only$Region != region & acousticIndices_biodiversity$R1Only$type == timeDay,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))
    testData <- acousticIndices_biodiversity$R1Only[acousticIndices_biodiversity$R1Only$Region == region & acousticIndices_biodiversity$R1Only$type == timeDay,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))
    
    trainData$Threshold40m <- factor(trainData$Threshold40m, levels = c('0', '1'))
    testData$Threshold40m <- factor(testData$Threshold40m, levels = c('0', '1'))
    
    #perform PCA on train
    PCA.train <- prcomp(trainData[2:ncol(trainData)], scale = TRUE)
    
    #select number of PCs to use (eigenvalues greater than 1? PCs to obtain >80 variance explained?)
    eig.val <- get_eigenvalue(PCA.train)
    
    #extract PCs
    train.PCA <- data.frame(Threshold40m = as.character(trainData$Threshold40m), PCA.train$x[,which(eig.val$eigenvalue >= 1)])
    
    #use same PCA on test?
    test.PCA <- data.frame(Threshold40m = as.character(testData$Threshold40m), predict(PCA.train, newdata = testData[2:ncol(testData)])[,which(eig.val$eigenvalue >= 1)])
    
    #Fit lda model
    model <- lda(Threshold40m~., data = train.PCA)
    
    #Make predictions on test set
    predictions <- model %>% predict(test.PCA)
    
    #observed and predicted
    obs_pred <- data.frame(observed = testData$Threshold40m, predicted = predictions$class)
    
    #Calculate model accuracy
    performance <- obs_pred %>% custom_metrics(truth = observed, estimate = predicted, event_level = "second")
    
    #performance <- data.frame(rbind(data.frame(.metric = 'accuracy',
    #                                           .estimate = mean(predictions$class==testData$Threshold40m)),
    #                                data.frame(.metric = 'bal_accuracy',
    #                                           .estimate = yardstick::bal_accuracy_vec(truth = testData$Threshold40m, estimate = predictions$class)),
    #                                data.frame(.metric = 'kappa',
    #                                           .estimate = yardstick::kap_vec(truth = testData$Threshold40m, estimate = predictions$class))))
    
    results <- list(model = model,
                    predictions = predictions,
                    obs_pred = obs_pred,
                    performance = performance)
    
    lda_pca_spatial_R1Only[[paste0(timeDay, "_", region)]] <- results
  }
}

#Calculate performance on whole dataset
AllPredictions_lda_spatial_R1Only <- lapply(lda_pca_spatial_R1Only, function(x) x[['obs_pred']]) %>% 
  bind_rows(.id = "Iteration") %>% 
  mutate(TimePeriod = gsub("^([A-Za-z]*)_.*", "\\1", Iteration),
         Model = "lda_pca",
         Region = gsub(".*_([A-Z]{1,2})", "\\1", Iteration))

PerfMetrics_lda_spatial_R1Only <- AllPredictions_lda_spatial_R1Only %>% 
  group_by(TimePeriod) %>% 
  custom_metrics(truth = observed, estimate = predicted, event_level = "second")




AllPerformance_lda_spatial <- lapply(lda_pca_spatial_R1Only, function(x) x[['performance']]) %>% 
  bind_rows(.id = "Iteration") %>% 
  mutate(TimePeriod = gsub("^([A-Za-z]*)_.*", "\\1", Iteration),
         Model = "lda_pca",
         Region = gsub(".*_([A-Z]{1,2})", "\\1", Iteration))

Performance_spatial <- ggplot(data = AllPerformance_lda_spatial, 
                              aes(x = TimePeriod, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = .10)) +
  scale_fill_viridis_d() +
  labs(x = "Time of Day", y = "Performance Estimate") +
  facet_wrap(~.metric) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "outputs/figures/modelperformance_ldapca/spatial_R1Only.png",
       plot = Performance_spatial,
       width = 24, height = 16, units = "cm", dpi = 800)

Performance_spatial_bal_accuracy <- ggplot(data = AllPerformance_lda_spatial[AllPerformance_lda_spatial$.metric == 'bal_accuracy',], 
                                           aes(x = TimePeriod, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = .10)) +
  scale_fill_viridis_d() +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Time of Day", y = "Balanced Accuracy") +
  #facet_wrap(~.metric) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "outputs/figures/modelperformance_ldapca/spatial_R1Only_bal_accuracy.png",
       plot = Performance_spatial_bal_accuracy,
       width = 8, height = 8, units = "cm", dpi = 800)


# ├ R1R2Combined ----

# ├ Best Indices Across Time ----

#Load data generated from bootstrap correlations
BestIndicesPerTimePeriod <- readRDS("outputs/figures/bootstrapcorrelations/BestIndicesPerTimePeriod.RDS")

BestIndicesPerTimePeriod$Region <- gsub("[0-9]", "", BestIndicesPerTimePeriod$Site)

lda_pca_spatial_bestTime <- list()
for (region in unique(BestIndicesPerTimePeriod$Region)) {
  
  #split into train and test & filter to acoustic index columns
  trainData <- BestIndicesPerTimePeriod[BestIndicesPerTimePeriod$Region != region,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))
  testData <- BestIndicesPerTimePeriod[BestIndicesPerTimePeriod$Region == region,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))
  
  trainData$Threshold40m <- factor(trainData$Threshold40m, levels = c('0', '1'))
  testData$Threshold40m <- factor(testData$Threshold40m, levels = c('0', '1'))
  
  #perform PCA on train
  PCA.train <- prcomp(trainData[2:ncol(trainData)], scale = TRUE)
  
  #select number of PCs to use (eigenvalues greater than 1? PCs to obtain >80 variance explained?)
  eig.val <- get_eigenvalue(PCA.train)
  
  #extract PCs
  train.PCA <- data.frame(Threshold40m = as.character(trainData$Threshold40m), PCA.train$x[,which(eig.val$eigenvalue >= 1)])
  
  #use same PCA on test?
  test.PCA <- data.frame(Threshold40m = as.character(testData$Threshold40m), predict(PCA.train, newdata = testData[2:ncol(testData)])[,which(eig.val$eigenvalue >= 1)])
  
  #Fit lda model
  model <- lda(Threshold40m~., data = train.PCA)
  
  #Make predictions on test set
  predictions <- model %>% predict(test.PCA)
  
  #observed and predicted
  obs_pred <- data.frame(observed = testData$Threshold40m, predicted = predictions$class)
  
  #Calculate model accuracy
  performance <- obs_pred %>% custom_metrics(truth = observed, estimate = predicted, event_level = "second")
  
  results <- list(model = model,
                  predictions = predictions,
                  obs_pred = obs_pred,
                  performance = performance)
  
  lda_pca_spatial_bestTime[[paste0(region)]] <- results
}


#Calculate performance on whole dataset
AllPredictions_lda_spatial_bestTime <- lapply(lda_pca_spatial_bestTime, function(x) x[['obs_pred']]) %>% 
  bind_rows(.id = "Region") %>% 
  mutate(Model = "lda_pca")

PerfMetrics_lda_spatial_bestTime <- AllPredictions_lda_spatial_bestTime %>% 
  custom_metrics(truth = observed, estimate = predicted, event_level = "second")




AllPerformance_lda_spatial <- lapply(lda_pca_spatial_bestTime, function(x) x[['performance']]) %>% 
  bind_rows(.id = "Region") %>% 
  mutate(Model = "lda_pca")

Performance_spatial <- ggplot(data = AllPerformance_lda_spatial, 
                              aes(x = Model, y = .estimate, fill = Model)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = .10)) +
  scale_fill_viridis_d() +
  labs(x = "Time of Day", y = "Performance Estimate") +
  facet_wrap(~.metric) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "outputs/figures/modelperformance_ldapca/spatial_bestTime.png",
       plot = Performance_spatial,
       width = 24, height = 16, units = "cm", dpi = 800)


# Temporal cross-validation ----

# ├ R1Only ----

lda_pca_temporal_R1Only <- list()
for (seasonyear in unique(acousticIndices_biodiversity$R1Only$SeasonYear)) {
  
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    
    #split into train and test & filter to acoustic index columns
    trainData <- acousticIndices_biodiversity$R1Only[acousticIndices_biodiversity$R1Only$SeasonYear != seasonyear & acousticIndices_biodiversity$R1Only$type == timeDay,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))
    testData <- acousticIndices_biodiversity$R1Only[acousticIndices_biodiversity$R1Only$SeasonYear == seasonyear & acousticIndices_biodiversity$R1Only$type == timeDay,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))
    
    trainData$Threshold40m <- factor(trainData$Threshold40m, levels = c('0', '1'))
    testData$Threshold40m <- factor(testData$Threshold40m, levels = c('0', '1'))
    
    #perform PCA on train
    PCA.train <- prcomp(trainData[2:ncol(trainData)], scale = TRUE)
    
    #select number of PCs to use (eigenvalues greater than 1? PCs to obtain >80 variance explained?)
    eig.val <- get_eigenvalue(PCA.train)
    
    #extract PCs
    train.PCA <- data.frame(Threshold40m = as.character(trainData$Threshold40m), PCA.train$x[,which(eig.val$eigenvalue >= 1)])
    
    #use same PCA on test?
    test.PCA <- data.frame(Threshold40m = as.character(testData$Threshold40m), predict(PCA.train, newdata = testData[2:ncol(testData)])[,which(eig.val$eigenvalue >= 1)])
    
    #Fit lda model
    model <- lda(Threshold40m~., data = train.PCA)
    
    #Make predictions on test set
    predictions <- model %>% predict(test.PCA)
    
    #observed and predicted
    obs_pred <- data.frame(observed = testData$Threshold40m, predicted = predictions$class)
    
    #Calculate model accuracy
    performance <- obs_pred %>% custom_metrics(truth = observed, estimate = predicted, event_level = "second")
    
    results <- list(model = model,
                    predictions = predictions,
                    obs_pred = obs_pred,
                    performance = performance)
    
    lda_pca_temporal_R1Only[[paste0(timeDay, "_", seasonyear)]] <- results
  }
}

#Calculate performance on whole dataset
AllPredictions_lda_temporal_R1Only <- lapply(lda_pca_temporal_R1Only, function(x) x[['obs_pred']]) %>% 
  bind_rows(.id = "Iteration") %>% 
  mutate(TimePeriod = gsub("^([A-Za-z]*)_.*", "\\1", Iteration),
         Model = "lda_pca",
         SeasonYear = gsub(".*_([A-Z]{1,2})", "\\1", Iteration))

PerfMetrics_lda_temporal_R1Only <- AllPredictions_lda_temporal_R1Only %>% 
  group_by(TimePeriod) %>% 
  custom_metrics(truth = observed, estimate = predicted, event_level = "second")




AllPerformance_lda_temporal <- lapply(lda_pca_temporal_R1Only, function(x) x[['performance']]) %>% 
  bind_rows(.id = "Iteration") %>% 
  mutate(TimePeriod = gsub("^([A-Za-z]*)_.*", "\\1", Iteration),
         Model = "lda_pca",
         SeasonYear = gsub(".*_([A-Z]{1,2})", "\\1", Iteration))

Performance_temporal <- ggplot(data = AllPerformance_lda_temporal, 
                               aes(x = TimePeriod, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = .10)) +
  scale_fill_viridis_d() +
  labs(x = "Time of Day", y = "Performance Estimate") +
  facet_wrap(~.metric) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "outputs/figures/modelperformance_ldapca/temporal_R1Only.png",
       plot = Performance_temporal,
       width = 24, height = 16, units = "cm", dpi = 800)

# ├ R1R2Combined ----

lda_pca_temporal_R1R2Combined <- list()
for (seasonyear in unique(acousticIndices_biodiversity$R1R2Combined$SeasonYear)) {
  
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    
    #split into train and test & filter to acoustic index columns
    trainData <- acousticIndices_biodiversity$R1R2Combined[acousticIndices_biodiversity$R1R2Combined$SeasonYear != seasonyear & acousticIndices_biodiversity$R1R2Combined$type == timeDay,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))
    testData <- acousticIndices_biodiversity$R1R2Combined[acousticIndices_biodiversity$R1R2Combined$SeasonYear == seasonyear & acousticIndices_biodiversity$R1R2Combined$type == timeDay,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))
    
    trainData$Threshold40m <- factor(trainData$Threshold40m, levels = c('0', '1'))
    testData$Threshold40m <- factor(testData$Threshold40m, levels = c('0', '1'))
    
    #perform PCA on train
    PCA.train <- prcomp(trainData[2:ncol(trainData)], scale = TRUE)
    
    #select number of PCs to use (eigenvalues greater than 1? PCs to obtain >80 variance explained?)
    eig.val <- get_eigenvalue(PCA.train)
    
    #extract PCs
    train.PCA <- data.frame(Threshold40m = as.character(trainData$Threshold40m), PCA.train$x[,which(eig.val$eigenvalue >= 1)])
    
    #use same PCA on test?
    test.PCA <- data.frame(Threshold40m = as.character(testData$Threshold40m), predict(PCA.train, newdata = testData[2:ncol(testData)])[,which(eig.val$eigenvalue >= 1)])
    
    #Fit lda model
    model <- lda(Threshold40m~., data = train.PCA)
    
    #Make predictions on test set
    predictions <- model %>% predict(test.PCA)
    
    #observed and predicted
    obs_pred <- data.frame(observed = testData$Threshold40m, predicted = predictions$class)
    
    #Calculate model accuracy
    performance <- obs_pred %>% custom_metrics(truth = observed, estimate = predicted, event_level = "second")
    
    results <- list(model = model,
                    predictions = predictions,
                    obs_pred = obs_pred,
                    performance = performance)
    
    lda_pca_temporal_R1R2Combined[[paste0(timeDay, "_", seasonyear)]] <- results
  }
}

#Calculate performance on whole dataset
AllPredictions_lda_temporal_R1R2Combined <- lapply(lda_pca_temporal_R1R2Combined, function(x) x[['obs_pred']]) %>% 
  bind_rows(.id = "Iteration") %>% 
  mutate(TimePeriod = gsub("^([A-Za-z]*)_.*", "\\1", Iteration),
         Model = "lda_pca",
         SeasonYear = gsub(".*_([A-Z]{1,2})", "\\1", Iteration))

PerfMetrics_lda_temporal_R1R2Combined <- AllPredictions_lda_temporal_R1R2Combined %>% 
  group_by(TimePeriod) %>% 
  custom_metrics(truth = observed, estimate = predicted, event_level = "second")




AllPerformance_lda_temporal <- lapply(lda_pca_temporal_R1R2Combined, function(x) x[['performance']]) %>% 
  bind_rows(.id = "Iteration") %>% 
  mutate(TimePeriod = gsub("^([A-Za-z]*)_.*", "\\1", Iteration),
         Model = "lda_pca",
         SeasonYear = gsub(".*_([A-Z]{1,2})", "\\1", Iteration))

Performance_temporal <- ggplot(data = AllPerformance_lda_temporal, 
                               aes(x = TimePeriod, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = .10)) +
  scale_fill_viridis_d() +
  labs(x = "Time of Day", y = "Performance Estimate") +
  facet_wrap(~.metric) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "outputs/figures/modelperformance_ldapca/temporal_R1R2Combined.png",
       plot = Performance_temporal,
       width = 24, height = 16, units = "cm", dpi = 800)



# Best Indices Across Time Periods ----

#Load data generated from bootstrap correlations
BestIndicesPerTimePeriod <- readRDS("outputs/figures/bootstrapcorrelations/BestIndicesPerTimePeriod.RDS")

#PCA plot
data <- BestIndicesPerTimePeriod %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))
PCA_BestIndicesPerTimePeriod <- prcomp(data[2:ncol(data)], scale = TRUE)

PCAPlots_BestIndicesPerTimePeriod <- fviz_pca_ind(PCA_BestIndicesPerTimePeriod, habillage = data[['Threshold40m']], label = "none", addEllipses = TRUE) + 
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(plot.title = element_blank())

ggsave("outputs/figures/pca/BestTimePeriodPerIndex_Threshold40m.png", PCAPlots_BestIndicesPerTimePeriod,
       dpi = 800, width = 11, height = 9, units = "cm")

#split into train and test & filter to acoustic index columns
train <- caret::createDataPartition(BestIndicesPerTimePeriod$Threshold40m, p = 0.8, list = F)

trainData <- BestIndicesPerTimePeriod[train,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))
testData <- BestIndicesPerTimePeriod[-train,] %>% dplyr::select(c('Threshold40m', ends_with(c("mean"))))

trainData$Threshold40m <- factor(trainData$Threshold40m, levels = c('0', '1'))
testData$Threshold40m <- factor(testData$Threshold40m, levels = c('0', '1'))

#perform PCA on train
PCA.train <- prcomp(trainData[2:ncol(trainData)], scale = TRUE)

#select number of PCs to use (eigenvalues greater than 1? PCs to obtain >80 variance explained?)
eig.val <- get_eigenvalue(PCA.train)

#extract PCs
train.PCA <- data.frame(Threshold40m = as.character(trainData$Threshold40m), PCA.train$x[,which(eig.val$eigenvalue >= 1)])

#use same PCA on test?
test.PCA <- data.frame(Threshold40m = as.character(testData$Threshold40m), predict(PCA.train, newdata = testData[2:ncol(testData)])[,which(eig.val$eigenvalue >= 1)])

#Fit lda model
model <- lda(Threshold40m~., data = train.PCA)

#Make predictions on test set
predictions <- model %>% predict(test.PCA)

#observed and predicted
obs_pred <- data.frame(observed = testData$Threshold40m, predicted = predictions$class)

#Calculate model accuracy
performance <- obs_pred %>% custom_metrics(truth = observed, estimate = predicted, event_level = "second")
