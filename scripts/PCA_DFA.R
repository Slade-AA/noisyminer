library(tidyverse)
library(MASS) #lda function
library(factoextra) #pca tools

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

lda_pca_spatial <- list()
for (region in unique(acousticIndices_richness$Region)) {
  
  for (timeDay in c('dawnChorus', 'solarNoon', 'eveningChorus', 'day')) {
    
    #split into train and test & filter to acoustic index columns
    trainData <- acousticIndices_richness[acousticIndices_richness$Region != region & acousticIndices_richness$type == timeDay,] %>% dplyr::select(c('NoisyPreAbs', ends_with(c("mean"))))
    testData <- acousticIndices_richness[acousticIndices_richness$Region == region & acousticIndices_richness$type == timeDay,] %>% dplyr::select(c('NoisyPreAbs', ends_with(c("mean"))))
    
    trainData$NoisyPreAbs <- factor(trainData$NoisyPreAbs, levels = c('0', '1'))
    testData$NoisyPreAbs <- factor(testData$NoisyPreAbs, levels = c('0', '1'))
    
    #perform PCA on train
    PCA.train <- prcomp(trainData[2:ncol(trainData)], scale = TRUE)
    
    #select number of PCs to use (eigenvalues greater than 1? PCs to obtain >80 variance explained?)
    eig.val <- get_eigenvalue(PCA.train)
    
    #extract PCs
    train.PCA <- data.frame(NoisyPreAbs = as.character(trainData$NoisyPreAbs), PCA.train$x[,which(eig.val$eigenvalue >= 1)])
    
    #use same PCA on test?
    test.PCA <- data.frame(NoisyPreAbs = as.character(testData$NoisyPreAbs), predict(PCA.train, newdata = testData[2:ncol(testData)])[,which(eig.val$eigenvalue >= 1)])
    
    #Fit lda model
    model <- lda(NoisyPreAbs~., data = train.PCA)
    
    #Make predictions on test set
    predictions <- model %>% predict(test.PCA)
    
    #Calculate model accuracy
    performance <- data.frame(rbind(data.frame(.metric = 'accuracy',
                                               .estimate = mean(predictions$class==testData$NoisyPreAbs)),
                                    data.frame(.metric = 'bal_accuracy',
                                               .estimate = yardstick::bal_accuracy_vec(truth = testData$NoisyPreAbs, estimate = predictions$class)),
                                    data.frame(.metric = 'kappa',
                                               .estimate = yardstick::kap_vec(truth = testData$NoisyPreAbs, estimate = predictions$class))))
    
    results <- list(model = model,
                    predictions = predictions,
                    performance = performance)
    
    lda_pca_spatial[[paste0(timeDay, "_", region)]] <- results
  }
}

AllPerformance_lda_spatial <- lapply(lda_pca_spatial, function(x) x[['performance']]) %>% bind_rows(.id = "Iteration") %>% mutate(TimePeriod = gsub("^([A-Za-z]*)_.*", "\\1", Iteration),
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

ggsave(filename = "outputs/figures/modelperformance_ldapca/spatial.png",
       plot = Performance_spatial,
       width = 24, height = 16, units = "cm", dpi = 800)


# Temporal cross-validation ----

lda_pca_temporal <- list()
for (seasonyear in unique(acousticIndices_richness$seasonYear)) {
  
  for (timeDay in c('dawnChorus', 'solarNoon', 'eveningChorus', 'day')) {
    
    #split into train and test & filter to acoustic index columns
    trainData <- acousticIndices_richness[acousticIndices_richness$seasonYear != seasonyear & acousticIndices_richness$type == timeDay,] %>% dplyr::select(c('NoisyPreAbs', ends_with(c("mean"))))
    testData <- acousticIndices_richness[acousticIndices_richness$seasonYear == seasonyear & acousticIndices_richness$type == timeDay,] %>% dplyr::select(c('NoisyPreAbs', ends_with(c("mean"))))
    
    trainData$NoisyPreAbs <- factor(trainData$NoisyPreAbs, levels = c('0', '1'))
    testData$NoisyPreAbs <- factor(testData$NoisyPreAbs, levels = c('0', '1'))
    
    #perform PCA on train
    PCA.train <- prcomp(trainData[2:ncol(trainData)], scale = TRUE)
    
    #select number of PCs to use (eigenvalues greater than 1? PCs to obtain >80 variance explained?)
    eig.val <- get_eigenvalue(PCA.train)
    
    #extract PCs
    train.PCA <- data.frame(NoisyPreAbs = as.character(trainData$NoisyPreAbs), PCA.train$x[,which(eig.val$eigenvalue >= 1)])
    
    #use same PCA on test?
    test.PCA <- data.frame(NoisyPreAbs = as.character(testData$NoisyPreAbs), predict(PCA.train, newdata = testData[2:ncol(testData)])[,which(eig.val$eigenvalue >= 1)])
    
    #Fit lda model
    model <- lda(NoisyPreAbs~., data = train.PCA)
    
    #Make predictions on test set
    predictions <- model %>% predict(test.PCA)
    
    #Calculate model accuracy
    performance <- data.frame(rbind(data.frame(.metric = 'accuracy',
                                               .estimate = mean(predictions$class==testData$NoisyPreAbs)),
                                    data.frame(.metric = 'bal_accuracy',
                                               .estimate = yardstick::bal_accuracy_vec(truth = testData$NoisyPreAbs, estimate = predictions$class)),
                                    data.frame(.metric = 'kappa',
                                               .estimate = yardstick::kap_vec(truth = testData$NoisyPreAbs, estimate = predictions$class))))
    
    results <- list(model = model,
                    predictions = predictions,
                    performance = performance)
    
    lda_pca_temporal[[paste0(timeDay, "_", seasonyear)]] <- results
  }
}

AllPerformance_lda_temporal <- lapply(lda_pca_temporal, function(x) x[['performance']]) %>% bind_rows(.id = "Iteration") %>% mutate(TimePeriod = gsub("^([A-Za-z]*)_.*", "\\1", Iteration),
                                                                                                                                    Model = "lda_pca",
                                                                                                                                    seasonYear = gsub(".*_([A-Z]{1,2})", "\\1", Iteration))

Performance_temporal <- ggplot(data = AllPerformance_lda_temporal, 
                               aes(x = TimePeriod, y = .estimate, fill = TimePeriod)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = .10)) +
  scale_fill_viridis_d() +
  labs(x = "Time of Day", y = "Performance Estimate") +
  facet_wrap(~.metric) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "outputs/figures/modelperformance_ldapca/temporal.png",
       plot = Performance_temporal,
       width = 24, height = 16, units = "cm", dpi = 800)