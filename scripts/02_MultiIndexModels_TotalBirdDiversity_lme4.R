library(tidyverse)
library(randomForest)
library(lme4)
library(lmerTest)
library(caret)


# Load in indices ----
Indices_Summary <- readRDS("outputs/Indices_Summary_noOutlierRemoval_2024-08-02.rds")

Indices_R <- readRDS("outputs/Indices_R_noOutlierRemoval_2024-08-02.rds")

#Combine both sets of indices
allSummary <- left_join(Indices_Summary,
                        Indices_R %>% 
                          dplyr::select(c('Site', 'SurveyIDR12', 'type', 'audioDays', starts_with(c("ADI", "BI", "M", "H"))))) %>% 
  drop_na()

#Fit models
MixedModelPerformance <- data.frame()

VariableImportance <- data.frame()

set.seed(1234)
for (numDays in seq(1, 8)) {
  
  for (rep in 1:30) {
    model_data <- allSummary %>% filter(type == 'dawn' & audioDays == numDays)
    
    #split into train and test & filter to acoustic index columns
    #train <- createDataPartition(model_data$Detected40, p = 0.8, list = F)
    train <- createDataPartition(model_data$Site, p = 0.8, list = F) #create data partition using the site variable instead of the response variable
    
    trainData <- model_data[train,] %>% dplyr::select(c('Detected40', 'Site', ends_with(c("median"))))
    testData <- model_data[-train,] %>% dplyr::select(c('Detected40', 'Site', ends_with(c("median"))))  
    
    predictorVariables <- c("ADI_median",
                            "BI_median",
                            "M_median",
                            "H_median",
                            "Activity_median",
                            "EventsPerSecond_median",
                            "HighFreqCover_median",
                            "MidFreqCover_median",
                            "LowFreqCover_median",
                            "AcousticComplexity_median",
                            "TemporalEntropy_median",
                            "ClusterCount_median",
                            "Ndsi_median",
                            "SptDensity_median")
    
    modelFormula <- as.formula(paste(paste("Detected40", paste(predictorVariables, sep = "", collapse = " + "), sep = " ~ "), "(1|Site)", sep = " + "))
  
    model_lmer <- lmer(modelFormula,
                       data = trainData)
    
    #Make predictions on test set
    predictions_lmer <- model_lmer %>% predict(testData, allow.new.levels = TRUE)
    
    #observed and predicted
    obs_pred_lmer <- data.frame(observed = testData$Detected40, predicted = predictions_lmer)
    
    fullModelCCC <- DescTools::CCC(obs_pred_lmer$observed, obs_pred_lmer$predicted)$rho.c
    
    MixedModelPerformance <- bind_rows(MixedModelPerformance,
                                       data.frame(NumDays = numDays,
                                                  Rep = rep,
                                                  fullModelCCC))
    
    
    ggplot(data = obs_pred_lmer, aes(x = predicted, y = observed)) + 
      geom_point() +
      geom_abline(slope = 1) +
      tune::coord_obs_pred() +
      theme_bw()
    
    for (acousticIndex in predictorVariables) {
      reducedPredictorVariables <- predictorVariables[!predictorVariables %in% acousticIndex]
      
      modelFormula <- as.formula(paste(paste("Detected40", paste(reducedPredictorVariables, sep = "", collapse = " + "), sep = " ~ "), "(1|Site)", sep = " + "))
      
      model_lmer <- lmer(modelFormula,
                         data = trainData)
      
      #Make predictions on test set
      predictions_lmer <- model_lmer %>% predict(testData, allow.new.levels = TRUE)
      
      #observed and predicted
      obs_pred_lmer <- data.frame(observed = testData$Detected40, predicted = predictions_lmer)
      
      VariableImportance <- bind_rows(VariableImportance,
                                      data.frame(NumDays = numDays,
                                                 Rep = rep,
                                                 AcousticIndex = acousticIndex,
                                                 FullModelCCC = fullModelCCC$est,
                                                 ReducedModelCCC = DescTools::CCC(obs_pred_lmer$observed, obs_pred_lmer$predicted)$rho.c$est))
    }
  }
  
}

#Plot results

#Concordance correlation coefficient
ggplot(data = MixedModelPerformance, aes(x = factor(NumDays), y = est)) +
  geom_boxplot() +
  labs(x = 'Number of Audio Days', y = 'Concordance Correlation Coefficient (CCC)') +
  theme_bw()


ggsave(file = "outputs/figures_2024_totalBirdDiversity/CCC_lmer.png",
       dpi = 800, width = 12, height = 12, units = "cm")


#Change in CCC
VariableImportance <- VariableImportance %>% 
  mutate(CCCDifference = FullModelCCC - ReducedModelCCC,
         AcousticIndex = gsub("_median", "", AcousticIndex))


ggplot(data = VariableImportance %>% filter(NumDays == 8),
       aes(x = CCCDifference, y = reorder(AcousticIndex, CCCDifference, median))) +
  geom_boxplot() +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(x = "Change in CCC", y = "Acoustic Index") +
  theme_bw()

ggsave(file = "outputs/figures_2024_totalBirdDiversity/CCC_Change_lmer.png",
       dpi = 800, width = 12, height = 12, units = "cm")