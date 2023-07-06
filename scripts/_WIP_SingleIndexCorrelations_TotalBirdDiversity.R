#WIP script to look at single acoustic index correlations across # of recording days
library(tidyverse)
library(boot)

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

allSummary <- readRDS("outputs/allSummary_2023-06-02")

#Add region variable?
unique(gsub("([0-9])", "", unique(allSummary$Site)))

#Column plot of number of survey periods with 'n' number of days
allSummary %>% 
  group_by(audioDays, type) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = audioDays, y = n)) + 
  geom_col() + 
  scale_x_continuous(breaks = seq(1, max(10))) +
  facet_wrap(~type) + 
  theme_bw()

# Bootstrap correlation ----

#Calculate bootstrap correlation values for each acoustic index at each # of audioDays
bootCor_results <- data.frame()
bootCor_raw <- data.frame()

for (measure in c('Detected40')) {
  for (timeperiod in c('dawn', 'solarNoon', 'dusk', 'day')) {
    for (numDays in seq(1, 10)) {
      for (summaryMethod in c("mean", "median")) {
        for (correlationMethod in c("spearman", "pearson")) {
          for (index in colnames(select(allSummary, ends_with(c(summaryMethod))))) {
            set.seed(1234)#set seed for reproducibility
            bootResults <- boot(allSummary[allSummary$type == timeperiod & allSummary$audioDays == numDays,], 
                                statistic = function(data, i) {
                                  cor(data[i, measure], data[i, index], method=correlationMethod) #spearman or pearson???
                                },
                                R = 1000)
            
            bootCor_raw <- bind_rows(bootCor_raw,
                                     data.frame(Index = gsub(paste0("_", summaryMethod), "", index),
                                                SummaryMethod = summaryMethod,
                                                CorrelationMethod = correlationMethod,
                                                Time = timeperiod,
                                                NumDaysAudio = numDays,
                                                Measure = measure,
                                                Correlation = bootResults$t[,1]))
            
            bootResultsCI <- boot.ci(bootResults, 
                                     conf = 0.95, type = "bca")
            
            bootCor_results <- bind_rows(bootCor_results,
                                         data.frame(Index = gsub(paste0("_", summaryMethod), "", index),
                                                    SummaryMethod = summaryMethod,
                                                    CorrelationMethod = correlationMethod,
                                                    Time = timeperiod,
                                                    NumDaysAudio = numDays,
                                                    Measure = measure,
                                                    Mean = mean(bootResults$t),
                                                    Low = bootResultsCI$bca[4],
                                                    High = bootResultsCI$bca[5]))
          }
        }
      }
    }
  }
}

#Mean & Spearman
ggplot(data = bootCor_results %>% filter(SummaryMethod == 'mean', CorrelationMethod == 'spearman'),
       aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4)) +
  facet_wrap(~Index) +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15))

#Mean & Pearson
ggplot(data = bootCor_results %>% filter(SummaryMethod == 'mean', CorrelationMethod == 'pearson'),
       aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4)) +
  facet_wrap(~Index) +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15))

#Very little difference in plots between both mean vs median and spearman vs pearson!

#Perhaps just use pearson and median? Median argument is to reduce the effect of outliers
#Median & Pearson
ggplot(data = bootCor_results %>% filter(SummaryMethod == 'median', CorrelationMethod == 'pearson'),
       aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4)) +
  facet_wrap(~Index) +
  labs(x = "Number of Recording Days", y = "Mean Correlation (+- 95% CI)") +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_y_continuous(limits = c(-0.8, 0.8), breaks = seq(-0.8, 0.8, 0.2)) +
  scale_color_viridis_d(name = "Time of Day") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15),
        panel.grid.minor = element_blank())


# Multi-index models ----

#Model Mean correlation for each index by number of audio days?
model_ACI <- lm(Mean ~ NumDaysAudio, 
                data = bootCor_results %>% 
                  filter(Index == 'AcousticComplexity', SummaryMethod == 'median', CorrelationMethod == 'pearson', Time == "dawn"))
model_ACI <- gam(Mean ~ s(NumDaysAudio), 
                 data = bootCor_results %>% 
                   filter(Index == 'AcousticComplexity', SummaryMethod == 'median', CorrelationMethod == 'pearson', Time == "dawn"))
#GAM just results in perfect fit - what exactly am I trying to say with these models?


#Generalized linear mixed-effects models - Single acoustic index, effect of number of days

testlme <- glmer(Detected40 ~ SptDensity_median + audioDays + (1|SurveyIDR12),
                 data = allSummary[allSummary$type == 'dawn',], 
                 family = poisson, nAGQ = 0,
                 glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

summary(testlme)
performance::check_model(testlme)



#Multi-index GAM?

#dawn, median, all indices

allSummary_filtered <- allSummary %>% filter(type == 'dawn')

library(mgcv)
mod_lm = gam(Detected40 ~ AcousticComplexity_median, data = allSummary %>% filter(type == 'dawn', audioDays == 8))
summary(mod_lm)


mod_gam = gam(Detected40 ~ s(AcousticComplexity_median, bs = "cr"), data = allSummary %>% filter(type == 'dawn', audioDays == 8))
summary(mod_gam)

anova(mod_lm, mod_gam, test = "Chisq") #gam significatnly better than linear


#Multiple acoustic indices
mod_lm2 = gam(Detected40 ~ AcousticComplexity_median + EventsPerSecond_median + MidFreqCover_median + Ndsi_median, 
              data = allSummary %>% filter(type == 'dawn', audioDays == 8))
summary(mod_lm2)

mod_gam2 = gam(Detected40 ~ s(AcousticComplexity_median) + s(EventsPerSecond_median) + s(MidFreqCover_median) + s(Ndsi_median) + s(Site, bs = "re"), 
               data = allSummary %>% filter(type == 'dawn', audioDays == 8))
summary(mod_gam2) #big difference in GCV and Deviance explained when including or excluding 'Site' as a random effect

MAE(mod_gam2$fitted.values, mod_gam2$y)

plot(ggeffects::ggpredict(mod_gam2), facets = TRUE)

anova(mod_lm2, mod_gam2, test = "Chisq")

anova(mod_gam, mod_gam2, test = "Chisq") #multiple indicators significantly better than single

#gamm
mod_gamm2 = gamm(Detected40 ~ s(AcousticComplexity_median) + s(EventsPerSecond_median) + s(MidFreqCover_median) + s(Ndsi_median), random=list(Site=~1), 
                 data = allSummary %>% filter(type == 'dawn', audioDays == 8))


#Same as extracting 'fitted.values' from the gam object
preds_modgam2 <- predict(mod_gam2, newdata = allSummary %>% filter(type == 'dawn', audioDays == 8), type = "response")

#Observed vs predicted
Obs_pred <- data.frame(Obs = mod_gam2$y,
                       Pred = mod_gam2$fitted.values)

ggplot(Obs_pred, aes(x = Obs, y = Pred)) +
  geom_abline(slope = 1, linetype = "dotted") +
  geom_point() +
  scale_x_continuous(limits = c(0 , max(c(Obs_pred$Obs, Obs_pred$Pred)))) +
  scale_y_continuous(limits = c(0 , max(c(Obs_pred$Obs, Obs_pred$Pred)))) +
  theme_bw()

DescTools::CCC(Obs_pred$Obs, Obs_pred$Pred)$rho.c

#Indices with good correlations (ENV, HFC, MFC, ACI, CLS, NDSI, SPD - i.e., remove ACT, LFC and ENT)
mod_gam4 = gam(Detected40 ~ 
                 s(EventsPerSecond_median) + 
                 s(HighFreqCover_median) + 
                 s(MidFreqCover_median) + 
                 s(AcousticComplexity_median) + 
                 s(ClusterCount_median) + 
                 s(Ndsi_median) + 
                 s(SptDensity_median) +
                 s(Site, bs = "re"), 
               data = allSummary %>% filter(type == 'dawn', audioDays == 8))
summary(mod_gam4)

#All indices
paste(colnames(select(allSummary, ends_with(c("median")))), collapse = " + ")

mod_gam3 = gam(Detected40 ~ 
                 s(Activity_median) + 
                 s(EventsPerSecond_median) + 
                 s(HighFreqCover_median) + 
                 s(MidFreqCover_median) + 
                 s(LowFreqCover_median) + 
                 s(AcousticComplexity_median) + 
                 s(TemporalEntropy_median) + 
                 s(ClusterCount_median) + 
                 s(Ndsi_median) + 
                 s(SptDensity_median) +
                 s(Site, bs = "re"), 
               data = allSummary %>% filter(type == 'dawn', audioDays == 8))
summary(mod_gam3)

mod_gam3_noSite = gam(Detected40 ~ 
                        s(Activity_median) + 
                        s(EventsPerSecond_median) + 
                        s(HighFreqCover_median) + 
                        s(MidFreqCover_median) + 
                        s(LowFreqCover_median) + 
                        s(AcousticComplexity_median) + 
                        s(TemporalEntropy_median) + 
                        s(ClusterCount_median) + 
                        s(Ndsi_median) + 
                        s(SptDensity_median), 
                      data = allSummary %>% filter(type == 'dawn', audioDays == 8))
summary(mod_gam3_noSite)


RMSE(mod_gam3$fitted.values, mod_gam3$y)

MAE(mod_gam3$fitted.values, mod_gam3$y)

R(mod_gam3$fitted.values, mod_gam3$y)




#dawn and solarNoon
allSummary_dawn_solarNoon <- allSummary %>% 
  filter(type == "dawn" | type == "solarNoon") %>% 
  pivot_wider(id_cols = c(SurveyIDR12, audioDays, Detected40), 
              names_from = type, 
              values_from = c(AcousticComplexity_median, SptDensity_median, MidFreqCover_median, Ndsi_median, EventsPerSecond_median), 
              names_glue = "{type}_{.value}")

#dawn only
mod_gam4 = gam(Detected40 ~ s(dawn_AcousticComplexity_median) + 
                 s(dawn_EventsPerSecond_median) + 
                 s(dawn_MidFreqCover_median) + 
                 s(dawn_Ndsi_median), 
               data = allSummary_dawn_solarNoon %>% filter(audioDays == 8))
summary(mod_gam4)

#dawn and solarNoon
mod_gam5 = gam(Detected40 ~ s(dawn_AcousticComplexity_median) + s(solarNoon_AcousticComplexity_median) + 
                 s(dawn_EventsPerSecond_median) + s(solarNoon_EventsPerSecond_median) + 
                 s(dawn_MidFreqCover_median) + s(solarNoon_MidFreqCover_median) + 
                 s(dawn_Ndsi_median) + s(solarNoon_Ndsi_median), 
               data = allSummary_dawn_solarNoon %>% filter(audioDays == 8))
summary(mod_gam5)



mod_lm_dawn = gam(Detected40 ~ AcousticComplexity_median, data = allSummary %>% filter(type == 'dawn', audioDays == 8))

mod_lm_solarNoon = gam(Detected40 ~ AcousticComplexity_median, data = allSummary %>% filter(type == 'solarNoon', audioDays == 8))

mod_lm_dawnSolarNoon = gam(Detected40 ~ dawn_AcousticComplexity_median + solarNoon_AcousticComplexity_median, 
                           data = allSummary_dawn_solarNoon %>% filter(audioDays == 8))

summary(mod_lm_dawn)
summary(mod_lm_solarNoon)
summary(mod_lm_dawnSolarNoon)




#Linear-mixed effects models
library(lme4)
library(lmerTest)

model_lme <- glmer(Detected40 ~ 
                     Activity_median + 
                     EventsPerSecond_median + 
                     HighFreqCover_median + 
                     MidFreqCover_median + 
                     LowFreqCover_median + 
                     AcousticComplexity_median + 
                     TemporalEntropy_median + 
                     ClusterCount_median + 
                     Ndsi_median + 
                     SptDensity_median +
                     (1|Site),
                   data = allSummary %>% filter(type == 'dawn', audioDays == 8),
                   family = poisson())

summary(model_lme)
performance::check_model(model_lme)


model_lme2 <- glmer(Detected40 ~ 
                      AcousticComplexity_median + 
                      MidFreqCover_median + 
                      Ndsi_median + 
                      EventsPerSecond_median +
                      (1|Site),
                    data = allSummary %>% filter(type == 'dawn', audioDays == 8),
                    family = poisson())
summary(model_lme2)
performance::check_model(model_lme2)


#Random Forest ----
library(caret)
library(randomForest)
library(mgcv)
library(yardstick)

prediction_results <- data.frame()

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
                      ntree = 1000,
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
    
  }
}


ggplot(prediction_results, aes(x = factor(audioDays, levels = c(1,2,3,4,5,6,7,8)), y = MAE, fill = method)) + 
  geom_boxplot() +
  facet_wrap(~performance) +
  theme_bw()

ggplot(prediction_results %>% filter(performance == 'train'), aes(x = factor(audioDays, levels = c(1,2,3,4,5,6,7,8)), y = MAE, fill = method)) + 
  geom_boxplot() +
  theme_bw()

ggplot(prediction_results, aes(x = factor(audioDays, levels = c(1,2,3,4,5,6,7,8)), y = MAE, fill = performance)) + 
  geom_boxplot() +
  facet_wrap(~method) +
  theme_bw()



ggplot(data = prediction_results, aes(x = method, y = est)) + 
  geom_boxplot() +
  theme_bw()


model_gamm <- gamm(Detected40 ~ s(AcousticComplexity_median) + s(EventsPerSecond_median) + s(MidFreqCover_median) + s(Ndsi_median), random = list(Site=~1), 
                   data = allSummary %>% filter(type == 'dawn', audioDays == 8))


library(party)
library(permimp)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verbose = FALSE, savePredictions = TRUE)
tunegrid <- expand.grid(.mtry=c(2:10))

rf_data <- allSummary %>% filter(type == 'dawn' & audioDays == 8)

#split into train and test & filter to acoustic index columns
train <- createDataPartition(rf_data$Detected40, p = 0.8, list = F)

trainData <- rf_data[train,] %>% dplyr::select(c('Detected40', ends_with(c("median")))) # 
testData <- rf_data[-train,] %>% dplyr::select(c('Detected40', ends_with(c("median")))) #'Site', 

rf_model_unbiased <- train(Detected40~.,
                           data = trainData,
                           method = "cforest",
                           tuneGrid = tunegrid,
                           trControl = control,
                           controls = cforest_unbiased(ntree = 2000))
print(rf_model_unbiased)

#Make predictions on test set
predictions_rf_unbiased <- rf_model_unbiased %>% predict(testData)

#observed and predicted
obs_pred_rf_unbiased <- data.frame(observed = testData$Detected40, predicted = predictions_rf_unbiased)

prediction_results_unbiased <- data.frame()
prediction_results_unbiased <- bind_rows(prediction_results_unbiased,
                                         cbind(method = "rf_party", DescTools::CCC(obs_pred_rf_unbiased$observed, obs_pred_rf_unbiased$predicted)$rho.c,
                                               MAE = MAE(obs_pred_rf_unbiased$predicted, obs_pred_rf_unbiased$observed)))

ggplot(obs_pred_rf_unbiased, aes(x = predicted, y = observed)) + geom_point() + geom_abline(slope = 1) +
  scale_x_continuous(limits = c(0 , max(c(obs_pred_rf_unbiased$observed, obs_pred_rf_unbiased$predicted)))) +
  scale_y_continuous(limits = c(0 , max(c(obs_pred_rf_unbiased$observed, obs_pred_rf_unbiased$predicted)))) +
  theme_bw()

varImp(rf_model_unbiased)
rf_importance  <- permimp(rf_model_unbiased$finalModel, conditional = TRUE, progressBar = FALSE, scaled = TRUE)