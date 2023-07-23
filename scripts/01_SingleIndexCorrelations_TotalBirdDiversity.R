#Script to look at single acoustic index correlations with Total Bird Diversity across # of recording days
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


save.image(file = "workspaces/SingleIndexCorrelations_TotalBirdDiversity.RData")