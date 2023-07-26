#Script to look at single acoustic index correlations with Total Bird Diversity across # of recording days
library(tidyverse)
library(boot)

# Performance metrics ----
MAE = function(pred, obs){
  mean(abs(pred - obs))
}

RMSE = function(pred, obs){
  sqrt(mean((pred - obs)^2))
}

R = function(pred, obs){
  sum((obs - mean(obs))*(pred - mean(pred))) / sqrt(sum((obs - mean(obs))^2)*sum((pred - mean(pred))^2))
}

# Load in indices ----
Indices_Summary <- readRDS("outputs/Indices_Summary_2023-07-18")

Indices_R <- readRDS("outputs/Indices_R_2023-07-26")


#Column plot of number of survey periods with 'n' number of days
Indices_Summary %>% 
  group_by(audioDays, type) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = audioDays, y = n)) + 
  geom_col() + 
  scale_x_continuous(breaks = seq(1, max(10))) +
  facet_wrap(~type) + 
  theme_bw()

# Bootstrap correlation - Summary Indices ----

#Calculate bootstrap correlation values for each acoustic index at each # of audioDays
bootCor_results_summary <- data.frame()
bootCor_raw_summary <- data.frame()

#Progress bar
pb = txtProgressBar(min = 0, max = 4*4*8*2*2*length(colnames(select(Indices_Summary, ends_with(c("median"))))), initial = 0, style = 3); k <- 0

for (measure in c('Mean20m', 'Mean40m', 'Detected20', 'Detected40')) {
  for (timeperiod in c('dawn', 'solarNoon', 'dusk', 'day')) {
    for (numDays in seq(1, 9)) { #9 and 10 have too few data points for comparison I think
      for (summaryMethod in c("mean", "median")) {
        for (correlationMethod in c("spearman", "pearson")) {
          for (index in colnames(select(Indices_Summary, ends_with(c(summaryMethod))))) {
            set.seed(1234)#set seed for reproducibility
            bootResults <- boot(Indices_Summary[Indices_Summary$type == timeperiod & Indices_Summary$audioDays == numDays,], 
                                statistic = function(data, i) {
                                  cor(data[i, measure], data[i, index], method=correlationMethod)
                                },
                                R = 1000)
            
            bootCor_raw_summary <- bind_rows(bootCor_raw_summary,
                                             data.frame(Index = gsub(paste0("_", summaryMethod), "", index),
                                                        SummaryMethod = summaryMethod,
                                                        CorrelationMethod = correlationMethod,
                                                        Time = timeperiod,
                                                        NumDaysAudio = numDays,
                                                        Measure = measure,
                                                        Correlation = bootResults$t[,1]))
            
            bootResultsCI <- boot.ci(bootResults, 
                                     conf = 0.95, type = "bca")
            
            bootCor_results_summary <- bind_rows(bootCor_results_summary,
                                                 data.frame(Index = gsub(paste0("_", summaryMethod), "", index),
                                                            SummaryMethod = summaryMethod,
                                                            CorrelationMethod = correlationMethod,
                                                            Time = timeperiod,
                                                            NumDaysAudio = numDays,
                                                            Measure = measure,
                                                            Mean = mean(bootResults$t),
                                                            Low = bootResultsCI$bca[4],
                                                            High = bootResultsCI$bca[5]))
            
            k <- k+1; setTxtProgressBar(pb, k)
          }
        }
      }
    }
  }
}

# ├ Produce plots ----

#Very little difference in plots between both mean vs median and spearman vs pearson!
#Perhaps just use spearman and median? Median argument is to reduce the effect of outliers, spearman is that we don't 
#necessarily expect the relationship to be linear

#Median & Spearman

# ├├ Mean20m ----
Plot_Indices_Summary_Mean20m <- ggplot(data = bootCor_results_summary %>% 
                                         filter(SummaryMethod == 'median', 
                                                CorrelationMethod == 'spearman',
                                                Measure == "Mean20m"),
                                       aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4)) +
  facet_wrap(~Index) +
  labs(x = "Number of Recording Days", y = "Mean Correlation (+- 95% CI)") +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_y_continuous(limits = c(-0.86, 0.86), breaks = seq(-0.8, 0.8, 0.2)) +
  scale_color_viridis_d(name = "Time of Day") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15),
        panel.grid.minor = element_blank())

ggsave(filename = "outputs/figures_2023/bootstrapcorrelations/Indices_Summary_Mean20m.png",
       plot = Plot_Indices_Summary_Mean20m,
       width = 12, height = 9)

# ├├ Mean40m ----
Plot_Indices_Summary_Mean40m <- ggplot(data = bootCor_results_summary %>% 
                                         filter(SummaryMethod == 'median', 
                                                CorrelationMethod == 'spearman',
                                                Measure == "Mean40m"),
                                       aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4)) +
  facet_wrap(~Index) +
  labs(x = "Number of Recording Days", y = "Mean Correlation (+- 95% CI)") +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_y_continuous(limits = c(-0.86, 0.86), breaks = seq(-0.8, 0.8, 0.2)) +
  scale_color_viridis_d(name = "Time of Day") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15),
        panel.grid.minor = element_blank())

ggsave(filename = "outputs/figures_2023/bootstrapcorrelations/Indices_Summary_Mean40m.png",
       plot = Plot_Indices_Summary_Mean40m,
       width = 12, height = 9)

# ├├ Detected20 ----
Plot_Indices_Summary_Detected20 <- ggplot(data = bootCor_results_summary %>% 
                                            filter(SummaryMethod == 'median', 
                                                   CorrelationMethod == 'spearman',
                                                   Measure == "Detected20"),
                                          aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4)) +
  facet_wrap(~Index) +
  labs(x = "Number of Recording Days", y = "Mean Correlation (+- 95% CI)") +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_y_continuous(limits = c(-0.86, 0.86), breaks = seq(-0.8, 0.8, 0.2)) +
  scale_color_viridis_d(name = "Time of Day") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15),
        panel.grid.minor = element_blank())

ggsave(filename = "outputs/figures_2023/bootstrapcorrelations/Indices_Summary_Detected20.png",
       plot = Plot_Indices_Summary_Detected20,
       width = 12, height = 9)

# ├├ Detected40 ----
Plot_Indices_Summary_Detected40 <- ggplot(data = bootCor_results_summary %>% 
                                            filter(SummaryMethod == 'median', 
                                                   CorrelationMethod == 'spearman',
                                                   Measure == "Detected40"),
                                          aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4)) +
  facet_wrap(~Index) +
  labs(x = "Number of Recording Days", y = "Mean Correlation (+- 95% CI)") +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_y_continuous(limits = c(-0.86, 0.86), breaks = seq(-0.8, 0.8, 0.2)) +
  scale_color_viridis_d(name = "Time of Day") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15),
        panel.grid.minor = element_blank())

ggsave(filename = "outputs/figures_2023/bootstrapcorrelations/Indices_Summary_Detected40.png",
       plot = Plot_Indices_Summary_Detected40,
       width = 12, height = 9)





# Bootstrap correlation - R Indices ----

#Calculate bootstrap correlation values for each acoustic index at each # of audioDays
bootCor_results_R <- data.frame()
bootCor_raw_R <- data.frame()

#Progress bar
pb = txtProgressBar(min = 0, max = 4*4*8*2*2*length(colnames(select(Indices_R, ends_with(c("median"))))[-c(2,3,4,5)]), initial = 0, style = 3); k <- 0

for (measure in c('Mean20m', 'Mean40m', 'Detected20', 'Detected40')) {
  for (timeperiod in c('dawn', 'solarNoon', 'dusk', 'day')) {
    for (numDays in seq(1, 9)) { #9 and 10 have too few data points for comparison I think
      for (summaryMethod in c("mean", "median")) {
        for (correlationMethod in c("spearman", "pearson")) {
          for (index in colnames(select(Indices_R, ends_with(c(summaryMethod))))[-c(2,3,4,5)]) {
            set.seed(1234)#set seed for reproducibility
            bootResults <- boot(Indices_R[Indices_R$type == timeperiod & Indices_R$audioDays == numDays,], 
                                statistic = function(data, i) {
                                  cor(data[i, measure], data[i, index], method=correlationMethod)
                                },
                                R = 1000)
            
            bootCor_raw_R <- bind_rows(bootCor_raw_R,
                                       data.frame(Index = gsub(paste0("_", summaryMethod), "", index),
                                                  SummaryMethod = summaryMethod,
                                                  CorrelationMethod = correlationMethod,
                                                  Time = timeperiod,
                                                  NumDaysAudio = numDays,
                                                  Measure = measure,
                                                  Correlation = bootResults$t[,1]))
            
            bootResultsCI <- boot.ci(bootResults, 
                                     conf = 0.95, type = "bca")
            
            bootCor_results_R <- bind_rows(bootCor_results_R,
                                           data.frame(Index = gsub(paste0("_", summaryMethod), "", index),
                                                      SummaryMethod = summaryMethod,
                                                      CorrelationMethod = correlationMethod,
                                                      Time = timeperiod,
                                                      NumDaysAudio = numDays,
                                                      Measure = measure,
                                                      Mean = mean(bootResults$t),
                                                      Low = bootResultsCI$bca[4],
                                                      High = bootResultsCI$bca[5]))
            
            k <- k+1; setTxtProgressBar(pb, k)
          }
        }
      }
    }
  }
}

# ├ Produce plots ----

# ├├ Mean20m ----
Plot_Indices_R_Mean20m <- ggplot(data = bootCor_results_R %>% 
                                   filter(SummaryMethod == 'median', 
                                          CorrelationMethod == 'spearman',
                                          Measure == "Mean20m"),
                                 aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4)) +
  facet_wrap(~Index) +
  labs(x = "Number of Recording Days", y = "Mean Correlation (+- 95% CI)") +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_y_continuous(limits = c(-0.86, 0.86), breaks = seq(-0.8, 0.8, 0.2)) +
  scale_color_viridis_d(name = "Time of Day") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15),
        panel.grid.minor = element_blank())

ggsave(filename = "outputs/figures_2023/bootstrapcorrelations/Indices_R_Mean20m.png",
       plot = Plot_Indices_R_Mean20m,
       width = 12, height = 9)

# ├├ Mean40m ----
Plot_Indices_R_Mean40m <- ggplot(data = bootCor_results_R %>% 
                                   filter(SummaryMethod == 'median', 
                                          CorrelationMethod == 'spearman',
                                          Measure == "Mean40m"),
                                 aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4)) +
  facet_wrap(~Index) +
  labs(x = "Number of Recording Days", y = "Mean Correlation (+- 95% CI)") +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_y_continuous(limits = c(-0.86, 0.86), breaks = seq(-0.8, 0.8, 0.2)) +
  scale_color_viridis_d(name = "Time of Day") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15),
        panel.grid.minor = element_blank())

ggsave(filename = "outputs/figures_2023/bootstrapcorrelations/Indices_R_Mean40m.png",
       plot = Plot_Indices_R_Mean40m,
       width = 12, height = 9)

# ├├ Detected20 ----
Plot_Indices_R_Detected20 <- ggplot(data = bootCor_results_R %>% 
                                      filter(SummaryMethod == 'median', 
                                             CorrelationMethod == 'spearman',
                                             Measure == "Detected20"),
                                    aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4)) +
  facet_wrap(~Index) +
  labs(x = "Number of Recording Days", y = "Mean Correlation (+- 95% CI)") +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_y_continuous(limits = c(-0.86, 0.86), breaks = seq(-0.8, 0.8, 0.2)) +
  scale_color_viridis_d(name = "Time of Day") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15),
        panel.grid.minor = element_blank())

ggsave(filename = "outputs/figures_2023/bootstrapcorrelations/Indices_R_Detected20.png",
       plot = Plot_Indices_R_Detected20,
       width = 12, height = 9)

# ├├ Detected40 ----
Plot_Indices_R_Detected40 <- ggplot(data = bootCor_results_R %>% 
                                      filter(SummaryMethod == 'median', 
                                             CorrelationMethod == 'spearman',
                                             Measure == "Detected40"),
                                    aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4)) +
  facet_wrap(~Index) +
  labs(x = "Number of Recording Days", y = "Mean Correlation (+- 95% CI)") +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_y_continuous(limits = c(-0.86, 0.86), breaks = seq(-0.8, 0.8, 0.2)) +
  scale_color_viridis_d(name = "Time of Day") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.15),
        panel.grid.minor = element_blank())

ggsave(filename = "outputs/figures_2023/bootstrapcorrelations/Indices_R_Detected40.png",
       plot = Plot_Indices_R_Detected40,
       width = 12, height = 9)


# Save Workspace ----
save.image(file = "workspaces/SingleIndexCorrelations_TotalBirdDiversity.RData")