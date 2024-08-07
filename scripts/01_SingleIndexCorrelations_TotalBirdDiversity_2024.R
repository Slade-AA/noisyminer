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

#Colour palettes
#     vibrant_orange    vibrant_blue vibrant_magenta    vibrant_cyan 
#>       "#ee7733"       "#0077bb"       "#ee3377"       "#33bbee"

# Load in indices ----
Indices_Summary <- readRDS("outputs/Indices_Summary_noOutlierRemoval_2024-08-02.rds")

Indices_R <- readRDS("outputs/Indices_R_noOutlierRemoval_2024-08-02.rds")


#Column plot of number of survey periods with 'n' number of days
Indices_Summary %>% 
  group_by(audioDays, type) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = audioDays, y = n)) + 
  geom_col() + 
  scale_x_continuous(breaks = seq(1, max(10))) +
  facet_wrap(~type) + 
  theme_bw()

ggsave(filename = "outputs/figures_2024_totalBirdDiversity/AvailableData.png",
       dpi = 800, width = 12, height = 12, units = "cm")

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

ggsave(filename = "outputs/figures_2024_totalBirdDiversity/bootstrapcorrelations/Indices_Summary_Mean20m.png",
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

ggsave(filename = "outputs/figures_2024_totalBirdDiversity/bootstrapcorrelations/Indices_Summary_Mean40m.png",
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

ggsave(filename = "outputs/figures_2024_totalBirdDiversity/bootstrapcorrelations/Indices_Summary_Detected20.png",
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

ggsave(filename = "outputs/figures_2024_totalBirdDiversity/bootstrapcorrelations/Indices_Summary_Detected40.png",
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

ggsave(filename = "outputs/figures_2024_totalBirdDiversity/bootstrapcorrelations/Indices_R_Mean20m.png",
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

ggsave(filename = "outputs/figures_2024_totalBirdDiversity/bootstrapcorrelations/Indices_R_Mean40m.png",
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

ggsave(filename = "outputs/figures_2024_totalBirdDiversity/bootstrapcorrelations/Indices_R_Detected20.png",
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

ggsave(filename = "outputs/figures_2024_totalBirdDiversity/bootstrapcorrelations/Indices_R_Detected40.png",
       plot = Plot_Indices_R_Detected40,
       width = 12, height = 9)

#Figures for publication (Detected40 and Mean40m) ----

#Colour palettes
cols <- c('dawn' = '#e41a1c', 'day' = '#377eb8', 'dusk' = '#984ea3', 'solarNoon' = '#4daf4a')
cols_alternate <- c('dawn' = '#ee7733', 'day' = '#0077bb', 'dusk' = '#ee3377', 'solarNoon' = '#33bbee')


bootCor_results_combined <- bind_rows(bootCor_results_R %>% 
                                        filter(Index %in% c("ADI", "BI", "M", "H"),
                                               SummaryMethod == 'median',
                                               CorrelationMethod == 'spearman'),
                                      bootCor_results_summary %>% 
                                        filter(Index %in% unique(bootCor_results_summary$Index),
                                               SummaryMethod == 'median',
                                               CorrelationMethod == 'spearman')) %>% 
  mutate(Alpha = ifelse(sign(Low) == sign(High), 1, 0.3))


# ├ Detected40 ----
Plot_BootstrapCorrelations_Detected40 <- ggplot(data = bootCor_results_combined %>% 
                                                  filter(Measure == "Detected40"),
                                                aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time, alpha = Alpha)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4), size = 0.25, linewidth = 0.6) +
  facet_wrap(~fct_relevel(Index, 'ADI', 'BI', 'M', 'H')) +
  labs(x = "Number of Recording Days", y = "Mean Correlation (+- 95% CI)") +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_y_continuous(limits = c(-0.86, 0.86), breaks = seq(-0.8, 0.8, 0.2)) +
  #scale_color_viridis_d(name = "Time of Day") +
  scale_color_manual(values = cols, name = "Time of Day") +
  scale_alpha_identity() +
  theme_bw() +
  theme(legend.position = c(0.75, 0.1),
        panel.grid.minor = element_blank())

ggsave(filename = "outputs/figures_2024_totalBirdDiversity/FigureX_BootstrapCorrelations_Detected40.png",
       plot = Plot_BootstrapCorrelations_Detected40,
       width = 190, height = 190, units = "mm", dpi = 800)

# ├ Mean40m ----
Plot_BootstrapCorrelations_Mean40m <- ggplot(data = bootCor_results_combined %>% 
                                               filter(Measure == "Mean40m"),
                                             aes(x = NumDaysAudio, y = Mean, group = Time, colour = Time, alpha = Alpha)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = Low, ymax = High), position = position_dodge(width = 0.4), size = 0.25, linewidth = 0.6) +
  facet_wrap(~fct_relevel(Index, 'ADI', 'BI', 'M', 'H')) +
  labs(x = "Number of Recording Days", y = "Mean Correlation (+- 95% CI)") +
  scale_x_continuous(breaks = seq(1, 10)) +
  scale_y_continuous(limits = c(-0.86, 0.86), breaks = seq(-0.8, 0.8, 0.2)) +
  #scale_color_viridis_d(name = "Time of Day") +
  scale_color_manual(values = cols, name = "Time of Day") +
  scale_alpha_identity() +
  theme_bw() +
  theme(legend.position = c(0.75, 0.1),
        panel.grid.minor = element_blank())

ggsave(filename = "outputs/figures_2024_totalBirdDiversity/FigureX_BootstrapCorrelations_Mean40m.png",
       plot = Plot_BootstrapCorrelations_Mean40m,
       width = 190, height = 190, units = "mm", dpi = 800)

#Best single index correlation values at day 9 ----

bootCor_results_combined %>% 
  filter(NumDaysAudio == 9,
         Measure == "Detected40") %>% 
  group_by(Index) %>% 
  slice_max(order_by = Mean, n = 1) %>% 
  ungroup() %>% 
  select(Index, Time, Mean, Low, High) %>% 
  arrange(desc(Mean)) %>% 
  write.csv(file = "outputs/figures_2024_totalBirdDiversity/CorrelationPerIndex.csv", row.names = FALSE, quote = FALSE)


# Save Workspace ----
save.image(file = "workspaces/SingleIndexCorrelations_2024_TotalBirdDiversity.RData")