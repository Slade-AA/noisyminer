#Calculate spearman correlation values for each acoustic index

# Load packages ----

library(tidyverse)
library(cowplot)
library(ggpubr)
library(boot)

# Load indices and biodiversity data ----

acousticIndices_biodiversity <- readRDS("outputs/data/acousticIndices_biodiversity.RDS")

# Test - remove CVR values >= 0.15 - possible rain/wind ----

#acousticIndices_biodiversity$R1Only <- acousticIndices_biodiversity$R1Only[acousticIndices_biodiversity$R1Only$CVR_0_1166_3646_11025_mean < 0.15,]
#acousticIndices_biodiversity$R1R2Combined <- acousticIndices_biodiversity$R1R2Combined[acousticIndices_biodiversity$R1R2Combined$CVR_0_1166_3646_11025_mean < 0.15,]

# Axis labels for plotting ----

#create acronymns for AP indices
axisLabels_AP <- c("Activity" = "ACT", 
                   "EventsPerSecond" = "ENV", 
                   "SpectralCentroid" = "CENT", 
                   "HighFreqCover" = "HFC", 
                   "MidFreqCover" = "MFC", 
                   "LowFreqCover" = "LFC", 
                   "AcousticComplexity" = "ACI", 
                   "TemporalEntropy" = "ENT",
                   "EntropyOfAverageSpectrum" = "EAS",  
                   "EntropyOfVarianceSpectrum" = "EVS",
                   "EntropyOfPeaksSpectrum" = "EPS",
                   "EntropyOfCoVSpectrum" = "ECS",
                   "ClusterCount" = "CLS", 
                   "ThreeGramCount" = "TGC", 
                   "Ndsi" = "NDSI", 
                   "SptDensity" = "SPD")

axisLabels_R <- c('ACI_soundecology' = "ACI",
                  'ADI' = "ADI",
                  'AE' = "AE",
                  'NDSI_soundecology' = "NDSI",
                  'NDSI_bio' = bquote("NDSI"[Bio]),
                  'NDSI_anthro' = bquote("NDSI"[Anthro]),
                  'M' = "M",
                  'H' = "H",
                  'Ht' = "Ht",
                  'Hf' = "Hf",
                  'BI' = "BI")

axisLabels_AP_NM <- c("Activity" = "ACT", 
                      "EventsPerSecond" = "ENV", 
                      "SpectralCentroid" = "CENT", 
                      "HighFreqCover" = "HFC", 
                      "MidFreqCover" = "MFC", 
                      "LowFreqCover" = "LFC", 
                      "AcousticComplexity" = "ACI", 
                      "TemporalEntropy" = "ENT",
                      "EntropyOfAverageSpectrum" = "EAS",  
                      "EntropyOfVarianceSpectrum" = "EVS",
                      "EntropyOfPeaksSpectrum" = "EPS",
                      "EntropyOfCoVSpectrum" = "ECS",
                      "ClusterCount" = "CLS", 
                      "ThreeGramCount" = "TGC", 
                      "Ndsi" = "NDSI", 
                      "SptDensity" = "SPD",
                      "CVR_1166_3646" = bquote("CVR"[chur]),
                      "CVR_0_1166_3646_11025" = bquote("CVR"[!chur]),
                      "CVR_ND" = bquote("CVR"[diff]),
                      "ENT_1166_3646" = bquote("ENT"[chur]),
                      "ENT_0_1166_3646_11025" = bquote("ENT"[!chur]),
                      "ENT_ND" = bquote("ENT"[diff]))

axisLabels_R_NM <- c('ACI_soundecology' = "ACI",
                     'ACI_chur' = bquote("ACI"[chur]),
                     'ACI_notchur' = bquote("ACI"[!chur]),
                     'ADI' = "ADI",
                     'AE' = "AE",
                     'NDSI_soundecology' = "NDSI",
                     'NDSI_bio' = bquote("NDSI"[bio]),
                     'NDSI_anthro' = bquote("NDSI"[anthro]),
                     'M' = "M",
                     'H' = "H",
                     'Ht' = "Ht",
                     'Hf' = "Hf",
                     'BI' = "BI",
                     'BI_chur' = bquote("BI"[chur]))

acousticIndices_set <- list(AP = axisLabels_AP_NM, R = axisLabels_R_NM)

# R1Only - Calculate bootstrap correlation between indices and biodiversity ----

bootCor_results_R1Only <- list()

for (measure in c('Total20', 'Total40', 'Detected20', 'Detected40', 'TotalMiner20', 'TotalMiner40')) {
  for (timeperiod in c('dawn', 'solarNoon', 'dusk', 'day')) {
    
    for (index in colnames(select(acousticIndices_biodiversity$R1Only, ends_with(c("mean"))))) {
      set.seed(1234)#set seed for reproducibility
      bootResults <- boot(acousticIndices_biodiversity$R1Only[acousticIndices_biodiversity$R1Only$type == timeperiod,], 
                          statistic = function(data, i) {
                            cor(data[i, measure], data[i, index], method='spearman')
                          },
                          R = 1000)
      bootResultsCI <- boot.ci(bootResults, 
                               conf = 0.95, type = "bca")
      
      bootCor_results_R1Only[[paste(measure, timeperiod, index, sep = "_")]] <- data.frame(Index = gsub("_mean", "", index),
                                                                                           Time = timeperiod,
                                                                                           Measure = measure,
                                                                                           Mean = mean(bootResults$t),
                                                                                           Low = bootResultsCI$bca[4],
                                                                                           High = bootResultsCI$bca[5])
    }
  }
}

bootCor_results_R1Only <- do.call(rbind, bootCor_results_R1Only)

# ├ Plot correlation bootstrap results ----

# ├ Total count and Diversity ----

# ├├ AP Acoustic Indices ----
correlationPlots_total_diversity_AP <- list()
for (measure in c('Total20', 'Total40', 'Detected20', 'Detected40')) {
  tmp_data <- bootCor_results_R1Only[bootCor_results_R1Only$Measure == measure & 
                                       bootCor_results_R1Only$Index %in% names(axisLabels_AP),]
  tmp_data$Index <- fct_relevel(tmp_data$Index, names(axisLabels_AP))
  
  correlationPlots_total_diversity_AP[[measure]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
    geom_vline(xintercept = seq(-0.8, 0.8, 0.2), linetype = 'dotted') +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_pointrange(aes(xmin = Low, xmax = High), position = position_dodge(width = 0.4)) +
    scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.8, 0.8, 0.4)) +
    scale_y_discrete(labels = axisLabels_AP) +
    scale_color_viridis_d() +
    labs(x = "Mean correlation") +
    theme_classic() +
    theme(axis.title = element_blank(),
          legend.position = "none")
}

legend_bottom <- get_legend(
  correlationPlots_total_diversity_AP[['Total20']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_total_diversity_AP <- plot_grid(plotlist = correlationPlots_total_diversity_AP,
                                                ncol = 2, nrow = 2,
                                                labels = c("A - Total 20m", "B - Total 40m",
                                                           "C - Diversity 20m", "D - Diversity 40m")) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/R1Only_correlationPlot_total_diversity_AP_spearman.png",
       correlationPlot_total_diversity_AP,
       width = 24, height = 24, units = "cm", dpi = 800)

# ├├ R Acoustic Indices ----
correlationPlots_total_diversity_R <- list()
for (measure in c('Total20', 'Total40', 'Detected20', 'Detected40')) {
  tmp_data <- bootCor_results_R1Only[bootCor_results_R1Only$Measure == measure & 
                                       bootCor_results_R1Only$Index %in% names(axisLabels_R),]
  tmp_data$Index <- fct_relevel(tmp_data$Index, names(axisLabels_R))
  
  correlationPlots_total_diversity_R[[measure]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
    geom_vline(xintercept = seq(-0.8, 0.8, 0.2), linetype = 'dotted') +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_pointrange(aes(xmin = Low, xmax = High), position = position_dodge(width = 0.4)) +
    scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.8, 0.8, 0.4)) +
    scale_y_discrete(labels = axisLabels_R) +
    scale_color_viridis_d() +
    labs(x = "Mean correlation") +
    theme_classic() +
    theme(axis.title = element_blank(),
          legend.position = "none")
}

legend_bottom <- get_legend(
  correlationPlots_total_diversity_R[['Total20']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)

correlationPlot_total_diversity_R <- plot_grid(plotlist = correlationPlots_total_diversity_R,
                                                ncol = 2, nrow = 2,
                                                labels = c("A - Total 20m", "B - Total 40m",
                                                           "C - Diversity 20m", "D - Diversity 40m")) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/R1Only_correlationPlot_total_diversity_R_spearman.png",
       correlationPlot_total_diversity_R,
       width = 24, height = 24, units = "cm", dpi = 800)


# ├ Number of Noisy miner ----
#'TotalMiner20', 'TotalMiner40'

correlationPlots_NumberNoisyMiner <- list()
for (measure in c('TotalMiner20', 'TotalMiner40')) {
  for (indexSet in 1:length(acousticIndices_set)) {
    tmp_data <- bootCor_results_R1Only[bootCor_results_R1Only$Measure == measure & 
                                         bootCor_results_R1Only$Index %in% names(acousticIndices_set[[indexSet]]),]
    tmp_data$Index <- fct_relevel(tmp_data$Index, names(acousticIndices_set[[indexSet]]))
    
    correlationPlots_NumberNoisyMiner[[paste0(measure, "_", names(acousticIndices_set)[indexSet])]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
      geom_vline(xintercept = seq(-0.8, 0.8, 0.2), linetype = 'dotted') +
      geom_vline(xintercept = 0, linetype = 'dashed') +
      geom_pointrange(aes(xmin = Low, xmax = High), position = position_dodge(width = 0.4)) +
      scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.8, 0.8, 0.4)) +
      scale_y_discrete(labels = acousticIndices_set[[indexSet]]) +
      scale_color_viridis_d() +
      labs(x = "Mean correlation") +
      theme_classic() +
      theme(axis.title = element_blank(),
            legend.position = "none")
  }
}


legend_bottom <- get_legend(
  correlationPlots_NumberNoisyMiner[['TotalMiner20_AP']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_NumberNoisyMiner <- plot_grid(plotlist = correlationPlots_NumberNoisyMiner,
                                              ncol = 2, nrow = 2, byrow = FALSE,
                                              labels = c("TotalMiner20", "TotalMiner40"), hjust = 0, vjust = 0, label_x = 0.1, label_y = 0.95,
                                              align = "hv",
                                              rel_heights = c(1.571429, 1)) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/R1Only_correlationPlot_NumberNoisyMiner_spearman.png",
       correlationPlot_NumberNoisyMiner,
       width = 48, height = 24, units = "cm", dpi = 800)


# R1R2Combined - Calculate bootstrap correlation between indices and biodiversity ----

bootCor_results_R1R2Combined <- list()

for (measure in c('Mean20m', 'Mean40m', 'Detected20', 'Detected40', 'MeanMiner20m', 'MeanMiner40m')) {
  for (timeperiod in c('dawn', 'solarNoon', 'dusk', 'day')) {
    
    for (index in colnames(select(acousticIndices_biodiversity$R1R2Combined, ends_with(c("mean"))))) {
      set.seed(1234)#set seed for reproducibility
      bootResults <- boot(acousticIndices_biodiversity$R1R2Combined[acousticIndices_biodiversity$R1R2Combined$type == timeperiod,], 
                          statistic = function(data, i) {
                            cor(data[i, measure], data[i, index], method='spearman')
                          },
                          R = 1000)
      bootResultsCI <- boot.ci(bootResults, 
                               conf = 0.95, type = "bca")
      
      bootCor_results_R1R2Combined[[paste(measure, timeperiod, index, sep = "_")]] <- data.frame(Index = gsub("_mean", "", index),
                                                                                           Time = timeperiod,
                                                                                           Measure = measure,
                                                                                           Mean = mean(bootResults$t),
                                                                                           Low = bootResultsCI$bca[4],
                                                                                           High = bootResultsCI$bca[5])
    }
  }
}

bootCor_results_R1R2Combined <- do.call(rbind, bootCor_results_R1R2Combined)

# ├ Plot correlation bootstrap results ----

# ├ Total count and Diversity ----

# ├├ AP Acoustic Indices ----
correlationPlots_total_diversity_AP <- list()
for (measure in c('Mean20m', 'Mean40m', 'Detected20', 'Detected40')) {
  tmp_data <- bootCor_results_R1R2Combined[bootCor_results_R1R2Combined$Measure == measure & 
                                       bootCor_results_R1R2Combined$Index %in% names(axisLabels_AP),]
  tmp_data$Index <- fct_relevel(tmp_data$Index, names(axisLabels_AP))
  
  correlationPlots_total_diversity_AP[[measure]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
    geom_vline(xintercept = seq(-0.8, 0.8, 0.2), linetype = 'dotted') +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_pointrange(aes(xmin = Low, xmax = High), position = position_dodge(width = 0.4)) +
    scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.8, 0.8, 0.4)) +
    scale_y_discrete(labels = axisLabels_AP) +
    scale_color_viridis_d() +
    labs(x = "Mean correlation") +
    theme_classic() +
    theme(axis.title = element_blank(),
          legend.position = "none")
}

legend_bottom <- get_legend(
  correlationPlots_total_diversity_AP[['Mean20m']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_total_diversity_AP <- plot_grid(plotlist = correlationPlots_total_diversity_AP,
                                                ncol = 2, nrow = 2,
                                                labels = c("A - Total 20m", "B - Total 40m",
                                                           "C - Diversity 20m", "D - Diversity 40m")) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/R1R2Combined_correlationPlot_total_diversity_AP_spearman.png",
       correlationPlot_total_diversity_AP,
       width = 24, height = 24, units = "cm", dpi = 800)

# ├├ R Acoustic Indices ----
correlationPlots_total_diversity_R <- list()
for (measure in c('Mean20m', 'Mean40m', 'Detected20', 'Detected40')) {
  tmp_data <- bootCor_results_R1R2Combined[bootCor_results_R1R2Combined$Measure == measure & 
                                       bootCor_results_R1R2Combined$Index %in% names(axisLabels_R),]
  tmp_data$Index <- fct_relevel(tmp_data$Index, names(axisLabels_R))
  
  correlationPlots_total_diversity_R[[measure]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
    geom_vline(xintercept = seq(-0.8, 0.8, 0.2), linetype = 'dotted') +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_pointrange(aes(xmin = Low, xmax = High), position = position_dodge(width = 0.4)) +
    scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.8, 0.8, 0.4)) +
    scale_y_discrete(labels = axisLabels_R) +
    scale_color_viridis_d() +
    labs(x = "Mean correlation") +
    theme_classic() +
    theme(axis.title = element_blank(),
          legend.position = "none")
}

legend_bottom <- get_legend(
  correlationPlots_total_diversity_R[['Mean20m']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)

correlationPlot_total_diversity_R <- plot_grid(plotlist = correlationPlots_total_diversity_R,
                                               ncol = 2, nrow = 2,
                                               labels = c("A - Total 20m", "B - Total 40m",
                                                          "C - Diversity 20m", "D - Diversity 40m")) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/R1R2Combined_correlationPlot_total_diversity_R_spearman.png",
       correlationPlot_total_diversity_R,
       width = 24, height = 24, units = "cm", dpi = 800)


# ├ Number of Noisy miner ----
#'TotalMiner20', 'TotalMiner40'

correlationPlots_NumberNoisyMiner <- list()
for (measure in c('MeanMiner20m', 'MeanMiner40m')) {
  for (indexSet in 1:length(acousticIndices_set)) {
    tmp_data <- bootCor_results_R1R2Combined[bootCor_results_R1R2Combined$Measure == measure & 
                                         bootCor_results_R1R2Combined$Index %in% names(acousticIndices_set[[indexSet]]),]
    tmp_data$Index <- fct_relevel(tmp_data$Index, names(acousticIndices_set[[indexSet]]))
    
    correlationPlots_NumberNoisyMiner[[paste0(measure, "_", names(acousticIndices_set)[indexSet])]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
      geom_vline(xintercept = seq(-0.8, 0.8, 0.2), linetype = 'dotted') +
      geom_vline(xintercept = 0, linetype = 'dashed') +
      geom_pointrange(aes(xmin = Low, xmax = High), position = position_dodge(width = 0.4)) +
      scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.8, 0.8, 0.4)) +
      scale_y_discrete(labels = acousticIndices_set[[indexSet]]) +
      scale_color_viridis_d() +
      labs(x = "Mean correlation") +
      theme_classic() +
      theme(axis.title = element_blank(),
            legend.position = "none")
  }
}


legend_bottom <- get_legend(
  correlationPlots_NumberNoisyMiner[['MeanMiner20m_AP']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_NumberNoisyMiner <- plot_grid(plotlist = correlationPlots_NumberNoisyMiner,
                                              ncol = 2, nrow = 2, byrow = FALSE,
                                              labels = c("TotalMiner20", "TotalMiner40"), hjust = 0, vjust = 0, label_x = 0.1, label_y = 0.95,
                                              align = "hv",
                                              rel_heights = c(1.571429, 1)) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/R1R2Combined_correlationPlot_NumberNoisyMiner_spearman.png",
       correlationPlot_NumberNoisyMiner,
       width = 48, height = 24, units = "cm", dpi = 800)

# Difference between R1Only and R1R2Combined ----
#Take the absolute difference between correlation values for the R1R2Combined data and the R1Only data
#Positive values mean that the correlation was higher for R1R2Combined, negative values mean that correlation was higher for R1Only

bootCor_results_Difference <- cbind(bootCor_results_R1Only[,1:3], 
                                    abs(bootCor_results_R1R2Combined[,4:6]) - abs(bootCor_results_R1Only[,4:6]))

# ├├ AP Acoustic Indices ----
correlationPlots_total_diversity_AP <- list()
for (measure in c('Total20', 'Total40', 'Detected20', 'Detected40')) {
  tmp_data <- bootCor_results_Difference[bootCor_results_Difference$Measure == measure & 
                                       bootCor_results_Difference$Index %in% names(axisLabels_AP),]
  tmp_data$Index <- fct_relevel(tmp_data$Index, names(axisLabels_AP))
  
  correlationPlots_total_diversity_AP[[measure]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
    geom_vline(xintercept = seq(-0.4, 0.4, 0.2), linetype = 'dotted') +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_linerange(aes(xmin = 0, xmax = Mean), position = position_dodge(width = 0.4)) +
    geom_point(position = position_dodge(width = 0.4), size = 2) +
    scale_x_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, 0.1)) +
    scale_y_discrete(labels = axisLabels_AP) +
    scale_color_viridis_d() +
    labs(x = "Mean correlation") +
    theme_classic() +
    theme(axis.title = element_blank(),
          legend.position = "none")
}

legend_bottom <- get_legend(
  correlationPlots_total_diversity_AP[['Total20']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_total_diversity_AP <- plot_grid(plotlist = correlationPlots_total_diversity_AP,
                                                ncol = 2, nrow = 2,
                                                labels = c("A - Total 20m", "B - Total 40m",
                                                           "C - Diversity 20m", "D - Diversity 40m")) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Change in Correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/Difference_correlationPlot_total_diversity_AP_spearman.png",
       correlationPlot_total_diversity_AP,
       width = 24, height = 24, units = "cm", dpi = 800)

# ├├ R Acoustic Indices ----
correlationPlots_total_diversity_R <- list()
for (measure in c('Total20', 'Total40', 'Detected20', 'Detected40')) {
  tmp_data <- bootCor_results_Difference[bootCor_results_Difference$Measure == measure & 
                                       bootCor_results_Difference$Index %in% names(axisLabels_R),]
  tmp_data$Index <- fct_relevel(tmp_data$Index, names(axisLabels_R))
  
  correlationPlots_total_diversity_R[[measure]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
    geom_vline(xintercept = seq(-0.4, 0.4, 0.2), linetype = 'dotted') +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_linerange(aes(xmin = 0, xmax = Mean), position = position_dodge(width = 0.4)) +
    geom_point(position = position_dodge(width = 0.4), size = 2) +
    scale_x_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, 0.1)) +
    scale_y_discrete(labels = axisLabels_R) +
    scale_color_viridis_d() +
    labs(x = "Mean correlation") +
    theme_classic() +
    theme(axis.title = element_blank(),
          legend.position = "none")
}

legend_bottom <- get_legend(
  correlationPlots_total_diversity_R[['Total20']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)

correlationPlot_total_diversity_R <- plot_grid(plotlist = correlationPlots_total_diversity_R,
                                               ncol = 2, nrow = 2,
                                               labels = c("A - Total 20m", "B - Total 40m",
                                                          "C - Diversity 20m", "D - Diversity 40m")) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/Difference_correlationPlot_total_diversity_R_spearman.png",
       correlationPlot_total_diversity_R,
       width = 24, height = 24, units = "cm", dpi = 800)

# ├├ Number of Noisy miner ----
#'TotalMiner20', 'TotalMiner40'

correlationPlots_NumberNoisyMiner <- list()
for (measure in c('TotalMiner20', 'TotalMiner40')) {
  for (indexSet in 1:length(acousticIndices_set)) {
    tmp_data <- bootCor_results_Difference[bootCor_results_Difference$Measure == measure & 
                                         bootCor_results_Difference$Index %in% names(acousticIndices_set[[indexSet]]),]
    tmp_data$Index <- fct_relevel(tmp_data$Index, names(acousticIndices_set[[indexSet]]))
    
    correlationPlots_NumberNoisyMiner[[paste0(measure, "_", names(acousticIndices_set)[indexSet])]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
      geom_vline(xintercept = seq(-0.4, 0.4, 0.2), linetype = 'dotted') +
      geom_vline(xintercept = 0, linetype = 'dashed') +
      geom_linerange(aes(xmin = 0, xmax = Mean), position = position_dodge(width = 0.4)) +
      geom_point(position = position_dodge(width = 0.4), size = 2) +
      scale_x_continuous(limits = c(-0.4, 0.4), breaks = seq(-0.4, 0.4, 0.1)) +
      scale_y_discrete(labels = acousticIndices_set[[indexSet]]) +
      scale_color_viridis_d() +
      labs(x = "Mean correlation") +
      theme_classic() +
      theme(axis.title = element_blank(),
            legend.position = "none")
  }
}


legend_bottom <- get_legend(
  correlationPlots_NumberNoisyMiner[['TotalMiner20_AP']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_NumberNoisyMiner <- plot_grid(plotlist = correlationPlots_NumberNoisyMiner,
                                              ncol = 2, nrow = 2, byrow = FALSE,
                                              labels = c("TotalMiner20", "TotalMiner40"), hjust = 0, vjust = 0, label_x = 0.1, label_y = 0.95,
                                              align = "hv",
                                              rel_heights = c(1.571429, 1)) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/Difference_correlationPlot_NumberNoisyMiner_spearman.png",
       correlationPlot_NumberNoisyMiner,
       width = 48, height = 24, units = "cm", dpi = 800)

# Best indices ----

topIndices_R1R2Combined_MeanMiner40m <- bootCor_results_R1R2Combined %>% 
  filter(Measure == 'MeanMiner40m') %>% 
  group_by(Index) %>% 
  filter(abs(Mean) == max(abs(Mean))) %>% #select highest correlation value
  filter(!between(0, Low, High))

BestIndicesPerTimePeriod <- list()
for (row in 1:nrow(topIndices_R1R2Combined_MeanMiner40m)) {
  BestIndicesPerTimePeriod[[as.character(row)]] <- acousticIndices_biodiversity$R1R2Combined %>% 
    select(Site, Season, SeasonYear, Year, Season2, type, Threshold40m, 
           paste0(topIndices_R1R2Combined_MeanMiner40m$Index[row], "_mean")) %>% 
    filter(type == topIndices_R1R2Combined_MeanMiner40m$Time[row]) %>% 
    select(-type)
}

BestIndicesPerTimePeriod <- BestIndicesPerTimePeriod %>% 
  reduce(left_join, by = c("Site", "Season", "SeasonYear", "Year", "Season2", "Threshold40m")) %>% 
  drop_na()

saveRDS(BestIndicesPerTimePeriod, "outputs/figures/bootstrapcorrelations/BestIndicesPerTimePeriod.RDS")

# Save workspace ----

save.image(file = "outputs/figures/bootstrapcorrelations/workspace.RData")