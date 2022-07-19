#Calculate spearman correlation values for each acoustic index

# Load packages ----

library(tidyverse)
library(cowplot)
library(ggpubr)
library(boot)

# Load indices and biodiversity data ----

acousticIndices_richness <- readRDS("outputs/data/acousticIndices_richness.RDS")
acousticIndices_richness_repscombined <- readRDS("outputs/data/acousticIndices_richness_repscombined.RDS")

# Calculate bootstrap correlation between indices and biodiversity ----

bootCor_results <- list()

for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner')) {
  for (timeperiod in unique(acousticIndices_richness$type)) {
    
    for (index in colnames(select(acousticIndices_richness, ends_with(c("mean"))))) {
      set.seed(1234)#set seed for reproducibility
      bootResults <- boot(acousticIndices_richness[acousticIndices_richness$type == timeperiod,], 
                          statistic = function(data, i) {
                            cor(data[i, measure], data[i, index], method='spearman')
                          },
                          R = 1000)
      bootResultsCI <- boot.ci(bootResults, 
                               conf = 0.95, type = "bca")
      
      bootCor_results[[paste(measure, timeperiod, index, sep = "_")]] <- data.frame(Index = gsub("_mean", "", index),
                                                                                    Time = timeperiod,
                                                                                    Measure = measure,
                                                                                    Mean = mean(bootResults$t),
                                                                                    Low = bootResultsCI$bca[4],
                                                                                    High = bootResultsCI$bca[5])
    }
  }
}

bootCor_results <- do.call(rbind, bootCor_results)

# Plot correlation bootstrap results ----

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

# ├ Total count and Diversity ----

# ├├ AP Acoustic Indices ----
correlationPlots_total_diversity_AP <- list()
for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m')) {
  tmp_data <- bootCor_results[bootCor_results$Measure == measure & 
                                bootCor_results$Index %in% names(axisLabels_AP),]
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
  correlationPlots_total_diversity_AP[['Total20m']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_total_diversity_AP <- plot_grid(plotlist = correlationPlots_total_diversity_AP,
                                                ncol = 2, nrow = 2,
                                                labels = c("A - Total 20m", "B - Total 40m",
                                                           "C - Diversity 20m", "D - Diversity 40m")) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/correlationPlot_total_diversity_AP_spearman.png",
       correlationPlot_total_diversity_AP,
       width = 24, height = 24, units = "cm", dpi = 800)

# ├├ R Acoustic Indices ----
correlationPlots_total_diversity_R <- list()
for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m')) {
  tmp_data <- bootCor_results[bootCor_results$Measure == measure & 
                                bootCor_results$Index %in% names(axisLabels_R),]
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
  correlationPlots_total_diversity_R[['Total20m']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_total_diversity_R <- plot_grid(plotlist = correlationPlots_total_diversity_R,
                                                ncol = 2, nrow = 2,
                                                labels = c("A - Total 20m", "B - Total 40m",
                                                           "C - Diversity 20m", "D - Diversity 40m")) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/correlationPlot_total_diversity_R_spearman.png",
       correlationPlot_total_diversity_R,
       width = 24, height = 24, units = "cm", dpi = 800)


# ├ Number of Noisy miner ----

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

correlationPlots_NumberNoisyMiner <- list()
for (indexSet in 1:length(acousticIndices_set)) {
  tmp_data <- bootCor_results[bootCor_results$Measure == 'NumberNoisyMiner' & 
                                bootCor_results$Index %in% names(acousticIndices_set[[indexSet]]),]
  tmp_data$Index <- fct_relevel(tmp_data$Index, names(acousticIndices_set[[indexSet]]))
  
  correlationPlots_NumberNoisyMiner[[names(acousticIndices_set)[indexSet]]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
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

legend_bottom <- get_legend(
  correlationPlots_NumberNoisyMiner[['AP']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_NumberNoisyMiner <- plot_grid(plotlist = correlationPlots_NumberNoisyMiner,
                                              ncol = 1, nrow = 2,
                                              labels = c("AP", "R"), hjust = 0, vjust = 0, label_x = 0.1, label_y = 0.95,
                                              align = "hv",
                                              rel_heights = c(1.571429, 1)) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/correlationPlot_NumberNoisyMiner_spearman.png",
       correlationPlot_NumberNoisyMiner,
       width = 24, height = 24, units = "cm", dpi = 800)