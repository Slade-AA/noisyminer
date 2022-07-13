#Calculate spearman correlation values for each acoustic index

# Load packages ----

library(tidyverse)
library(cowplot)
library(ggpubr)

# Load indices and biodiversity data ----

acousticIndices_richness <- readRDS("outputs/data/acousticIndices_richness.RDS")
acousticIndices_richness_repscombined <- readRDS("outputs/data/acousticIndices_richness_repscombined.RDS")

# Calculate bootstrap correlation between indices and biodiversity ----

library(boot)

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
axisLabels <- c("Activity" = "ACT", 
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

# ├ Total count and Diversity ----
correlationPlots_total_diversity <- list()
for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m')) {
  tmp_data <- bootCor_results[bootCor_results$Measure == measure & 
                                bootCor_results$Index %in% names(axisLabels),]
  tmp_data$Index <- fct_relevel(tmp_data$Index, names(axisLabels))
  
  correlationPlots_total_diversity[[measure]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_pointrange(aes(xmin = Low, xmax = High), position = position_dodge(width = 0.4)) +
    scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.8, 0.8, 0.4)) +
    scale_y_discrete(labels = axisLabels) +
    scale_color_viridis_d() +
    labs(x = "Mean correlation") +
    theme_classic() +
    theme(axis.title = element_blank(),
          legend.position = "none")
}

legend_bottom <- get_legend(
  correlationPlots_total_diversity[['Total20m']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_total_diversity <- plot_grid(plotlist = correlationPlots_total_diversity,
          ncol = 2, nrow = 2,
          labels = c("A - Total 20m", "B - Total 40m",
                     "C - Diversity 20m", "D - Diversity 40m")) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/correlationPlot_total_diversity_spearman.png",
       correlationPlot_total_diversity,
       width = 24, height = 24, units = "cm", dpi = 800)

# ├ Number of Noisy miner ----
