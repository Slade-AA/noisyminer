# Bootstrap correlation plots with a reduced set of indices - better for presentation?

# Load packages ----

library(tidyverse)
library(cowplot)
library(ggpubr)
library(boot)

#Load workspace
load(file = "outputs/figures/bootstrapcorrelations/workspace.RData")

#create acronymns for reduced set of indices
axisLabels_ReducedSet <- c('ACI_soundecology' = "ACI",
                           'ADI' = "ADI",
                           'AE' = "AEI",
                           'NDSI_soundecology' = "NDSI",
                           'M' = "M",
                           'H' = "H",
                           'Ht' = "Ht",
                           'Hf' = "Hf",
                           'BI' = "BI",
                           "SptDensity" = "SPD",
                           "ClusterCount" = "CLS",
                           "MidFreqCover" = "MFC")

# Total Diversity R1Only ----

correlationPlots_total_diversity_ReducedSet <- list()
for (measure in c('Total20', 'Total40', 'Detected20', 'Detected40')) {
  tmp_data <- bootCor_results_R1Only[bootCor_results_R1Only$Measure == measure & 
                                       bootCor_results_R1Only$Index %in% names(axisLabels_ReducedSet),]
  tmp_data$Index <- fct_relevel(tmp_data$Index, names(axisLabels_ReducedSet))
  
  correlationPlots_total_diversity_ReducedSet[[measure]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
    geom_vline(xintercept = seq(-0.8, 0.8, 0.4), linetype = 'dotted') +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_pointrange(aes(xmin = Low, xmax = High), position = position_dodge(width = 0.4)) +
    scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.8, 0.8, 0.4)) +
    scale_y_discrete(labels = axisLabels_ReducedSet) +
    scale_color_viridis_d() +
    labs(x = "Mean correlation") +
    theme_classic() +
    theme(axis.title = element_blank(),
          legend.position = "none")
}

legend_bottom <- get_legend(
  correlationPlots_total_diversity_ReducedSet[['Total20']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_total_diversity_ReducedSet <- plot_grid(plotlist = correlationPlots_total_diversity_ReducedSet,
                                                ncol = 2, nrow = 2,
                                                labels = c("A - Total 20m", "B - Total 40m",
                                                           "C - Diversity 20m", "D - Diversity 40m"),
                                                hjust = 0, label_x = 0.12) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/R1Only_correlationPlot_total_diversity_ReducedSet_spearman.png",
       correlationPlot_total_diversity_ReducedSet,
       width = 24, height = 24, units = "cm", dpi = 800)

# Total Diversity R1R2Combined ----

correlationPlots_total_diversity_ReducedSet <- list()
for (measure in c('Mean20m', 'Mean40m', 'Detected20', 'Detected40')) {
  tmp_data <- bootCor_results_R1R2Combined[bootCor_results_R1R2Combined$Measure == measure & 
                                             bootCor_results_R1R2Combined$Index %in% names(axisLabels_ReducedSet),]
  tmp_data$Index <- fct_relevel(tmp_data$Index, names(axisLabels_ReducedSet))
  
  correlationPlots_total_diversity_ReducedSet[[measure]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
    geom_vline(xintercept = seq(-0.8, 0.8, 0.4), linetype = 'dotted') +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_pointrange(aes(xmin = Low, xmax = High), position = position_dodge(width = 0.4)) +
    scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.8, 0.8, 0.4)) +
    scale_y_discrete(labels = axisLabels_ReducedSet) +
    scale_color_viridis_d() +
    labs(x = "Mean correlation") +
    theme_classic() +
    theme(axis.title = element_blank(),
          legend.position = "none")
}

legend_bottom <- get_legend(
  correlationPlots_total_diversity_ReducedSet[['Mean20m']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


correlationPlot_total_diversity_ReducedSet <- plot_grid(plotlist = correlationPlots_total_diversity_ReducedSet,
                                                ncol = 2, nrow = 2,
                                                labels = c("A - Total 20m", "B - Total 40m",
                                                           "C - Diversity 20m", "D - Diversity 40m"),
                                                hjust = 0, label_x = 0.12) %>% 
  annotate_figure(left = "Acoustic index", bottom = "Mean correlation") %>% 
  plot_grid(legend_bottom, ncol = 1, rel_heights = c(1, .1))

ggsave("outputs/figures/bootstrapcorrelations/R1R2Combined_correlationPlot_total_diversity_ReducedSet_spearman.png",
       correlationPlot_total_diversity_ReducedSet,
       width = 24, height = 24, units = "cm", dpi = 800)

# Number of Noisy miner (R1Only) ----

# Number of Noisy miner (R1R2Combined) ----