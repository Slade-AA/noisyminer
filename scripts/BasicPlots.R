#Basic scatterplots of acoustic indices and diversity, boxplots of indices and Noisy miner presence-absence

# Load packages ----

library(tidyverse)
library(cowplot)
library(ggpubr)

# Load indices and biodiversity data ----

acousticIndices_richness <- readRDS("outputs/data/acousticIndices_richness.RDS")
acousticIndices_richness_repscombined <- readRDS("outputs/data/acousticIndices_richness_repscombined.RDS")

# Plot scatterplots of individual indices and biodiversity ----

# ├ All data points ----
for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner')) {
  Scatterplots <- list()
  for (index in colnames(select(acousticIndices_richness, ends_with(c("mean"))))) {
    Scatterplots[[index]] <- ggplot(data = acousticIndices_richness[acousticIndices_richness$type == 'dawnChorus',], aes_string(x = measure, y = index)) +
      geom_point() +
      theme_bw()
  }
  
  Scatterplot <- plot_grid(plotlist = Scatterplots)
  
  ggsave(paste0("outputs/figures/basicplots/scatterplot_", measure, ".png"),
         Scatterplot,
         width = 24, height = 24, units = "cm", dpi = 800)
}

# ├ Replicates combined ----
for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner')) {
  Scatterplots_repscombined <- list()
  for (index in colnames(select(acousticIndices_richness_repscombined, ends_with(c("mean"))))) {
    Scatterplots_repscombined[[index]] <- ggplot(data = acousticIndices_richness_repscombined[acousticIndices_richness_repscombined$type == 'dawnChorus',], aes_string(x = measure, y = index)) +
      geom_point() +
      theme_bw()
  }
  
  Scatterplot_repscombined <- plot_grid(plotlist = Scatterplots_repscombined)
  
  ggsave(paste0("outputs/figures/basicplots/scatterplot_", measure, "_repscombined.png"),
         Scatterplot_repscombined,
         width = 24, height = 24, units = "cm", dpi = 800)
}

# Boxplots of acoustic indices and presence-absence noisy miner ----

# ├ All data points ----
Boxplots_pre_abs <- list()
for (index in colnames(select(acousticIndices_richness, ends_with(c("mean"))))) {
  Boxplots_pre_abs[[index]] <- ggplot(data = acousticIndices_richness[acousticIndices_richness$type == 'dawnChorus',], 
         aes_string(x = "NoisyPreAbs", y = index, fill = "NoisyPreAbs")) +
    geom_boxplot() +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(legend.position = "none")
  
  
}
Boxplot_pre_abs <- plot_grid(plotlist = Boxplots_pre_abs)

ggsave("outputs/figures/basicplots/boxplot_pre_abs.png",
       Boxplot_pre_abs,
       width = 24, height = 24, units = "cm", dpi = 800)

# ├ Replicates combined ----
Boxplots_pre_abs_repscombined <- list()
for (index in colnames(select(acousticIndices_richness, ends_with(c("mean"))))) {
  Boxplots_pre_abs_repscombined[[index]] <- ggplot(data = acousticIndices_richness_repscombined[acousticIndices_richness_repscombined$type == 'dawnChorus',], 
                                                   aes_string(x = "NoisyPreAbs", y = index, fill = "NoisyPreAbs")) +
    geom_boxplot() +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(legend.position = "none")
  
  
}
Boxplot_pre_abs_repscombined <- plot_grid(plotlist = Boxplots_pre_abs_repscombined)

ggsave("outputs/figures/basicplots/boxplot_pre_abs_repscombined.png",
       Boxplot_pre_abs_repscombined,
       width = 24, height = 24, units = "cm", dpi = 800)

# Noisy miner numbers over time ----

Plot_Noisyminersovertime <- ggplot(data = acousticIndices_richness[acousticIndices_richness$type == 'dawnChorus',], aes(x = as.Date(Date), y = NumberNoisyMiner, colour = gsub("[0-9]", "",Site))) + 
  geom_path(size = 1.5) + 
  facet_wrap(~Site) +
  labs(x = "Date", y = "Number of Noisy miners", colour = "Region") +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave("outputs/figures/basicplots/noisyminersovertime.png",
       Plot_Noisyminersovertime,
       width = 36, height = 24, units = "cm", dpi = 800)
