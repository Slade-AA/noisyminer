#Basic scatterplots of acoustic indices and diversity, boxplots of indices and Noisy miner presence-absence

# Load packages ----

library(tidyverse)
library(cowplot)
library(ggpubr)

# Load indices and biodiversity data ----

acousticIndices_biodiversity <- readRDS("outputs/data/acousticIndices_biodiversity.RDS")

# Plot scatterplots of individual indices and biodiversity ----

# ├ R1Only ----

dir.create("outputs/figures/basicplots/R1Only/scatterplots")

for (measure in c('Total20', 'Total40', 'Detected20', 'Detected40', 'TotalMiner20', 'TotalMiner40')) {
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    Scatterplots <- list()
    for (index in colnames(select(acousticIndices_biodiversity$R1Only, ends_with(c("mean"))))) {
      Scatterplots[[index]] <- ggplot(data = acousticIndices_biodiversity$R1Only[acousticIndices_biodiversity$R1Only$type == timeDay,], aes_string(x = measure, y = index)) +
        geom_point() +
        theme_bw()
    }
    Scatterplot <- plot_grid(plotlist = Scatterplots)
    
    ggsave(paste0("outputs/figures/basicplots/R1Only/scatterplots/scatterplot_", measure, "_", timeDay, ".png"),
           Scatterplot,
           width = 24, height = 24, units = "cm", dpi = 800)
  }
}

# ├ R1R2Combined ----

dir.create("outputs/figures/basicplots/R1R2Combined/scatterplots")

for (measure in c('Mean20m', 'Mean40m', 'Detected20', 'Detected40', 'MeanMiner20m', 'MeanMiner40m')) {
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    Scatterplots <- list()
    for (index in colnames(select(acousticIndices_biodiversity$R1R2Combined, ends_with(c("mean"))))) {
      Scatterplots[[index]] <- ggplot(data = acousticIndices_biodiversity$R1R2Combined[acousticIndices_biodiversity$R1R2Combined$type == timeDay,], aes_string(x = measure, y = index)) +
        geom_point() +
        theme_bw()
    }
    Scatterplot <- plot_grid(plotlist = Scatterplots)
    
    ggsave(paste0("outputs/figures/basicplots/R1R2Combined/scatterplots/scatterplot_", measure, "_", timeDay, ".png"),
           Scatterplot,
           width = 24, height = 24, units = "cm", dpi = 800)
  }
}

# Boxplots of acoustic indices and presence-absence noisy miner ----

# ├ R1Only ----

acousticIndices_biodiversity$R1Only <- acousticIndices_biodiversity$R1Only %>% mutate(NMPresent = factor(NMPresent),
                                                                                      Threshold20m = factor(Threshold20m),
                                                                                      Threshold40m = factor(Threshold40m))
dir.create("outputs/figures/basicplots/R1Only/boxplots")

for (measure in c('NMPresent', 'Threshold20m', 'Threshold40m')) {
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    Boxplots_pre_abs <- list()
    for (index in colnames(select(acousticIndices_biodiversity$R1Only, ends_with(c("mean"))))) {
      Boxplots_pre_abs[[index]] <- ggplot(data = acousticIndices_biodiversity$R1Only[acousticIndices_biodiversity$R1Only$type == timeDay,], 
                                          aes_string(x = measure, y = index, fill = measure)) +
        geom_boxplot() +
        scale_fill_viridis_d() +
        theme_bw() +
        theme(legend.position = "none")
      
      
    }
    Boxplot_pre_abs <- plot_grid(plotlist = Boxplots_pre_abs)
    
    ggsave(paste0("outputs/figures/basicplots/R1Only/boxplots/boxplot_", measure, "_", timeDay, ".png"),
           Boxplot_pre_abs,
           width = 24, height = 24, units = "cm", dpi = 800)
  }
}

# ├ R1R2Combined ----

acousticIndices_biodiversity$R1R2Combined <- acousticIndices_biodiversity$R1R2Combined %>% mutate(NMPresent = factor(NMPresent),
                                                                                                  Threshold20m = factor(Threshold20m),
                                                                                                  Threshold40m = factor(Threshold40m))

dir.create("outputs/figures/basicplots/R1R2Combined/boxplots")

for (measure in c('NMPresent', 'Threshold20m', 'Threshold40m')) {
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    Boxplots_pre_abs <- list()
    for (index in colnames(select(acousticIndices_biodiversity$R1R2Combined, ends_with(c("mean"))))) {
      Boxplots_pre_abs[[index]] <- ggplot(data = acousticIndices_biodiversity$R1R2Combined[acousticIndices_biodiversity$R1R2Combined$type == timeDay,], 
                                          aes_string(x = measure, y = index, fill = measure)) +
        geom_boxplot() +
        scale_fill_viridis_d() +
        theme_bw() +
        theme(legend.position = "none")
      
      
    }
    Boxplot_pre_abs <- plot_grid(plotlist = Boxplots_pre_abs)
    
    ggsave(paste0("outputs/figures/basicplots/R1R2Combined/boxplots/boxplot_", measure, "_", timeDay, ".png"),
           Boxplot_pre_abs,
           width = 24, height = 24, units = "cm", dpi = 800)
  }
}





# ├ All data points ----
for (timeDay in c('dawnChorus', 'solarNoon', 'eveningChorus', 'day')) {
  Boxplots_pre_abs <- list()
  for (index in colnames(select(acousticIndices_richness, ends_with(c("mean"))))) {
    Boxplots_pre_abs[[index]] <- ggplot(data = acousticIndices_richness[acousticIndices_richness$type == timeDay,], 
                                        aes_string(x = "NoisyPreAbs", y = index, fill = "NoisyPreAbs")) +
      geom_boxplot() +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "none")
    
    
  }
  Boxplot_pre_abs <- plot_grid(plotlist = Boxplots_pre_abs)
  
  ggsave(paste0("outputs/figures/basicplots/boxplot_pre_abs_", timeDay, ".png"),
         Boxplot_pre_abs,
         width = 24, height = 24, units = "cm", dpi = 800)
}


# ├ Replicates combined ----
for (timeDay in c('dawnChorus', 'solarNoon', 'eveningChorus', 'day')) {
  Boxplots_pre_abs_repscombined <- list()
  for (index in colnames(select(acousticIndices_richness, ends_with(c("mean"))))) {
    Boxplots_pre_abs_repscombined[[index]] <- ggplot(data = acousticIndices_richness_repscombined[acousticIndices_richness_repscombined$type == timeDay,], 
                                                     aes_string(x = "NoisyPreAbs", y = index, fill = "NoisyPreAbs")) +
      geom_boxplot() +
      scale_fill_viridis_d() +
      theme_bw() +
      theme(legend.position = "none")
    
    
  }
  Boxplot_pre_abs_repscombined <- plot_grid(plotlist = Boxplots_pre_abs_repscombined)
  
  ggsave(paste0("outputs/figures/basicplots/boxplot_pre_abs_", timeDay, "_repscombined.png"),
         Boxplot_pre_abs_repscombined,
         width = 24, height = 24, units = "cm", dpi = 800)
}


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













# Boxplots broken up by site and season ----

timeDay <- 'dawnChorus'
timeDay <- 'solarNoon'
timeDay <- 'eveningChorus'
timeDay <- 'day'

Boxplots_pre_abs <- list()
for (index in colnames(select(acousticIndices_richness, ends_with(c("mean"))))) {
  Boxplots_pre_abs[[index]] <- ggplot(data = acousticIndices_richness[acousticIndices_richness$type == timeDay,], 
                                      aes_string(x = "NoisyPreAbs", y = index, fill = "NoisyPreAbs")) +
    geom_boxplot() +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(legend.position = "none")
}

Boxplots_pre_abs_Site <- list()
for (index in colnames(select(acousticIndices_richness, ends_with(c("mean"))))) {
  Boxplots_pre_abs_Site[[index]] <- ggplot(data = acousticIndices_richness[acousticIndices_richness$type == timeDay,], 
                                           aes_string(x = "NoisyPreAbs", y = index, fill = "NoisyPreAbs")) +
    geom_boxplot() +
    scale_fill_viridis_d() +
    facet_wrap(~Site) +
    theme_bw() +
    theme(legend.position = "none")
}

Boxplots_pre_abs_Season <- list()
for (index in colnames(select(acousticIndices_richness, ends_with(c("mean"))))) {
  Boxplots_pre_abs_Season[[index]] <- ggplot(data = acousticIndices_richness[acousticIndices_richness$type == timeDay,], 
                                             aes_string(x = "NoisyPreAbs", y = index, fill = "NoisyPreAbs")) +
    geom_boxplot() +
    scale_fill_viridis_d() +
    facet_wrap(~season) +
    theme_bw() +
    theme(legend.position = "none")
}