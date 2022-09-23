#Basic scatterplots of acoustic indices and diversity, boxplots of indices and Noisy miner presence-absence

# Load packages ----

library(tidyverse)
library(cowplot)
library(ggpubr)

# Load indices and biodiversity data ----

acousticIndices_biodiversity <- readRDS("outputs/data/acousticIndices_biodiversity.RDS")

# Plot scatterplots of individual indices and continuous biodiversity measures ----

# Acronyms of acoustic indices for axis labels
axisLabels <- c("Activity" = "ACT", #AP indices
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
                "ENT_ND" = bquote("ENT"[diff]),
                'ACI_soundecology' = "ACI", # R indices
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

# ├ R1Only ----

dir.create("outputs/figures/basicplots/R1Only/scatterplots")

for (measure in c('Total20', 'Total40', 'Detected20', 'Detected40', 'TotalMiner20', 'TotalMiner40')) {
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    Scatterplots <- list()
    for (index in colnames(select(acousticIndices_biodiversity$R1Only, ends_with(c("mean"))))) {
      Scatterplots[[index]] <- ggplot(data = acousticIndices_biodiversity$R1Only[acousticIndices_biodiversity$R1Only$type == timeDay,], aes_string(x = measure, y = index)) +
        labs(y = axisLabels[[gsub("_mean", "", index)]]) +
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
        labs(y = axisLabels[[gsub("_mean", "", index)]]) +
        geom_point() +
        theme_bw()
    }
    Scatterplot <- plot_grid(plotlist = Scatterplots)
    
    ggsave(paste0("outputs/figures/basicplots/R1R2Combined/scatterplots/scatterplot_", measure, "_", timeDay, ".png"),
           Scatterplot,
           width = 24, height = 24, units = "cm", dpi = 800)
  }
}

# Boxplots of acoustic indices and binary noisy miner measures (presence, threshold20m and threshold40m) ----

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
        labs(y = axisLabels[[gsub("_mean", "", index)]]) +
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
        labs(y = axisLabels[[gsub("_mean", "", index)]]) +
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