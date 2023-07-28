library(tidyverse)
library(cowplot)
library(ggpubr)


#Spectral Indices
spectralIndices <- readRDS("outputs/Indices_SpectralAggregated_2023-07-18")

for (measure in c('MeanMiner20m', 'MeanMiner40m')) {
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    for (numDays in unique(spectralIndices$audioDays)) {
      Scatterplots <- list()
      for (index in colnames(select(spectralIndices, ends_with(c("median"))))) {
        Scatterplots[[index]] <- ggplot(data = spectralIndices[spectralIndices$type == timeDay & spectralIndices$audioDays == numDays,], aes_string(x = measure, y = index)) +
          #labs(y = axisLabels[[gsub("_median", "", index)]]) +
          geom_smooth(method = "gam") +
          geom_point() +
          theme_bw()
      }
      Scatterplot <- plot_grid(plotlist = Scatterplots)
      
      ggsave(paste0("outputs/figures_2023/scatterplots_NMNumbers/scatterplot_", measure, "_", timeDay, "_", numDays, ".png"),
             Scatterplot,
             width = 24, height = 24, units = "cm", dpi = 800)
    }
  }
}

#Summary Indices
summaryIndices <- readRDS("outputs/Indices_Summary_2023-07-18")

for (measure in c('MeanMiner20m', 'MeanMiner40m')) {
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    for (numDays in 1:9) {
      Scatterplots <- list()
      for (index in colnames(select(summaryIndices, ends_with(c("median"))))) {
        Scatterplots[[index]] <- ggplot(data = summaryIndices[summaryIndices$type == timeDay & summaryIndices$audioDays == numDays,], aes_string(x = measure, y = index)) +
          #labs(y = axisLabels[[gsub("_median", "", index)]]) +
          geom_smooth(method = "gam") +
          geom_point() +
          theme_bw()
      }
      Scatterplot <- plot_grid(plotlist = Scatterplots)
      
      ggsave(paste0("outputs/figures_2023/scatterplots_NMNumbers/SummaryIndices/scatterplot_", measure, "_", timeDay, "_", numDays, ".png"),
             Scatterplot,
             width = 24, height = 24, units = "cm", dpi = 800, bg = "white")
    }
  }
}


#R Indices

RIndices <- readRDS("outputs/Indices_R_2023-07-26")

for (measure in c('MeanMiner20m', 'MeanMiner40m')) {
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    for (numDays in 1:9) {
      Scatterplots <- list()
      for (index in colnames(select(RIndices, ends_with(c("median"))))) {
        Scatterplots[[index]] <- ggplot(data = RIndices[RIndices$type == timeDay & RIndices$audioDays == numDays,], aes_string(x = measure, y = index)) +
          #labs(y = axisLabels[[gsub("_median", "", index)]]) +
          geom_smooth(method = "gam") +
          geom_point() +
          theme_bw()
      }
      Scatterplot <- plot_grid(plotlist = Scatterplots,
                               ncol = 5, nrow = 4)
      
      ggsave(paste0("outputs/figures_2023/scatterplots_NMNumbers/RIndices/scatterplot_", measure, "_", timeDay, "_", numDays, ".png"),
             Scatterplot,
             width = 30, height = 24, units = "cm", dpi = 800, bg = "white")
    }
  }
}