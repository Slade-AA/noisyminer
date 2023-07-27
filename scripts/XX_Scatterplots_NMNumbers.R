library(tidyverse)
library(cowplot)
library(ggpubr)

spectralIndices <- readRDS("outputs/Indices_SpectralAggregated_2023-07-18")



for (measure in c('MeanMiner20m', 'MeanMiner40m')) {
  for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
    for (numDays in unique(spectralIndices$audioDays)) {
      Scatterplots <- list()
      for (index in colnames(select(spectralIndices, ends_with(c("median"))))) {
        Scatterplots[[index]] <- ggplot(data = spectralIndices[spectralIndices$type == timeDay & spectralIndices$audioDays == numDays,], aes_string(x = measure, y = index)) +
          #labs(y = axisLabels[[gsub("_median", "", index)]]) +
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