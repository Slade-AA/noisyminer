library(tidyverse)
library(colorspace)
library(ggpubr)
library(cowplot)

#Custom palette
pal <- c("#303248", "#ddb02a") #Colours extracted from picture of Noisy miner

#Function to add the sample size to ggplot
add_sample <- function(x){
  return(c(y = min(x) - .025, #plots the sample size below the minimum for each group (can replace with max and + if desired)
           label = length(x)))
}

# Load in summary indices with biodiversity data ----

#Summary indices from Analysis Programs
Indices_Summary <- readRDS(file = "outputs/Indices_Summary_2023-07-18")

#Analysis Programs spectral indices (ACI, CVR, ENT, PMN) aggregated across different frequency bands
Indices_SpectralAggregated <- readRDS(file = "outputs/Indices_SpectralAggregated_2023-07-18")

Indices_R <- readRDS("outputs/Indices_R_2023-07-26")


# Spectral Indices ----

for (measure in c("NMPresent", "Threshold20m", "Threshold40m")) {
  for (numDays in seq(1,9)) {
    for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
      data <- Indices_SpectralAggregated %>% 
        filter(type == timeDay, audioDays == numDays) %>% 
        dplyr::select(c(measure, ends_with(c("median"))))
      
      data[,1] <- as.factor(data[,1])
      
      
      Plots_Raincloud <- list()
      for (index in colnames(select(data, ends_with(c("median"))))) {
        Plots_Raincloud[[index]] <- ggplot(data, aes(x = .data[[measure]], y = .data[[index]])) + 
          ggdist::stat_halfeye(aes(color = .data[[measure]],
                                   fill = after_scale(lighten(color, 0.5))),
                               adjust = .5, 
                               width = .4, 
                               .width = 0, 
                               justification = -.4, 
                               point_colour = NA) + 
          geom_boxplot(aes(colour = .data[[measure]], 
                           colour = after_scale(darken(colour, 0.2, space = "HLS")),
                           fill = after_scale(desaturate(lighten(color, .8), .4))),
                       width = .25, 
                       outlier.shape = NA) +
          geom_point(aes(color = .data[[measure]], 
                         fill = .data[[measure]],
                         color = after_scale(darken(color, 0.2, space = "HLS"))),
                     size = 1.3,
                     alpha = .3,
                     position = position_jitter(
                       seed = 1, width = .1)) + 
          stat_summary(geom = "text",
                       fun.data = add_sample,
                       aes(label = paste("n =", ..label..),
                           color = .data[[measure]],
                           color = after_scale(darken(color, .3, space = "HLS"))),
                       size = 4,
                       hjust = 0) +
          geom_text(aes(label = paste0("Wilcoxon, p = ", with(.data, round(wilcox.test(.data[[index]]~.data[[measure]])$p.value, 3)),
                                       "\n Cohen's D = ", with(.data, round(effsize::cohen.d(.data[[index]],.data[[measure]])$estimate, 3))),
                        x = -Inf, y = Inf, hjust = -0.7, vjust = 1.5),
                    check_overlap = TRUE, size = 3.3) +
          #stat_compare_means(label.x.npc = 0.45, hjust = 0.3) +
          #stat_compare_means(method = "wilcox.test", 
          #                   aes(label = paste0("Wilcoxon, p = ",after_stat(p.adj),
          #                                      "; \nCohen's D = ",
          #                                      with(data, round(effsize::cohen.d(data[[index]],data[[measure]])$estimate, 3)))),
          #                   label.x.npc = 0.45, hjust = 0.3, vjust = 0.8) +
          scale_color_manual(values = pal, guide = "none") +
          scale_fill_manual(values = pal, guide = "none") +
          labs(x = measure, y = index) +
          theme_classic() +
          theme(legend.position = "none")
      }
      plot_grid(plotlist = Plots_Raincloud,
                ncol = 4, nrow = 4) %>% 
        annotate_figure(top = paste0("Spectral Indices - ", measure, " - ", timeDay, " - ", numDays, "days")) %>% 
        ggsave(filename = paste0("outputs/figures_2023/boxplots_NMPresence/", "Spectral Indices - ", measure, " - ", timeDay, " - ", numDays, "days", ".png"),
               bg = "white", width = 12, height = 12)
    }
  }
}


# Summary Indices ----

for (measure in c("NMPresent", "Threshold20m", "Threshold40m")) {
  for (numDays in seq(1,9)) {
    for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
      data <- Indices_Summary %>% 
        filter(type == timeDay, audioDays == numDays) %>% 
        dplyr::select(c(measure, ends_with(c("median"))))
      
      data[,1] <- as.factor(data[,1])
      
      
      Plots_Raincloud <- list()
      for (index in colnames(select(data, ends_with(c("median"))))) {
        Plots_Raincloud[[index]] <- ggplot(data, aes(x = .data[[measure]], y = .data[[index]])) + 
          ggdist::stat_halfeye(aes(color = .data[[measure]],
                                   fill = after_scale(lighten(color, 0.5))),
                               adjust = .5, 
                               width = .4, 
                               .width = 0, 
                               justification = -.4, 
                               point_colour = NA) + 
          geom_boxplot(aes(colour = .data[[measure]], 
                           colour = after_scale(darken(colour, 0.2, space = "HLS")),
                           fill = after_scale(desaturate(lighten(color, .8), .4))),
                       width = .25, 
                       outlier.shape = NA) +
          geom_point(aes(color = .data[[measure]], 
                         fill = .data[[measure]],
                         color = after_scale(darken(color, 0.2, space = "HLS"))),
                     size = 1.3,
                     alpha = .3,
                     position = position_jitter(
                       seed = 1, width = .1)) + 
          stat_summary(geom = "text",
                       fun.data = add_sample,
                       aes(label = paste("n =", ..label..),
                           color = .data[[measure]],
                           color = after_scale(darken(color, .3, space = "HLS"))),
                       size = 4,
                       hjust = 0) +
          geom_text(aes(label = paste0("Wilcoxon, p = ", with(.data, round(wilcox.test(.data[[index]]~.data[[measure]])$p.value, 3)),
                                       "\n Cohen's D = ", with(.data, round(effsize::cohen.d(.data[[index]],.data[[measure]])$estimate, 3))),
                        x = -Inf, y = Inf, hjust = -0.7, vjust = 1.5),
                    check_overlap = TRUE, size = 3.3) +
          #stat_compare_means(label.x.npc = 0.45, hjust = 0.3) +
          scale_color_manual(values = pal, guide = "none") +
          scale_fill_manual(values = pal, guide = "none") +
          labs(x = measure, y = index) +
          theme_classic() +
          theme(legend.position = "none")
      }
      plot_grid(plotlist = Plots_Raincloud,
                ncol = 4, nrow = 3) %>% 
        annotate_figure(top = paste0("Summary Indices - ", measure, " - ", timeDay, " - ", numDays, "days")) %>% 
        ggsave(filename = paste0("outputs/figures_2023/boxplots_NMPresence/SummaryIndices/", "Summary Indices - ", measure, " - ", timeDay, " - ", numDays, "days", ".png"),
               bg = "white", width = 12, height = 9)
    }
  }
}

# R Indices ----

for (measure in c("NMPresent", "Threshold20m", "Threshold40m")) {
  for (numDays in seq(1,9)) {
    for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
      data <- Indices_R %>% 
        filter(type == timeDay, audioDays == numDays) %>% 
        dplyr::select(c(measure, ends_with(c("median"))))
      
      data[,1] <- as.factor(data[,1])
      
      
      Plots_Raincloud <- list()
      for (index in colnames(select(data, ends_with(c("median"))))) {
        Plots_Raincloud[[index]] <- ggplot(data, aes(x = .data[[measure]], y = .data[[index]])) + 
          ggdist::stat_halfeye(aes(color = .data[[measure]],
                                   fill = after_scale(lighten(color, 0.5))),
                               adjust = .5, 
                               width = .4, 
                               .width = 0, 
                               justification = -.4, 
                               point_colour = NA) + 
          geom_boxplot(aes(colour = .data[[measure]], 
                           colour = after_scale(darken(colour, 0.2, space = "HLS")),
                           fill = after_scale(desaturate(lighten(color, .8), .4))),
                       width = .25, 
                       outlier.shape = NA) +
          geom_point(aes(color = .data[[measure]], 
                         fill = .data[[measure]],
                         color = after_scale(darken(color, 0.2, space = "HLS"))),
                     size = 1.3,
                     alpha = .3,
                     position = position_jitter(
                       seed = 1, width = .1)) + 
          stat_summary(geom = "text",
                       fun.data = add_sample,
                       aes(label = paste("n =", ..label..),
                           color = .data[[measure]],
                           color = after_scale(darken(color, .3, space = "HLS"))),
                       size = 4,
                       hjust = 0) +
          geom_text(aes(label = paste0("Wilcoxon, p = ", with(.data, round(wilcox.test(.data[[index]]~.data[[measure]])$p.value, 3)),
                                       "\n Cohen's D = ", with(.data, round(effsize::cohen.d(.data[[index]],.data[[measure]])$estimate, 3))),
                        x = -Inf, y = Inf, hjust = -0.7, vjust = 1.5),
                    check_overlap = TRUE, size = 3.3) +
          #stat_compare_means(label.x.npc = 0.45, hjust = 0.3) +
          scale_color_manual(values = pal, guide = "none") +
          scale_fill_manual(values = pal, guide = "none") +
          labs(x = measure, y = index) +
          theme_classic() +
          theme(legend.position = "none")
      }
      plot_grid(plotlist = Plots_Raincloud,
                ncol = 5, nrow = 4) %>% 
        annotate_figure(top = paste0("R Indices - ", measure, " - ", timeDay, " - ", numDays, "days")) %>% 
        ggsave(filename = paste0("outputs/figures_2023/boxplots_NMPresence/RIndices/", "R Indices-", measure, "-", timeDay, "-", numDays, "days", ".png"),
               bg = "white", width = 15, height = 12)
    }
  }
}