#PCA-DFA Models for predicting Noisy Miner presence

library(tidyverse)
library(MASS) #lda function
library(factoextra) #pca tools
library(yardstick) #classification performance metrics
library(ggpubr)
library(cowplot)

# Load in summary indices with biodiversity data ----

#Summary indices from Analysis Programs
Indices_Summary <- readRDS(file = "outputs/Indices_Summary_2023-07-18")

#Analysis Programs spectral indices (ACI, CVR, ENT, PMN) aggregated across different frequency bands
Indices_SpectralAggregated <- readRDS(file = "outputs/Indices_SpectralAggregated_2023-07-18")

pal <- c("#303248", "#ddb02a") #Colours extracted from picture of Noisy miner

# PCA plots per Time of Day ----

# ├ Summary Indices ----
for (measure in c("NMPresent", "Threshold20m", "Threshold40m")) {
  for (numDays in seq(1,8)) {
    PCAPlots <- list()
    for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
      data <- Indices_Summary %>% 
        filter(type == timeDay, audioDays == numDays) %>% 
        dplyr::select(c(measure, ends_with(c("median"))))
      
      PCA <- prcomp(data[2:ncol(data)], scale = TRUE)
      
      PCAPlots[[paste0(timeDay, "_", numDays)]] <- fviz_pca_ind(PCA, habillage = data[[measure]], label = "none", addEllipses = TRUE) + 
        scale_color_manual(values = pal) + 
        scale_fill_manual(values = pal) +
        theme_minimal() +
        theme(plot.title = element_blank(),
              legend.direction = "horizontal")
    }
    
    PCA_legend <- get_legend(PCAPlots[[1]])
    
    plot_grid(PCAPlots[[1]] + rremove("legend"),
              PCAPlots[[2]] + rremove("legend"),
              PCAPlots[[3]] + rremove("legend"),
              PCAPlots[[4]] + rremove("legend"),
              labels = c("A - dawn", "B - solarNoon", "C - dusk", "D - day"),
              hjust = 0, label_x = 0.1) %>% 
      plot_grid(PCA_legend,
                ncol = 1,
                rel_heights = c(1,0.1)) %>% 
      annotate_figure(top = paste0("Summary Indices - ", measure, " - ", numDays, "days")) %>% 
      ggsave(filename = paste0("outputs/figures_2023/pca/SummaryIndices/", "Summary Indices - ", measure, " - ", numDays, "days", ".png"),
             bg = "white", width = 6.5, height = 6.7)
  }
}

# ├ Spectral Aggregate Indices ----
for (measure in c("NMPresent", "Threshold20m", "Threshold40m")) {
  for (numDays in seq(1,8)) {
    PCAPlots <- list()
    for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
      data <- Indices_SpectralAggregated %>% 
        filter(type == timeDay, audioDays == numDays) %>% 
        dplyr::select(c(measure, ends_with(c("median"))))
      
      PCA <- prcomp(data[2:ncol(data)], scale = TRUE)
      
      PCAPlots[[paste0(timeDay, "_", numDays)]] <- fviz_pca_ind(PCA, habillage = data[[measure]], label = "none", addEllipses = TRUE) + 
        scale_color_manual(values = pal) + 
        scale_fill_manual(values = pal) +
        theme_minimal() +
        theme(plot.title = element_blank(),
              legend.direction = "horizontal")
    }
    
    PCA_legend <- get_legend(PCAPlots[[1]])
    
    plot_grid(PCAPlots[[1]] + rremove("legend"),
              PCAPlots[[2]] + rremove("legend"),
              PCAPlots[[3]] + rremove("legend"),
              PCAPlots[[4]] + rremove("legend"),
              labels = c("A - dawn", "B - solarNoon", "C - dusk", "D - day"),
              hjust = 0, label_x = 0.1) %>% 
      plot_grid(PCA_legend,
                ncol = 1,
                rel_heights = c(1,0.1)) %>% 
      annotate_figure(top = paste0("Spectral Aggregate Indices - ", measure, " - ", numDays, "days")) %>% 
      ggsave(filename = paste0("outputs/figures_2023/pca/SpectralAggregateIndices/", "Spectral Aggregate Indices - ", measure, " - ", numDays, "days", ".png"),
             bg = "white", width = 6.5, height = 6.7)
  }
}

# Fit DFA Models to PCA data ----

library(caret)

fit_control <- trainControl(method = "cv",
                            number = 10)
PCA_DFA_Performance <- data.frame()

# ├ Summary Indices ----

for (measure in c("NMPresent", "Threshold20m", "Threshold40m")) {
  for (numDays in seq(1,8)) {
    for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
      data <- Indices_Summary %>% 
        filter(type == timeDay, audioDays == numDays) %>% 
        dplyr::select(c(measure, ends_with(c("median"))))
      
      #perform PCA on data
      PCA <- prcomp(data[2:ncol(data)], scale = TRUE)
      
      #select number of PCs to use (eigenvalues greater than 1? PCs to obtain >80 variance explained?)
      eig.val <- get_eigenvalue(PCA)
      
      #extract PCs
      PCA.data <- bind_cols(factor(data[,1], levels = c("0", "1")),
                            PCA$x[,which(eig.val$eigenvalue >= 1)])
      
      #Fit lda model using caret and cross-validation
      lda_model <- train(...1~.,
                         data = PCA.data,
                         method = "lda",
                         metric = "Kappa",
                         trControl = fit_control)
      
      PCA_DFA_Performance <- bind_rows(PCA_DFA_Performance,
                                       bind_cols(data.frame(measure = measure,
                                                            audioDays = numDays,
                                                            timeDay = timeDay,
                                                            indices = "Summary"),
                                                 lda_model$results[,2:5]))
      
    }
  }
}


# ├ Spectral Aggregate Indices ----

for (measure in c("NMPresent", "Threshold20m", "Threshold40m")) {
  for (numDays in seq(1,8)) {
    for (timeDay in c('dawn', 'solarNoon', 'dusk', 'day')) {
      data <- Indices_SpectralAggregated %>% 
        filter(type == timeDay, audioDays == numDays) %>% 
        dplyr::select(c(measure, ends_with(c("median"))))
      
      #perform PCA on data
      PCA <- prcomp(data[2:ncol(data)], scale = TRUE)
      
      #select number of PCs to use (eigenvalues greater than 1? PCs to obtain >80 variance explained?)
      eig.val <- get_eigenvalue(PCA)
      
      #extract PCs
      PCA.data <- bind_cols(factor(data[,1], levels = c("0", "1")),
                            PCA$x[,which(eig.val$eigenvalue >= 1)])
      
      #Fit lda model using caret and cross-validation
      lda_model <- train(...1~.,
                         data = PCA.data,
                         method = "lda",
                         metric = "Kappa",
                         trControl = fit_control)
      
      PCA_DFA_Performance <- bind_rows(PCA_DFA_Performance,
                                       bind_cols(data.frame(measure = measure,
                                                            audioDays = numDays,
                                                            timeDay = timeDay,
                                                            indices = "SpectralAggregated"),
                                                 lda_model$results[,2:5]))
      
    }
  }
}