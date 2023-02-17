#PCA and multiple correlation for overall bird diversity measures (R1Only - Total20 and Detected20)

library(tidyverse)
library(factoextra) #pca tools
library(cowplot)

# Load indices and biodiversity data ----

acousticIndices_biodiversity <- readRDS("outputs/data/acousticIndices_biodiversity.RDS")

# Total20 ----
data <- acousticIndices_biodiversity$R1Only %>% 
  filter(type == "day") %>% 
  dplyr::select(c("Total20", ends_with(c("mean"))))

PCA <- prcomp(data[2:ncol(data)], scale = TRUE)

#select number of PCs to use (eigenvalues greater than 1?)
eig.val <- get_eigenvalue(PCA)

#extract PCs
PCs <- data.frame(Total20 = data$Total20, PCA$x[,which(eig.val$eigenvalue >= 1)])

cor(x = as.numeric(PCs$Total20), y = PCs$PC1, method = "spearman")

model.lm <- lm(Total20 ~ ., data = PCs)

#summary(model.lm)

sqrt(summary(model.lm)$r.squared)

ggplot(data = PCs, aes(x = Total20, y = PC1)) +
  geom_point() +
  theme_bw()

Plot_Total20 <- ggplot(data = data.frame(fitted = model.lm$fitted.values, original = data$Total20), aes(x = fitted, y = original)) +
  geom_point() +
  labs(x = "estimate", y = "Abundance (20min)") +
  theme_bw()

cor(model.lm$fitted.values, data$Total20, method = "spearman") #spearman correlation


# Detected20----
data <- acousticIndices_biodiversity$R1Only %>% 
  filter(type == "day") %>% 
  dplyr::select(c("Detected20", ends_with(c("mean"))))

PCA <- prcomp(data[2:ncol(data)], scale = TRUE)

#select number of PCs to use (eigenvalues greater than 1?)
eig.val <- get_eigenvalue(PCA)

#extract PCs
PCs <- data.frame(Detected20 = data$Detected20, PCA$x[,which(eig.val$eigenvalue >= 1)])

cor(x = as.numeric(PCs$Detected20), y = PCs$PC1, method = "spearman")

model.lm <- lm(Detected20 ~ ., data = PCs)

#summary(model.lm)

sqrt(summary(model.lm)$r.squared)

ggplot(data = PCs, aes(x = Detected20, y = PC1)) +
  geom_point() +
  theme_bw()

Plot_Detected20 <- ggplot(data = data.frame(fitted = model.lm$fitted.values, original = data$Detected20), aes(x = fitted, y = original)) +
  geom_point() +
  labs(x = "estimate", y = "Diversity (20min)") +
  theme_bw()

cor(model.lm$fitted.values, data$Detected20, method = "spearman") #spearman correlation

# Combine figures----
CombinedFigure <- plot_grid(Plot_Total20, Plot_Detected20)

ggsave(filename = "outputs/figures/pca/R1Only - MultipleCorrelationPCs - Abundance&Diversity20min.png",
       plot = CombinedFigure,
       width = 18, height = 9, units = "cm", dpi = 800)
