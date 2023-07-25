#Raincloud plots

pal <- c("#303248", "#ddb02a") #Colours extracted from picture of Noisy miner

#Function to add the sample size to ggplot
add_sample <- function(x){
  return(c(y = min(x) - .025, #plots the sample size below the minimum for each group (can replace with max and + if desired)
           label = length(x)))
}

library(tidyverse)
library(colorspace)
library(ggpubr)

data[,1] <- as.factor(data[,1])

ggplot(data, aes(x = Threshold40m, y = ACI_Band_1.5_4.0_median)) + 
  ggdist::stat_halfeye(aes(color = Threshold40m,
                           fill = after_scale(lighten(color, 0.5))),
                       adjust = .5, 
                       width = .6, 
                       .width = 0, 
                       justification = -.3, 
                       point_colour = NA) + 
  geom_boxplot(aes(colour = Threshold40m, 
                   colour = after_scale(darken(colour, 0.2, space = "HLS")),
                   fill = after_scale(desaturate(lighten(color, .8), .4))),
               width = .25, 
               outlier.shape = NA) +
  geom_point(aes(color = Threshold40m, 
                 fill = Threshold40m,
                 color = after_scale(darken(color, 0.2, space = "HLS"))),
             size = 1.3,
             alpha = .3,
             position = position_jitter(
               seed = 1, width = .1)) + 
  stat_summary(geom = "text",
               fun.data = add_sample,
               aes(label = paste("n =", ..label..),
                   color = Threshold40m,
                   color = after_scale(darken(color, .3, space = "HLS"))),
               size = 4,
               hjust = 0) +
  stat_compare_means(label.x.npc = 0.5, hjust = 0.3) +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values = pal, guide = "none") +
  labs(x = "Threshold40m") +
  theme_classic() +
  theme(legend.position = "none")


compare_means(ACI_Band_1.5_4.0_median ~ Threshold40m, 
              data = data, 
              method = "wilcox.test")
