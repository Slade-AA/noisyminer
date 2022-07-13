#Calculate spearman correlation values for each acoustic index

# Load packages ----

library(tidyverse)
library(cowplot)

# Load indices and biodiversity data ----

# ├ richness ----

richness_abundance <- read.csv("rawdata/SummarySurveyData2019_2021.csv")

richness_abundance <- richness_abundance[complete.cases(richness_abundance),] #remove any NA rows
richness_abundance$SurveyID <- gsub(" ", "", richness_abundance$SurveyID) #remove spaces from SurveyID
richness_abundance$SiteID <- gsub(" ", "", richness_abundance$SiteID) #remove spaces from SiteID

richness_abundance <- richness_abundance %>% 
  rename(Site = SiteID) %>% 
  mutate(Date = as.character(as.Date(Date, "%d/%m/%Y")),
         Site = as.character(Site))

#richness data frame has some duplicates (127, 129, 6 look to be errors)
richness_abundance_dups <- richness_abundance[richness_abundance$SurveyID %in% richness_abundance$SurveyID[which(duplicated(richness_abundance$SurveyID))],]

richness_abundance <- richness_abundance[-c(127, 129, 6),] #remove incorrect duplicates

#extract 'season', 'seasonYear' and 'replicate' from 'SurveyID'
richness_abundance <- richness_abundance %>% mutate(season = gsub("^[A-Z]{1,2}[0-9]{1,2}([A-Za-z]{6})[0-9]{5}", "\\1", SurveyID),
                                                    seasonYear = gsub("^[A-Z]{1,2}[0-9]{1,2}([A-Za-z]{6}[0-9]{4})[0-9]{1}", "\\1", SurveyID),
                                                    replicate = gsub("^[A-Z]{1,2}[0-9]{1,2}[A-Za-z]{6}[0-9]{4}", "", SurveyID))


# ├ acoustic indices ----

files <- file.info(list.files("./outputs/data/", pattern = ".*_acousticIndices_summary.RData$", full.names = TRUE)) #list files
latestFile <- rownames(files)[which.max(files$mtime)] #determine most recent file to use for loading

load(latestFile)


# Merge indices and biodiversity data ----

acousticIndices_richness  <- left_join(x = acousticIndices_summary, richness_abundance)
acousticIndices_richness <- acousticIndices_richness[complete.cases(acousticIndices_richness),] #remove any NA rows

acousticIndices_richness <- acousticIndices_richness %>% filter(p > 0.7) #remove datapoints where less than 70% of audio was available



# Calculate bootstrap correlation between indices and biodiversity ----

library(boot)

bootCor_results <- list()

for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner')) {
  for (timeperiod in unique(acousticIndices_richness$type)) {
    
    for (index in colnames(select(acousticIndices_richness, ends_with(c("mean"))))) {
      set.seed(1234)#set seed for reproducibility
      bootResults <- boot(acousticIndices_richness[acousticIndices_richness$type == timeperiod,], 
                          statistic = function(data, i) {
                            cor(data[i, measure], data[i, index], method='spearman')
                          },
                          R = 1000)
      bootResultsCI <- boot.ci(bootResults, 
                               conf = 0.95, type = "bca")
      
      bootCor_results[[paste(measure, timeperiod, index, sep = "_")]] <- data.frame(Index = gsub("_mean", "", index),
                                                                                    Time = timeperiod,
                                                                                    Measure = measure,
                                                                                    Mean = mean(bootResults$t),
                                                                                    Low = bootResultsCI$bca[4],
                                                                                    High = bootResultsCI$bca[5])
    }
  }
}

bootCor_results <- do.call(rbind, bootCor_results)

# Plot correlation bootstrap results ----

correlationPlots <- list()
for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner')) {
  tmp_data <- bootCor_results[bootCor_results$Measure == measure,]# & 
                                #bootCor_results$Index %in% indicesToUse,]
  #tmp_data$Index <- fct_relevel(tmp_data$Index, indicesToUse)
  
  correlationPlots[[measure]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index, group = Time, colour = Time)) +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_pointrange(aes(xmin = Low, xmax = High), position = position_dodge(width = 0.4)) +
    #geom_hline(yintercept = 1+0.5, linetype = "dotted") +
    #geom_hline(yintercept = 4+0.5, linetype = "dotted") +
    scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.8, 0.8, 0.4)) +
    scale_color_viridis_d() +
    labs(x = "Mean correlation") +
    theme_classic() +
    theme(axis.title = element_blank(),
          legend.position = "none")
}

legend_bottom <- get_legend(
  correlationPlots[['Total20m']] + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom", legend.direction = "horizontal", legend.title = element_blank())
)


plot_grid(correlationPlots$Total20m, correlationPlots$Total40m,
          correlationPlots$Diversity20m, correlationPlots$Diversity40m,
          ncol = 2, nrow = 2,
          labels = c("A - Total 20m", "B - Total 40m",
                     "C - Diversity 20m", "D - Diversity 40m"))
