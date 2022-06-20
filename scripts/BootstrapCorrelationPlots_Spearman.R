richness_abundance <- read.csv("rawdata/SummarySurveyData2019_2021.csv")

richness_abundance <- richness_abundance %>% 
  rename(Site = SiteID) %>% 
  mutate(Date = as.character(as.Date(Date, "%d/%m/%Y")),
         Site = as.character(Site))

acousticIndices_day <- acousticIndices_day %>% 
  mutate(Date = as.character(Date))

testMerge <- left_join(x = acousticIndices_day, richness_abundance, by.x = c("Site", "Date"), by.y = c("SiteID", "Date"))

testMerge <- merge(x = acousticIndices_day, richness_abundance, by = c("Site", "Date"))



# Calculate bootstrap correlation between indices and biodiversity ----

library(boot)

bootCor_results <- list()

for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner')) {
  
  for (index in colnames(select(testMerge, ends_with(c("mean"))))) {
    set.seed(1234)#set seed for reproducibility
    bootResults <- boot(testMerge, 
                        statistic = function(data, i) {
                          cor(data[i, measure], data[i, index], method='spearman')
                        },
                        R = 1000)
    bootResultsCI <- boot.ci(bootResults, 
                             conf = 0.95, type = "bca")
    
    bootCor_results[[paste(measure, index, sep = "_")]] <- data.frame(Index = gsub("_mean", "", index),
                                                                      Measure = measure,
                                                                      Mean = mean(bootResults$t),
                                                                      Low = bootResultsCI$bca[4],
                                                                      High = bootResultsCI$bca[5])
  }
}

bootCor_results <- do.call(rbind, bootCor_results)

# Plot correlation bootstrap results ----

correlationPlots <- list()
for (measure in c('Total20m', 'Total40m', 'Diversity20m', 'Diversity40m', 'NumberNoisyMiner')) {
  tmp_data <- bootCor_results[bootCor_results$Measure == measure & 
                                bootCor_results$Index %in% indicesToUse,]
  #tmp_data$Index <- fct_relevel(tmp_data$Index, indicesToUse)
  
  correlationPlots[[measure]] <- ggplot(data = tmp_data, aes(x = Mean, y = Index)) +
    geom_vline(xintercept = 0, linetype = 'dashed') +
    geom_pointrange(aes(xmin = Low, xmax = High), position = position_dodge(width = 0.4)) +
    #geom_hline(yintercept = 1+0.5, linetype = "dotted") +
    #geom_hline(yintercept = 4+0.5, linetype = "dotted") +
    scale_x_continuous(limits = c(-0.9, 0.9), breaks = seq(-0.8, 0.8, 0.4)) +
    #scale_color_viridis_d() +
    labs(x = "Mean correlation") +
    theme_classic() +
    theme(axis.title = element_blank(),
          legend.position = "none")
}
