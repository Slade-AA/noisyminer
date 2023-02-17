BestIndicesPerTimePeriod_R1Only <- list()
for (measure in c('TotalMiner20', 'TotalMiner40')) {
  topIndices <- bootCor_results_R1Only %>% 
    filter(Measure == measure) %>% 
    group_by(Index) %>% 
    filter(abs(Mean) == max(abs(Mean))) %>% #select highest correlation value
    filter(!between(0, Low, High))
  
  BestIndicesPerTimePeriod <- list()
  for (row in 1:nrow(topIndices)) {
    BestIndicesPerTimePeriod[[as.character(row)]] <- acousticIndices_biodiversity$R1Only %>% 
      select(Site, Season, SeasonYear, Year, Season2, type, measure, 
             paste0(topIndices$Index[row], "_mean")) %>% 
      filter(type == topIndices$Time[row]) %>% 
      select(-type)
  }
  
  BestIndicesPerTimePeriod_R1Only[[measure]] <- BestIndicesPerTimePeriod %>% 
    reduce(left_join, by = c("Site", "Season", "SeasonYear", "Year", "Season2", measure)) %>% 
    drop_na()
}

BestIndicesPerTimePeriod_R1R2Combined <- list()
for (measure in c('MeanMiner20m' = "Threshold20m", 'MeanMiner40m' = "Threshold40m")) {
  topIndices <- bootCor_results_R1R2Combined %>% 
    filter(Measure == names(measure)) %>% 
    group_by(Index) %>% 
    filter(abs(Mean) == max(abs(Mean))) %>% #select highest correlation value
    filter(!between(0, Low, High))
  
  BestIndicesPerTimePeriod <- list()
  for (row in 1:nrow(topIndices)) {
    BestIndicesPerTimePeriod[[as.character(row)]] <- acousticIndices_biodiversity$R1R2Combined %>% 
      select(Site, Season, SeasonYear, Year, Season2, type, measure[[1]], 
             paste0(topIndices$Index[row], "_mean")) %>% 
      filter(type == topIndices$Time[row]) %>% 
      select(-type)
  }
  
  BestIndicesPerTimePeriod_R1R2Combined[[measure]] <- BestIndicesPerTimePeriod %>% 
    reduce(left_join, by = c("Site", "Season", "SeasonYear", "Year", "Season2", measure[[1]])) %>% 
    drop_na()
}