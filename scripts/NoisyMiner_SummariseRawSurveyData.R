#Noisy miner data
library(tidyverse)

rawData <- read.csv("rawdata/2019-2023MasterData26May2023.csv")


#Create summary for repeated visits
summaryData <- rawData %>% 
  select(-SurveyID) %>% 
  mutate(SurveyIDR12 = paste(SiteID, Survey, Year, sep = "_"),
         .before = Year) %>% 
  group_by(SurveyIDR12, SiteID) %>% 
  summarise(Mean20m = case_when(
    length(unique(Repeat)) == 1  ~ mean(c(sum(Total20[Repeat == 1]))),
    length(unique(Repeat)) == 2  ~ mean(c(sum(Total20[Repeat == 1]), sum(Total20[Repeat == 2])))
  ),
  Mean40m = case_when(
    length(unique(Repeat)) == 1  ~ mean(c(sum(Total40[Repeat == 1]))),
    length(unique(Repeat)) == 2  ~ mean(c(sum(Total40[Repeat == 1]), sum(Total40[Repeat == 2])))
  ),
  Detected20 = length(unique(Species[Detected20 > 0])),
  Detected40 = length(unique(Species[Detected40 > 0])),
  NMPresent = max(NMPresent),
  TotalMiner20 = sum(Total20[Species == 'Noisy Miner']),
  TotalMiner40 = sum(Total40[Species == 'Noisy Miner']),
  MeanMiner20m = case_when(
    length(unique(Repeat)) == 1  ~ mean(c(sum(Total20[Repeat == 1 & Species == 'Noisy Miner']))),
    length(unique(Repeat)) == 2  ~ mean(c(sum(Total20[Repeat == 1 & Species == 'Noisy Miner']), sum(Total20[Repeat == 2 & Species == 'Noisy Miner'])))
  ),
  MeanMiner40m = case_when(
    length(unique(Repeat)) == 1  ~ mean(c(sum(Total40[Repeat == 1 & Species == 'Noisy Miner']))),
    length(unique(Repeat)) == 2  ~ mean(c(sum(Total40[Repeat == 1 & Species == 'Noisy Miner']), sum(Total40[Repeat == 2 & Species == 'Noisy Miner'])))
  ),
  NumberOFRepeats = length(unique(Repeat)),
  Mean20m_NMExluded = case_when(
    length(unique(Repeat)) == 1  ~ mean(c(sum(Total20[Repeat == 1 & !Species == 'Noisy Miner']))),
    length(unique(Repeat)) == 2  ~ mean(c(sum(Total20[Repeat == 1 & !Species == 'Noisy Miner']), sum(Total20[Repeat == 2 & !Species == 'Noisy Miner'])))
  ),
  Mean40m_NMExluded = case_when(
    length(unique(Repeat)) == 1  ~ mean(c(sum(Total40[Repeat == 1 & !Species == 'Noisy Miner']))),
    length(unique(Repeat)) == 2  ~ mean(c(sum(Total40[Repeat == 1 & !Species == 'Noisy Miner']), sum(Total40[Repeat == 2 & !Species == 'Noisy Miner'])))
  )) %>% 
  mutate(Threshold20m = ifelse(MeanMiner20m >= 2.4, 1, 0),
         Threshold40m = ifelse(MeanMiner40m >= 7, 1, 0),
         .before = NumberOFRepeats) %>% 
  ungroup()

saveRDS(summaryData, "rawdata/FinalSummaryData.rds")