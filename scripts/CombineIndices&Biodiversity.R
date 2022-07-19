#Combine acoustic indices and biodiversity data

# Load packages ----

library(tidyverse)

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

# Calculate Noisy miner presence-absence ----
acousticIndices_richness <- acousticIndices_richness %>% mutate(NoisyPreAbs = as_factor(case_when(
  NumberNoisyMiner > 0 ~ 1,
  NumberNoisyMiner == 0 ~ 0
)), .after = "NumberNoisyMiner")

# Combine replicates ----

#combine indices and biodiversity numbers wihtin sampling season (e.g. take mean or max of replicates)
acousticIndices_richness_repscombined <- acousticIndices_richness %>% 
  group_by(Site, seasonYear, season, type) %>% 
  summarise(Total20m = mean(Total20m), #have taken mean of counts
            Total40m = mean(Total40m),
            Diversity20m = max(Diversity20m), #have taken max of diversity
            Diversity40m = max(Diversity40m),
            NoisyPreAbs = as_factor(max(as.numeric(as.character(NoisyPreAbs)))),
            NumberNoisyMiner = mean(NumberNoisyMiner), #have taken mean of number noisy miner
            across(.cols = ends_with(c("_mean", "_median", "_iqr", "_sd")), ~ weighted.mean(.x, w = c(n)))) %>% 
  ungroup()


# Save datasets ----

saveRDS(acousticIndices_richness, "outputs/data/acousticIndices_richness.RDS")
saveRDS(acousticIndices_richness_repscombined, "outputs/data/acousticIndices_richness_repscombined.RDS")
