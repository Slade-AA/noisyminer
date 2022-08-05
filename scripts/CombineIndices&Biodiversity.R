#Combine acoustic indices and biodiversity data

# Load packages ----

library(tidyverse)

# Load indices and biodiversity data ----

# ├ biodiversity data ----

# ├├ Repeats combined ----

biodiversity_R1R2combined <- read.csv("rawdata/FinalMeanDataAllRepeats.csv")

#Extract Season and Year from SurveyIDR12 column
biodiversity_R1R2combined <- biodiversity_R1R2combined %>% mutate(Season = gsub("^[A-Z0-9]{2,4}_([A-Za-z]*)_[0-9]{4}$", "\\1", SurveyIDR12),
                                                                  Year = gsub(".*_([0-9]{4})$", "\\1", SurveyIDR12),
                                                                  .after = SiteID)

# ├├ First repeat only ----

biodiversity_R1only <- read.csv("rawdata/FinalDataR1Only.csv")

#Extract Season and Year from SurveyIDR12 column
biodiversity_R1only <- biodiversity_R1only %>% mutate(Season = gsub("^[A-Z0-9]{2,4}_([A-Za-z]*)_[0-9]{4}$", "\\1", SurveyIDR12),
                                                      Year = gsub(".*_([0-9]{4})$", "\\1", SurveyIDR12),
                                                      .after = SiteID)

#Provided threshold columns appear to be incorrect - will recalculate here
biodiversity_R1only <- biodiversity_R1only %>% 
  mutate(Threshold20m = if_else(TotalMiner20 >= 2.4, 1, 0),
         Threshold40m = if_else(TotalMiner40 >= 7, 1, 0))

# ├ acoustic indices ----

files <- file.info(list.files("./outputs/data/", pattern = ".*_acousticIndices_summary.RData$", full.names = TRUE)) #list files
latestFile <- rownames(files)[which.max(files$mtime)] #determine most recent file to use for loading

load(latestFile)

# ├├ Survey Dates data ----
#Need to join this to acoustic indices in order to join with biodiversity data

SurveyDates <- readRDS("outputs/SurveyDates.RDS")

test <- right_join(SurveyDates, acousticIndices_summary)
#BN5 (2021-02-15), BN6 (2021-02-15), and Y2 (2021-07-28) have both replicates of one season conducted on the same day
#Current solution is to drop these for R1 and R2 combined analysis
test <- test %>% filter(!(Site == 'BN5' & Date == '2021-02-15'),
                        !(Site == 'BN6' & Date == '2021-02-15'),
                        !(Site == 'Y2' & Date == '2021-07-28'))

# Merge indices and biodiversity data ----

# ├ R1R2Combined ----
#Need to average acoutic indices for both survey days (i.e., repeats 1 and 2)
#Removing any time periods where less than 70% of the acoustic data is available - this will result in a couple more surveys being dropped

#NOTE:
#BN5 (2021-02-15), BN6 (2021-02-15), and Y2 (2021-07-28) have both replicates of one season conducted on the same day
#Current solution is to drop these for R1 and R2 combined analysis
acousticIndices_biodiversity_R1R2combined <- right_join(SurveyDates, acousticIndices_summary) %>% 
  filter(!(Site == 'BN5' & Date == '2021-02-15'), #removing surveys where both were conducted on same day - only 1 day of audio
         !(Site == 'BN6' & Date == '2021-02-15'),
         !(Site == 'Y2' & Date == '2021-07-28')) %>% 
  filter(p >= 0.7) %>% #remove datapoints where less than 70% of audio was available
  group_by(Site, Season, SeasonYear, Year, Season2, type) %>% 
  filter(n() == 2) %>% #A number of sites have survey periods with audio only available for 1 of the two replicates - these will be removed
  summarise(across(.cols = ends_with(c("_mean", "_median", "_iqr", "_sd")), ~ weighted.mean(.x, w = c(n)))) %>% #average acoustic indices using a weighted mean of the number of minutes
  left_join(biodiversity_R1R2combined, by = c("Site" = "SiteID", "Season2" = "Season", "Year" = "Year"))


 # ├ R1Only ----
  
acousticIndices_biodiversity_R1Only <- right_join(SurveyDates, acousticIndices_summary) %>% 
  filter(p >= 0.7 & Repeat == 1) %>%
  left_join(biodiversity_R1R2combined, by = c("Site" = "SiteID", "Season2" = "Season", "Year" = "Year"))
  

# Combine & Save ----

acousticIndices_biodiversity <- list(R1Only = acousticIndices_biodiversity_R1Only,
                                     R1R2Combined = acousticIndices_biodiversity_R1R2combined)

saveRDS(acousticIndices_biodiversity, "outputs/data/acousticIndices_biodiversity.RDS")