
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


#Days between replicate 1 and 2 each sampling season

daysBetween <- richness_abundance %>% 
  arrange(replicate) %>% 
  filter(!replicate %in% c(3,4)) %>% 
  group_by(Site, seasonYear) %>% 
  summarise(n_days = difftime(Date[2], Date[1], units = "days")-1)

#Summary
daysBetween %>% group_by(n_days) %>% summarise(n = n())