#Create data frame with dates and season identifiers from first set of data Paul sent through

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
richness_abundance <- richness_abundance %>% mutate(Season = gsub("^[A-Z]{1,2}[0-9]{1,2}([A-Za-z]{6})[0-9]{5}", "\\1", SurveyID),
                                                    SeasonYear = gsub("^[A-Z]{1,2}[0-9]{1,2}([A-Za-z]{6}[0-9]{4})[0-9]{1}", "\\1", SurveyID),
                                                    Year = gsub("^[A-Z]{1,2}[0-9]{1,2}[A-Za-z]{6}([0-9]{4})[0-9]{1}", "\\1", SurveyID),
                                                    Replicate = gsub("^[A-Z]{1,2}[0-9]{1,2}[A-Za-z]{6}[0-9]{4}", "", SurveyID))

#Create a 'season2' column that has early and late spring categories to match latest data
richness_abundance <- richness_abundance %>% 
  group_by(Site, SeasonYear) %>% 
  mutate(Season2 = case_when(
    n() == 4 & Replicate == '1' ~ paste0(Season, "E"),
    n() == 4 & Replicate == '2' ~ paste0(Season, "E"),
    n() == 4 & Replicate == '3' ~ paste0(Season, "L"),
    n() == 4 & Replicate == '4' ~ paste0(Season, "L"),
    n() != 4 & Season == 'Spring' ~ paste0(Season, "L"),
    n() != 4 ~ paste0(Season)
  ))


SurveyDates <- richness_abundance %>% select(SurveyID, Site, Date, Repeat, Season, SeasonYear, Year, Replicate, Season2)

saveRDS(SurveyDates, "outputs/SurveyDates.RDS")