library(tidyverse)

rawData <- read.csv("rawdata/2019-2023MasterData26May2023.csv")

SurveyDatesAll <- rawData %>% 
  select(Site = SiteID, Date, Repeat, Season = Survey, Year) %>% 
  group_by(Site, Date, Repeat, Season, Year) %>% 
  summarise(across(everything())) %>% 
  mutate(SurveyID = paste0(Site, Season, Year, Repeat),
         .before = Site) %>% 
  mutate(Date = as.POSIXct(Date, format = "%d/%m/%Y", tz = "Australia/Sydney")) %>% 
  mutate(Year = as.character(Year)) %>% 
  ungroup()

saveRDS(SurveyDatesAll, "outputs/SurveyDatesAll.RDS")