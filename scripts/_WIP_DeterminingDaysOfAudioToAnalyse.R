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


# ├├ Survey Dates data ----
#Need to join this to acoustic indices in order to join with biodiversity data

SurveyDates <- readRDS("outputs/SurveyDates.RDS") %>% mutate(Date = as.Date(Date))


# Load suntimes ----

suntimes <- readRDS("outputs/Site_suntimes.RDS")
suntimes <- suntimes %>% rename(Date = date)


#test <- right_join(SurveyDates, acousticIndices_summary)


#Add SurveyDate1 and SurveyDate2 to biodiversity_R1R2combined

biodiversity_R1R2combined_SurveyDates <- left_join(x = biodiversity_R1R2combined %>% filter(NumberOFRepeats == 2), 
                  y = SurveyDates %>% select(-c(Replicate, Repeat, SurveyID)), 
                  by = c("SiteID" = "Site", "Season" = "Season2", "Year" = "Year"))

biodiversity_R1R2combined_SurveyDates <- biodiversity_R1R2combined_SurveyDates %>% group_by(SurveyIDR12) %>% mutate(Date1 = min(Date),
                                                  Date2 = max(Date)) %>% select(-Date) %>% distinct_all()

# Number of days between on-ground surveys (remove particularly large ones (e.g. 66 days!)? as well as zeros (3 occassions))
biodiversity_R1R2combined_SurveyDates$Date2 - biodiversity_R1R2combined_SurveyDates$Date1

sites <- c("G1", "G2", "G3")
dawnSummary <- data.frame()
for (site in sites) {
  indexSet <- "Indices_Summary"
  
  #Read in acoustic indices for specific site
  SiteIndices <- readRDS(paste0("C:/Users/jc696551/Downloads/", site, ".RDS"))
  
  
  
  indicesToSummarise <- colnames(SiteIndices[[indexSet]])[-c(1:3)] #create vector of acoustic index names that will be summarised below
  
  #Join suntimes to acoustic indices
  SiteIndices[[indexSet]] <- left_join(SiteIndices[[indexSet]], suntimes)
  
  
  surveys_Site <- biodiversity_R1R2combined_SurveyDates[biodiversity_R1R2combined_SurveyDates$SiteID == site,]
  
  for (survey in 1:nrow(surveys_Site)) {
    #which(SiteIndices[[indexSet]]$Date >= surveys_Site$Date1[survey]-2 & SiteIndices[[indexSet]]$Date >= surveys_Site$Date2[survey]+2)
    
    #list dates in acoustic index dataset which fall within 2 days either side of either survey day
    audioDaysWithin2days <- unique(SiteIndices[[indexSet]]$Date)[which(unique(SiteIndices[[indexSet]]$Date) >= (surveys_Site$Date1[survey]-2) & unique(SiteIndices[[indexSet]]$Date) <= (surveys_Site$Date2[survey]+2))]
    
    #Determine the number of days within 2 that have dawn recordings (at least 75% of minutes)
    audioDaysWithin2days_dawn <- audioDaysWithin2days[which(sapply(audioDaysWithin2days, function(x) nrow(SiteIndices[[indexSet]][SiteIndices[[indexSet]]$Date == x & SiteIndices[[indexSet]]$Time >= (SiteIndices[[indexSet]]$dawn-hms("00:30:00")) & SiteIndices[[indexSet]]$Time < (SiteIndices[[indexSet]]$dawn+hms("01:30:00")),])) >= (120*0.75))]
    
    #audioDaysWithin2days_dusk....
    
    #Dawn Summary
    if (length(audioDaysWithin2days_dawn) > 0) {
      for (i in 1:length(audioDaysWithin2days_dawn)) {
        #randomly samples i number of days with audio recordings
        daysToUse <- sample(audioDaysWithin2days_dawn, i) #Should I check whether the required audio is present for these days first? (e.g. do we have dawn recordings for all days? etc.)
        
        #Loop through each day and select the minutes of audio that will be summarised
        tmpIndices_dawn <- data.frame()
        for (dayToUse in daysToUse) {
          tmpIndices_dawn <- bind_rows(tmpIndices_dawn, 
                                       SiteIndices[[indexSet]] %>% 
                                         filter(Date == dayToUse) %>% 
                                         filter(Time >= (dawn-hms("00:30:00")) & Time < (dawn+hms("01:30:00"))))
        }
        
        #Summarise the minutes of audio selected in above loop
        dawnSummary <- bind_rows(dawnSummary,
                                 tmpIndices_dawn %>% 
                                   mutate(n = n(), p = n()/(120*i), audioDays = i, SurveyIDR12 = surveys_Site$SurveyIDR12[survey], type = "dawn") %>% 
                                   group_by(Site, SurveyIDR12, type, n, p, audioDays) %>% 
                                   summarise_at(vars(all_of(indicesToSummarise)), 
                                                list(mean = mean, median = median, iqr = IQR, sd = sd)) %>% 
                                   ungroup())
        
      }
    }
    
    
    #Dusk Summary
    
  }
}

