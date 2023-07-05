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


#Add SurveyDate1 and SurveyDate2 to biodiversity_R1R2combined

biodiversity_R1R2combined_SurveyDates <- left_join(x = biodiversity_R1R2combined %>% filter(NumberOFRepeats == 2), 
                  y = SurveyDates %>% select(-c(Replicate, Repeat, SurveyID)), 
                  by = c("SiteID" = "Site", "Season" = "Season2", "Year" = "Year"))

biodiversity_R1R2combined_SurveyDates <- biodiversity_R1R2combined_SurveyDates %>% group_by(SurveyIDR12) %>% mutate(Date1 = min(Date),
                                                  Date2 = max(Date)) %>% select(-Date) %>% distinct_all()

#Filter out survey periods with excessive number of days (i.e., 10? days) between repeats and 0 days between repeats (i.e., surveys both done on same day)
biodiversity_R1R2combined_SurveyDates <- biodiversity_R1R2combined_SurveyDates %>% filter(Date2 - Date1 > 0 & Date2 - Date1 <= 12)



#sites <- c("G1", "G2", "G3")
#sites <- c("RC1")
sites <- gsub(".RDS", "", list.files(path = "E:/_combinedIndices/", pattern = "*.RDS$"))

#Initialise data frames for storing aggregates of acoustic indices for different 'time of day' periods
dawnSummary <- data.frame()
duskSummary <- data.frame()
solarNoonSummary <- data.frame()
daySummary <- data.frame()

for (site in sites) {
  indexSet <- "Indices_Summary" #Do I repeat this for each set of acoustic indices?
  #for (indexSet in c("Indices_Summary", "Indices_SpectralAggregated", "Indices_FeatureReduction")) {}
  
  #Read in acoustic indices for specific site
  #SiteIndices <- readRDS(paste0("C:/Users/jc696551/Downloads/", site, ".RDS"))
  SiteIndices <- readRDS(paste0("E:/_combinedIndices/", site, ".RDS"))
  
  
  indicesToSummarise <- colnames(SiteIndices[[indexSet]])[-c(1:3)] #create vector of acoustic index names that will be summarised below
  
  #Join suntimes to acoustic indices
  SiteIndices[[indexSet]] <- left_join(SiteIndices[[indexSet]], suntimes)
  
  
  surveys_Site <- biodiversity_R1R2combined_SurveyDates[biodiversity_R1R2combined_SurveyDates$SiteID == site,]
  
  for (survey in 1:nrow(surveys_Site)) {

    #list dates in acoustic index dataset which fall within 2 days either side of either survey day
    audioDaysWithin2days <- unique(SiteIndices[[indexSet]]$Date)[which(unique(SiteIndices[[indexSet]]$Date) >= (surveys_Site$Date1[survey]-2) & unique(SiteIndices[[indexSet]]$Date) <= (surveys_Site$Date2[survey]+2))]
    
    #Determine the number of days within 2 that have dawn recordings (at least 75% of minutes)
    audioDaysWithin2days_dawn <- audioDaysWithin2days[which(sapply(audioDaysWithin2days, function(x) nrow(SiteIndices[[indexSet]][SiteIndices[[indexSet]]$Date == x & SiteIndices[[indexSet]]$Time >= (SiteIndices[[indexSet]]$dawn-hms("00:30:00")) & SiteIndices[[indexSet]]$Time < (SiteIndices[[indexSet]]$dawn+hms("01:30:00")),])) >= (120*0.75))]
    
    #Determine the number of days within 2 that have dusk recordings (at least 75% of minutes)
    audioDaysWithin2days_dusk <- audioDaysWithin2days[which(sapply(audioDaysWithin2days, function(x) nrow(SiteIndices[[indexSet]][SiteIndices[[indexSet]]$Date == x & SiteIndices[[indexSet]]$Time >= (SiteIndices[[indexSet]]$dusk-hms("01:30:00")) & SiteIndices[[indexSet]]$Time < (SiteIndices[[indexSet]]$dusk+hms("00:30:00")),])) >= (120*0.75))]
    
    #Determine the number of days within 2 that have solarNoon recordings (at least 75% of minutes)
    audioDaysWithin2days_solarNoon <- audioDaysWithin2days[which(sapply(audioDaysWithin2days, function(x) nrow(SiteIndices[[indexSet]][SiteIndices[[indexSet]]$Date == x & SiteIndices[[indexSet]]$Time >= (SiteIndices[[indexSet]]$solarNoon-hms("01:00:00")) & SiteIndices[[indexSet]]$Time < (SiteIndices[[indexSet]]$solarNoon+hms("01:00:00")),])) >= (120*0.75))]
    
    #Determine the number of days within 2 that have day (sunrise to sunset) recordings (at least 75% of minutes)
    audioDaysWithin2days_day <- audioDaysWithin2days[which(sapply(audioDaysWithin2days, function(x) nrow(SiteIndices[[indexSet]][SiteIndices[[indexSet]]$Date == x & SiteIndices[[indexSet]]$Time >= (SiteIndices[[indexSet]]$sunrise) & SiteIndices[[indexSet]]$Time < (SiteIndices[[indexSet]]$sunset),])) >= sapply(audioDaysWithin2days, function(x) ((period_to_seconds(hm(gsub("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2} ([0-9\\:]{5})\\:[0-9]{2}$","\\1",SiteIndices[[indexSet]]$sunset[SiteIndices[[indexSet]]$Date == x][1])) - hm(gsub("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2} ([0-9\\:]{5})\\:[0-9]{2}$","\\1",SiteIndices[[indexSet]]$sunrise[SiteIndices[[indexSet]]$Date == x][1])))/60)*0.75)))]
    
    
    
    #Dawn Summary
    if (length(audioDaysWithin2days_dawn) > 0) {
      for (i in 1:length(audioDaysWithin2days_dawn)) {
        #randomly samples i number of days with audio recordings
        daysToUse <- sample(audioDaysWithin2days_dawn, i)
        
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
    if (length(audioDaysWithin2days_dusk) > 0) {
      for (i in 1:length(audioDaysWithin2days_dusk)) {
        #randomly samples i number of days with audio recordings
        daysToUse <- sample(audioDaysWithin2days_dusk, i)
        
        #Loop through each day and select the minutes of audio that will be summarised
        tmpIndices_dusk <- data.frame()
        for (dayToUse in daysToUse) {
          tmpIndices_dusk <- bind_rows(tmpIndices_dusk, 
                                       SiteIndices[[indexSet]] %>% 
                                         filter(Date == dayToUse) %>% 
                                         filter(Time >= (dusk-hms("01:30:00")) & Time < (dusk+hms("00:30:00"))))
        }
        
        #Summarise the minutes of audio selected in above loop
        duskSummary <- bind_rows(duskSummary,
                                 tmpIndices_dusk %>% 
                                   mutate(n = n(), p = n()/(120*i), audioDays = i, SurveyIDR12 = surveys_Site$SurveyIDR12[survey], type = "dusk") %>% 
                                   group_by(Site, SurveyIDR12, type, n, p, audioDays) %>% 
                                   summarise_at(vars(all_of(indicesToSummarise)), 
                                                list(mean = mean, median = median, iqr = IQR, sd = sd)) %>% 
                                   ungroup())
        
      }
    }
    
    
    #solarNoon Summary
    if (length(audioDaysWithin2days_solarNoon) > 0) {
      for (i in 1:length(audioDaysWithin2days_solarNoon)) {
        #randomly samples i number of days with audio recordings
        daysToUse <- sample(audioDaysWithin2days_solarNoon, i)
        
        #Loop through each day and select the minutes of audio that will be summarised
        tmpIndices_solarNoon <- data.frame()
        for (dayToUse in daysToUse) {
          tmpIndices_solarNoon <- bind_rows(tmpIndices_solarNoon, 
                                       SiteIndices[[indexSet]] %>% 
                                         filter(Date == dayToUse) %>% 
                                         filter(Time >= (solarNoon-hms("01:00:00")) & Time < (solarNoon+hms("01:00:00"))))
        }
        
        #Summarise the minutes of audio selected in above loop
        solarNoonSummary <- bind_rows(solarNoonSummary,
                                 tmpIndices_solarNoon %>% 
                                   mutate(n = n(), p = n()/(120*i), audioDays = i, SurveyIDR12 = surveys_Site$SurveyIDR12[survey], type = "solarNoon") %>% 
                                   group_by(Site, SurveyIDR12, type, n, p, audioDays) %>% 
                                   summarise_at(vars(all_of(indicesToSummarise)), 
                                                list(mean = mean, median = median, iqr = IQR, sd = sd)) %>% 
                                   ungroup())
        
      }
    }
    
    
    #Day Summary
    if (length(audioDaysWithin2days_day) > 0) {
      for (i in 1:length(audioDaysWithin2days_day)) {
        #randomly samples i number of days with audio recordings
        daysToUse <- sample(audioDaysWithin2days_day, i)
        
        #Loop through each day and select the minutes of audio that will be summarised
        tmpIndices_day <- data.frame()
        dayMinutes <- c() #Vector for storing the number of minutes between sunrise and sunset
        for (dayToUse in daysToUse) {
          tmpIndices_day <- bind_rows(tmpIndices_day, 
                                            SiteIndices[[indexSet]] %>% 
                                              filter(Date == dayToUse) %>% 
                                              filter(Time >= sunrise & Time < sunset))
          
          dayMinutes <- c(dayMinutes, (period_to_seconds(hm(gsub("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2} ([0-9\\:]{5})\\:[0-9]{2}$","\\1",SiteIndices[[indexSet]]$sunset[SiteIndices[[indexSet]]$Date == dayToUse][1])) - hm(gsub("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2} ([0-9\\:]{5})\\:[0-9]{2}$","\\1",SiteIndices[[indexSet]]$sunrise[SiteIndices[[indexSet]]$Date == dayToUse][1])))/60))
        }
        dayMinutes <- sum(dayMinutes)
        
        #Summarise the minutes of audio selected in above loop
        daySummary <- bind_rows(daySummary,
                                      tmpIndices_day %>% 
                                        mutate(n = n(), p = n()/dayMinutes, audioDays = i, SurveyIDR12 = surveys_Site$SurveyIDR12[survey], type = "day") %>% 
                                        group_by(Site, SurveyIDR12, type, n, p, audioDays) %>% 
                                        summarise_at(vars(all_of(indicesToSummarise)), 
                                                     list(mean = mean, median = median, iqr = IQR, sd = sd)) %>% 
                                        ungroup())
        
      }
    }
    
    
  }
}


#Join everything together into 1 data frame?
allSummary <- bind_rows(dawnSummary,
                        duskSummary,
                        solarNoonSummary,
                        daySummary)

#Add biodiversity information to indices - join occurs on 'SurveyIDR12'
allSummary <- left_join(allSummary,
                        biodiversity_R1R2combined_SurveyDates %>% select(SurveyIDR12, Mean20m:Threshold40m))

saveRDS(allSummary, paste0("outputs/allSummary_", Sys.Date()))