#Calculate mean and median values for each acoustic index for...

# Load packages ----

library(tidyverse)
#library(lubridate)

# Load suntimes and join with indices ----

suntimes <- readRDS("outputs/Site_suntimes.RDS")
suntimes <- suntimes %>% rename(Date = date)

#List individual site files
siteFiles <- list.files(path = "E:/_combinedIndices/", pattern = ".RDS$", full.names = T)

siteFiles <- "C:/Users/jc696551/Downloads/G1.RDS"

acousticIndices_dawn <- list()
acousticIndices_dusk <- list()
acousticIndices_solarNoon <- list()
acousticIndices_day <- list()

for (siteFile in siteFiles) {
  
  SiteIndices <- readRDS(siteFile)
  
  for (indexSet in c("Indices_Summary", "Indices_SpectralAggregated", "Indices_FeatureReduction")) {
    
    indicesToSummarise <- colnames(SiteIndices[[indexSet]])[-c(1:3)] #create vector of acoustic index names that will be summarised below
    
    #Join suntimes to acoustic indices
    SiteIndices[[indexSet]] <- left_join(SiteIndices[[indexSet]], suntimes)
    
    
    # Summarise indices by time periods ----
    
    # ├ Summarise dawn (30 mins before dawn to 90 mins post dawn) ----
    acousticIndices_dawn[[indexSet]] <- bind_rows(acousticIndices_dawn[[indexSet]], SiteIndices[[indexSet]] %>% 
                                                    #drop_na() %>% 
                                                    filter(Time >= (dawn-hms("00:30:00")) & Time < (dawn+hms("01:30:00"))) %>% 
                                                    group_by(Site, Date) %>% 
                                                    mutate(n = n(), p = n()/120) %>%
                                                    group_by(Site, Date, n, p) %>% 
                                                    summarise_at(vars(all_of(indicesToSummarise)), 
                                                                 list(mean = mean, median = median, iqr = IQR, sd = sd)) %>% 
                                                    ungroup())
    
    # ├ Summarise dusk (90 mins before dusk to 30 mins post dusk) ----
    acousticIndices_dusk[[indexSet]] <- bind_rows(acousticIndices_dusk[[indexSet]], SiteIndices[[indexSet]] %>% 
                                                    #drop_na() %>% 
                                                    filter(Time >= (dusk-hms("01:30:00")) & Time < (dusk+hms("00:30:00"))) %>%
                                                    group_by(Site, Date) %>% 
                                                    mutate(n = n(), p = n()/120) %>% 
                                                    group_by(Site, Date, n, p) %>% 
                                                    summarise_at(vars(all_of(indicesToSummarise)), 
                                                                 list(mean = mean, median = median, iqr = IQR, sd = sd)) %>% 
                                                    ungroup())
    
    # ├ Summarise solarNoon (solarNoon +- 1 hour) ----
    acousticIndices_solarNoon[[indexSet]] <- bind_rows(acousticIndices_solarNoon[[indexSet]], SiteIndices[[indexSet]] %>% 
                                                         #drop_na() %>% 
                                                         filter(Time >= (solarNoon-hms("01:00:00")) & Time < (solarNoon+hms("01:00:00"))) %>% 
                                                         group_by(Site, Date) %>% 
                                                         mutate(n = n(), p = n()/120) %>% 
                                                         group_by(Site, Date, n, p) %>% 
                                                         summarise_at(vars(all_of(indicesToSummarise)), 
                                                                      list(mean = mean, median = median, iqr = IQR, sd = sd)) %>% 
                                                         ungroup())
    
    # ├ Summarise day (sunrise to sunset) ----
    acousticIndices_day[[indexSet]] <- bind_rows(acousticIndices_day[[indexSet]], SiteIndices[[indexSet]] %>% 
                                                   #drop_na() %>% 
                                                   filter(Time >= sunrise & Time < sunset) %>% 
                                                   group_by(Site, Date) %>% 
                                                   mutate(n = n(), p = n()/(period_to_seconds(hm(gsub("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2} ([0-9\\:]{5})\\:[0-9]{2}$","\\1",sunset)) - hm(gsub("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2} ([0-9\\:]{5})\\:[0-9]{2}$","\\1",sunrise)))/60)) %>% 
                                                   group_by(Site, Date, n, p) %>% 
                                                   summarise_at(vars(all_of(indicesToSummarise)), 
                                                                list(mean = mean, median = median, iqr = IQR, sd = sd)) %>% 
                                                   ungroup())
  }
}



# Bind in single data frame and save ----
acousticIndices_summary <- bind_rows(list(dawn = acousticIndices_dawn[["Indices_Summary"]],
                                          dusk = acousticIndices_dusk[["Indices_Summary"]],
                                          solarNoon = acousticIndices_solarNoon[["Indices_Summary"]],
                                          day = acousticIndices_day[["Indices_Summary"]]),
                                     .id = "type") %>% ungroup

acousticIndices_spectral <- bind_rows(list(dawn = acousticIndices_dawn[["Indices_SpectralAggregated"]],
                                           dusk = acousticIndices_dusk[["Indices_SpectralAggregated"]],
                                           solarNoon = acousticIndices_solarNoon[["Indices_SpectralAggregated"]],
                                           day = acousticIndices_day[["Indices_SpectralAggregated"]]),
                                      .id = "type") %>% ungroup

acousticIndices_featurereduction <- bind_rows(list(dawn = acousticIndices_dawn[["Indices_FeatureReduction"]],
                                                   dusk = acousticIndices_dusk[["Indices_FeatureReduction"]],
                                                   solarNoon = acousticIndices_solarNoon[["Indices_FeatureReduction"]],
                                                   day = acousticIndices_day[["Indices_FeatureReduction"]]),
                                              .id = "type") %>% ungroup


saveRDS(acousticIndices_summary, "outputs/indices_2023/acousticIndices_summary.RDS")
saveRDS(acousticIndices_spectral, "outputs/indices_2023/acousticIndices_spectral.RDS")
saveRDS(acousticIndices_featurereduction, "outputs/indices_2023/acousticIndices_featurereduction.RDS")