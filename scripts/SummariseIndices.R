#Calculate mean and median values for each acoustic index for...

# Load packages ----

library(tidyverse)
library(lubridate)

# Read in aggregated indices ----

summaryIndices_R <- readRDS("outputs/indices/summaryIndices_R.RDS") #Haven't been generated for all recordings yet
summaryIndices_AP <- readRDS("outputs/indices/summaryIndices_AP.RDS")
spectralIndices_AP <- readRDS("outputs/indices/spectralIndices_AP_CVR_ENT.RDS")

# Fix timezones of datetime columns ----
#use 'force_tz' to change the timezone without changing the actual time - the actual clock time should be correct

summaryIndices_R <- summaryIndices_R %>% mutate(DATETIME = force_tz(Time, tz = "Australia/Sydney"), .after = "Date") %>% select(-Time)
summaryIndices_AP <- summaryIndices_AP %>% mutate(DATETIME = force_tz(DATETIME, tz = "Australia/Sydney"))
spectralIndices_AP <- spectralIndices_AP %>% mutate(DATETIME = force_tz(DATETIME, tz = "Australia/Sydney"))

# Specify acoustic indices ----

RIndices <- c('ACI_soundecology',
              'ACI_chur',
              'ACI_notchur',
              'ADI',
              'AE',
              'NDSI_soundecology',
              'NDSI_bio',
              'NDSI_anthro',
              'M',
              'H',
              'Ht',
              'Hf',
              'BI',
              'BI_chur')

APIndices <- c('Activity',
               'EventsPerSecond', 
               'SpectralCentroid', 
               'HighFreqCover', 
               'MidFreqCover', 
               'LowFreqCover', 
               'AcousticComplexity', 
               'TemporalEntropy', 
               'EntropyOfAverageSpectrum',
               'EntropyOfVarianceSpectrum',
               'EntropyOfPeaksSpectrum',
               'EntropyOfCoVSpectrum',
               'ClusterCount',
               'ThreeGramCount',
               'Ndsi',
               'SptDensity')

APSpectralIndices <- colnames(spectralIndices_AP[5:(ncol(spectralIndices_AP)-1)])

# Combine indices ----

combinedIndices_AP <- full_join(summaryIndices_AP, spectralIndices_AP)

#include R indices
combinedIndices <- summaryIndices_R %>% 
  mutate(Date = as.character(Date)) %>% 
  left_join(combinedIndices_AP)

# Load suntimes and join with indices ----

suntimes <- readRDS("outputs/Site_suntimes.RDS")
suntimes <- suntimes %>% rename(Date = date) %>% mutate(Date = as.character(Date))

combinedIndices <- left_join(combinedIndices, suntimes)

saveRDS(combinedIndices, "outputs/data/combinedIndices.RDS")

# Summarise indices by time periods ----

# ├ Summarise dawn (30 mins before dawn to 90 mins post dawn) ----
acousticIndices_dawn <- combinedIndices %>% 
  drop_na() %>% 
  filter(DATETIME >= (dawn-hms("00:30:00")) & DATETIME < (dawn+hms("01:30:00"))) %>% 
  group_by(Site, Date) %>% 
  mutate(n = n(), p = n()/120) %>%
  group_by(Site, Date, n, p) %>% 
  summarise_at(vars(all_of(APIndices), all_of(APSpectralIndices), all_of(RIndices)), 
               list(mean = mean, median = median, iqr = IQR, sd = sd))

# ├ Summarise dusk (90 mins before dusk to 30 mins post dusk) ----
acousticIndices_dusk <- combinedIndices %>% 
  drop_na() %>% 
  filter(DATETIME >= (dusk-hms("01:30:00")) & DATETIME < (dusk+hms("00:30:00"))) %>%
  group_by(Site, Date) %>% 
  mutate(n = n(), p = n()/120) %>% 
  group_by(Site, Date, n, p) %>% 
  summarise_at(vars(all_of(APIndices), all_of(APSpectralIndices), all_of(RIndices)), 
               list(mean = mean, median = median, iqr = IQR, sd = sd))

# ├ Summarise solarNoon (solarNoon +- 1 hour) ----
acousticIndices_solarNoon <- combinedIndices %>% 
  drop_na() %>% 
  filter(DATETIME >= (solarNoon-hms("01:00:00")) & DATETIME < (solarNoon+hms("01:00:00"))) %>% 
  group_by(Site, Date) %>% 
  mutate(n = n(), p = n()/120) %>% 
  group_by(Site, Date, n, p) %>% 
  summarise_at(vars(all_of(APIndices), all_of(APSpectralIndices), all_of(RIndices)), 
               list(mean = mean, median = median, iqr = IQR, sd = sd))

# ├ Summarise day (sunrise to sunset) ----
acousticIndices_day <- combinedIndices %>% 
  drop_na() %>% 
  filter(DATETIME >= sunrise & DATETIME < sunset) %>% 
  group_by(Site, Date) %>% 
  mutate(n = n(), p = n()/(period_to_seconds(hm(gsub("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2} ([0-9\\:]{5})\\:[0-9]{2}$","\\1",sunset)) - hm(gsub("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2} ([0-9\\:]{5})\\:[0-9]{2}$","\\1",sunrise)))/60)) %>% 
  group_by(Site, Date, n, p) %>% 
  summarise_at(vars(all_of(APIndices), all_of(APSpectralIndices), all_of(RIndices)), 
               list(mean = mean, median = median, iqr = IQR, sd = sd))

# Bind in single data frame and save ----
acousticIndices_summary <- bind_rows(list(dawn = acousticIndices_dawn,
                                          dusk = acousticIndices_dusk,
                                          solarNoon = acousticIndices_solarNoon,
                                          day = acousticIndices_day),
                                     .id = "type") %>% ungroup()

save(acousticIndices_summary, file = paste0("./outputs/data/", Sys.Date(), "_acousticIndices_summary.RData"))