#Calculate mean and median values for each acoustic index for...

# Load packages ----

library(tidyverse)
library(lubridate)

# Read in aggregated indices ----

summaryIndices <- readRDS("outputs/summaryIndices.RDS")

# Summarise acoustic indices ----

indicesToUse <- c('ACI_soundecology',
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

# ├ Summarise morning (6am-9am) ----
acousticIndices_morning <- summaryIndices %>% 
  drop_na() %>% 
  filter(hms(TIME_NEW) >= hms("06:00:00") & hms(TIME_NEW) < hms("09:00:00")) %>% 
  group_by(Site, Date) %>% 
  mutate(n = n(), p = n()/180) %>% 
  group_by(Site, Date, n, p) %>% 
  summarise_at(vars(all_of(indicesToUse)), 
               list(mean = mean, median = median, iqr = IQR, sd = sd))

# ├ Summarise afternoon (3pm-6pm) ----
acousticIndices_afternoon <- summaryIndices %>% 
  drop_na() %>% 
  filter(hms(TIME_NEW) >= hms("15:00:00") & hms(TIME_NEW) < hms("18:00:00")) %>%
  group_by(Site, Date) %>% 
  mutate(n = n(), p = n()/180) %>% 
  group_by(Site, Date, n, p) %>% 
  summarise_at(vars(all_of(indicesToUse)), 
               list(mean = mean, median = median, iqr = IQR, sd = sd))

# ├ Summarise day (6am-6pm) ----
acousticIndices_day <- summaryIndices %>% 
  drop_na() %>% 
  filter(hms(TIME_NEW) >= hms("06:00:00") & hms(TIME_NEW) < hms("18:00:00")) %>% 
  group_by(Site, Date) %>% 
  mutate(n = n(), p = n()/720) %>% 
  group_by(Site, Date, n, p) %>% 
  summarise_at(vars(all_of(indicesToUse)), 
               list(mean = mean, median = median, iqr = IQR, sd = sd))

# Bind in single data frame and save ----
acousticIndices_summary <- bind_rows(list(morning = acousticIndices_morning,
                                          afternoon = acousticIndices_afternoon,
                                          day = acousticIndices_day),
                                     .id = "type") %>% ungroup()

save(acousticIndices_summary, file = paste0("./outputs/data/acousticIndices_summary.RData"))