#Calculate sunrise and sunset times for all sites

# Load packages and site locations ----

library(tidyverse)
library(lubridate)
library(suncalc)

GPS_Site <- read.csv(file = "outputs/GPS_Site.csv")

# Create long data frame of all dates ----

dates <- expand.grid(date = seq(ymd('2019-08-01'),ymd('2022-03-31'), by = '1 day'),
                     Site = unique(GPS_Site$Site))

GPS_Site_AllDays <- full_join(GPS_Site, dates)

# Extract sunrise and sunset times ----

Site_suntimes <- getSunlightTimes(data = GPS_Site_AllDays,
                                  tz = "Australia/Sydney")

Site_suntimes <- full_join(GPS_Site_AllDays, Site_suntimes)

saveRDS(Site_suntimes, "outputs/Site_suntimes.RDS")