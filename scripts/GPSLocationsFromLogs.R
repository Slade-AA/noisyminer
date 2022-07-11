#Extract site locations from GPS logs

# Load packages ----

library(tidyverse)

# Extract GPS coords from log files ----

GPSlogfiles <- list.files(path = "E:/NoisyMiner_Logs",
                          pattern = "^GPS*",
                          recursive = TRUE,
                          full.names = TRUE)

GPSLogs <- list()
for (logfile in GPSlogfiles) {
  
  line3 <- readLines(logfile, 6)[which(startsWith(readLines(logfile, 6), "-"))]
  if (length(line3) == 0) {
    next
  }
  
  if (grepl(",\\+", line3)) {
    pattern = "^[0-9\\.\\-]{8},\\+([0-9\\.]{5,10}),.*"
  } else if (grepl("\\+", line3)) {
    pattern = "^[0-9\\.\\-]{8}\\+([0-9\\.]{5,10}),.*"
  } else if(grepl(",", line3)) {
    pattern = "^[0-9\\.\\-]{8},([0-9\\.]{5,10}),.*"
  }
  
  GPSLogs[[logfile]] <- data.frame(Site = gsub(".*NoisyMiner_Logs/([0-9A-Z]{2,4})/.*", "\\1", logfile),
                                   lat = gsub(",.*|\\+.*", "", line3),
                                   lon = gsub(pattern, "\\1", line3),
                                   Date = strptime(gsub(".*[0-9]{2}:[0-9]{2},(.*)", "\\1", line3), "%d/%m/%Y"))
  
  remove(line3)
  
}

GPSLogs <- do.call(rbind, GPSLogs)

#convert from factor to numeric
GPSLogs$lat <- as.numeric(as.character(GPSLogs$lat))
GPSLogs$lon <- as.numeric(as.character(GPSLogs$lon))

#save
write.csv(GPSLogs, file = "outputs/GPSLogs.csv", row.names = F, quote = F)


# Mean position per site ----
GPS_Site <- GPSLogs %>% group_by(Site) %>% summarise(lat = mean(lat),
                                                     lon = mean(lon))

write.csv(GPS_Site, file = "outputs/GPS_Site.csv", row.names = F, quote = F)


