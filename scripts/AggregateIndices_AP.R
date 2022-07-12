#Extract summary indices from QUT outputs

# Load packages ----

library(tidyverse)
library(lubridate)


# Read in indices, add columns for site, sensor etc. ----

summaryIndicesFiles <- list.files(path = "E:/NoisyMiner_Indices_AP",
                                  pattern = ".*Towsey.Acoustic.Indices.csv$",
                                  recursive = TRUE,
                                  full.names = TRUE)

tictoc::tic()
summaryIndices <- list()
for (file in summaryIndicesFiles) {
  tmp <- read.csv(file = file)
  
  tmp$Site <- gsub(".*NoisyMiner_Indices_AP/([0-9A-Z]{2,4})/.*", "\\1", file)
  
  #extract date and time from filename and column in csv - needs to accomadate different naming systems
  if (grepl("T[0-9]{6}\\+", basename(file))) {
    tmp$DATETIME <- strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(file)), gsub(".*T([0-9]{6})\\+.*", "\\1", basename(file))), "%Y%m%d%H%M%S") + tmp$ResultStartSeconds
  } else if (grepl(".*[0-9]{8}_[0-9]{6}_", basename(file))) {
    tmp$DATETIME <- strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(file)), gsub(".*_([0-9]{6})_.*", "\\1", basename(file))), "%Y%m%d%H%M%S") + tmp$ResultStartSeconds
  } else {
    print(paste0("Unrecognised file name format - Skipping file: ", file))
    next
  }
  
  #extract hour and 1 min interval from datetime
  tmp$TIME_NEW <- strftime(tmp$DATETIME, format = "%H:%M:%S")
  
  
  summaryIndices[[paste0(tmp$Site[1], "_", tmp$DATETIME[1])]] <- tmp
}
tictoc::toc()
#~7 minutes

summaryIndices <- do.call(rbind, summaryIndices)

summaryIndices <- summaryIndices %>% mutate(Date = as.character(as.POSIXct(gsub("^([0-9\\-]{10}).*", "\\1", DATETIME), format = "%Y-%m-%d")))


# Save output ----
saveRDS(object = summaryIndices, file = "outputs/indices/summaryIndices_AP.RDS")
