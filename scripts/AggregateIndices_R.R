#Script to join R outputs from 'CalculateIndices_R.R'

# Load packages ----

library(tidyverse)
library(lubridate)

# Read in indices ----

summaryIndicesFiles <- list.files(path = "E:/NoisyMiner_Indices",
                                  pattern = "*.RDS",
                                  recursive = TRUE,
                                  full.names = TRUE)

summaryIndices <- list()
for (file in summaryIndicesFiles) {
  tmp <- readRDS(file)
  
  #extract hour and 1 min interval from Time into a new column
  tmp <- add_column(tmp, TIME_NEW = strftime(tmp$Time, format = "%H:%M:%S"), .after = "Time")
  
  #Fix incorrect site names for previously run code
  #First batch of generated indices assigned incorrect site names to folders with a length 2 identifier (e.g., Y1, G2 etc.)
  if (nchar(tmp$Site[1]) > 4) {
    tmp$Site <- gsub(".*NoisyMiner_Recordings/([0-9A-Z]{2,4})/.*", "\\1", tmp$Site)
  }
  
  summaryIndices[[paste0(tmp$Site[1], "_", tmp$Time[1])]] <- tmp
  
  remove(tmp)
}

summaryIndices <- do.call(rbind, summaryIndices)

#check random sample of date, time, filename to make sure all okay
#summaryIndices[sample(nrow(summaryIndices), 10),1:4]

saveRDS(object = summaryIndices, file = "outputs/indices/summaryIndices.RDS")
