#Extract summary indices from QUT outputs - frequency specific summaries

# Load packages ----

library(tidyverse)
library(lubridate)

# Read in indices, add columns for site, sensor etc. ----

frequencyBand <- c(low = 1166, high = 3646) #Noisy miner 'churr' vocalisation
indices <- c('CVR', 'ENT')


for (index in indices) {
  specificIndicesFiles <- list.files(path = "E:/NoisyMiner_Indices_AP",
                                     pattern = paste0(".*Towsey.Acoustic.", index, ".csv$"),
                                     recursive = TRUE,
                                     full.names = TRUE)
  
  
  spectralIndices <- list()
  for (file in specificIndicesFiles) {
    tmp <- read.csv(file = file)
    
    tmp <- data.frame(Index = tmp[,1],
                      RowMeans = rowMeans(tmp[,(floor(frequencyBand[['low']]/43.06641)+2):(ceiling(frequencyBand[['high']]/43.06641)+2)]),
                      RowMeans2 = rowMeans(tmp[,c(2:(floor(frequencyBand[['low']]/43.06641)+1),(ceiling(frequencyBand[['high']]/43.06641)+3):257)]))
    
    tmp <- tmp %>% mutate(ND = (tmp$RowMeans-tmp$RowMeans2)/(tmp$RowMeans+tmp$RowMeans2))
    tmp <- tmp %>% rename_with(.fn = ~paste0(index, "_", frequencyBand[['low']], "_", frequencyBand[['high']]), .cols = RowMeans)
    tmp <- tmp %>% rename_with(.fn = ~paste0(index, "_0_", frequencyBand[['low']], "_", frequencyBand[['high']], "_11025"), .cols = RowMeans2)
    tmp <- tmp %>% rename_with(.fn = ~paste0(index, "_ND"), .cols = ND)
    
    tmp$Site <- gsub(".*NoisyMiner_Indices_AP/([0-9A-Z]{2,4})/.*", "\\1", file)
    
    #extract date and time from filename and column in csv - needs to accomadate different naming systems
    if (grepl("T[0-9]{6}\\+", basename(file))) {
      tmp$DATETIME <- strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(file)), gsub(".*T([0-9]{6})\\+.*", "\\1", basename(file))), "%Y%m%d%H%M%S") + (tmp$Index * 60)
    } else if (grepl(".*[0-9]{8}_[0-9]{6}_", basename(file))) {
      tmp$DATETIME <- strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(file)), gsub(".*_([0-9]{6})_.*", "\\1", basename(file))), "%Y%m%d%H%M%S") + (tmp$Index * 60)
    } else {
      print(paste0("Unrecognised file name format - Skipping file: ", file))
      next
    }
    
    #extract hour and 1 min interval from datetime
    tmp$TIME_NEW <- strftime(tmp$DATETIME, format = "%H:%M:%S")
    
    spectralIndices[[paste0(tmp$Site[1], "_", tmp$DATETIME[1])]] <- tmp
    
  }
  
  spectralIndices <- do.call(rbind, spectralIndices)
  
  assign(paste0(index), spectralIndices)
}

combined <- merge(get(indices[1]), get(indices[2]))

combined <- combined %>% mutate(Date = as.character(as.POSIXct(gsub("^([0-9\\-]{10}).*", "\\1", DATETIME), format = "%Y-%m-%d")))



# Save output ----
saveRDS(object = combined, file = paste0("outputs/indices/spectralIndices_AP_", paste(indices, collapse = "_"), ".RDS"))
