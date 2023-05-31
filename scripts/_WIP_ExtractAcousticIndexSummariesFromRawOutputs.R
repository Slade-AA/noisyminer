# Script to read Acoustic Index files and summarise by time period

#load packages
library(tidyverse)

#load functions
source("scripts/functions_AcousticIndexFeatureReduction.R")

#List files for analysis (perform by site folder?)
siteDirectories <- list.dirs("E:/_combinedIndices/Indices_AP", recursive = F)

for (site in 1:length(siteDirectories)) {
  
  files <- list.files(path = siteDirectories[site], full.names = T)
  
  start.time <- Sys.time()
  
  Indices_Summary <- data.frame()
  Indices_SpectralAggregated <- data.frame()
  Indices_FeatureReduction <- data.frame()
  Indices_R <- data.frame()
  
  for (file in files) {
    #data <- readRDS("E:/_combinedIndices/Indices_AP/G1/G1_2020-03-08_20200308T000000%2B1100_Continuous.RDS")
    data <- readRDS(file)
    
    #############
    #Fixing Time#
    #############
    
    #Time value in data$info is not necessarily correct - it has just been extracted from the filename and set in R using AEST as the timezone
    #We need to adjust these values to correct times using the UTC offset and then set them to Sydney's timezone for easy interpretability
    
    #Add +11:00 as UTC offset for recordings that are missing a UTC offset in filename. Manual checking of log files indicated this was the offset used
    if (is.na(data$info$TZ)) {
      data$info$TZ <- "1100"
    }
    
    #Manually adjust time to UTC using filename and UTC offset before converting to Sydney time
    data$info$Time <- lubridate::with_tz(lubridate::force_tz(data$info$Time - (3600*as.numeric(gsub("([0-9]{2}).*","\\1",data$info$TZ))), "UTC"), "Australia/Sydney")
    
    
    ####################
    #AP Summary Indices#
    ####################
    summaryIndicesToUse <- c("Activity",
                             "EventsPerSecond",
                             "HighFreqCover",
                             "MidFreqCover",
                             "LowFreqCover",
                             "AcousticComplexity",
                             "TemporalEntropy",
                             "ClusterCount",
                             "Ndsi",
                             "SptDensity")
    
    SummaryIndices <- data.frame(Site = data$info$Site,
                                 Date = data$info$Date,
                                 Time = data$info$Time + (data$Indices$ResultMinute*60),
                                 data$Indices[which(colnames(data$Indices) %in% summaryIndicesToUse)])
    
    ############################
    #Spectral Index Aggregation#
    ############################
    #Targetted Noisy Miner frequency bands for some indices (ACI, CVR, ENT, PMN)
    spectralIndices <- c('ACI', 'CVR', 'ENT', 'PMN')
    frequencyBands <- list(Band_1.5_4.0 = c(low = 1500, high = 4000),
                           Band_2.0_3.0 = c(low = 2000, high = 3000),
                           Band_4.0_7.0 = c(low = 4000, high = 7000),
                           Band_5.0_6.0 = c(low = 5000, high = 6000))
    
    SpectralIndexAggregation <- data.frame(#Index = data[[spectralIndex]][,1],
      Site = data$info$Site,
      Date = data$info$Date,
      Time = data$info$Time + (data[['ACI']][,1]*60))
    
    for (spectralIndex in spectralIndices) {
      tmp <- data.frame(Band01 = rowMeans(data[[spectralIndex]][,(floor(frequencyBands[['Band_1.5_4.0']][['low']]/43.06641)+2):(ceiling(frequencyBands[['Band_1.5_4.0']][['high']]/43.06641)+2)]),
                        Band02 = rowMeans(data[[spectralIndex]][,(floor(frequencyBands[['Band_2.0_3.0']][['low']]/43.06641)+2):(ceiling(frequencyBands[['Band_2.0_3.0']][['high']]/43.06641)+2)]),
                        Band03 = rowMeans(data[[spectralIndex]][,(floor(frequencyBands[['Band_4.0_7.0']][['low']]/43.06641)+2):(ceiling(frequencyBands[['Band_4.0_7.0']][['high']]/43.06641)+2)]),
                        Band04 = rowMeans(data[[spectralIndex]][,(floor(frequencyBands[['Band_5.0_6.0']][['low']]/43.06641)+2):(ceiling(frequencyBands[['Band_5.0_6.0']][['high']]/43.06641)+2)]))
      
      tmp <- tmp %>% rename_with(.fn = ~paste0(spectralIndex, "_", names(frequencyBands[1])), .cols = Band01)
      tmp <- tmp %>% rename_with(.fn = ~paste0(spectralIndex, "_", names(frequencyBands[2])), .cols = Band02)
      tmp <- tmp %>% rename_with(.fn = ~paste0(spectralIndex, "_", names(frequencyBands[3])), .cols = Band03)
      tmp <- tmp %>% rename_with(.fn = ~paste0(spectralIndex, "_", names(frequencyBands[4])), .cols = Band04)
      
      SpectralIndexAggregation <- bind_cols(SpectralIndexAggregation, tmp)              
    }
    
    ########################
    #Compressed Feature Set#
    ########################
    reductionIndices <- c("BGN", "PMN", "ACI", "ENT", "RHZ")
    
    FeatureReductionIndices <- data.frame(#Index = data[['ACI']][,1],
      Site = data$info$Site,
      Date = data$info$Date,
      Time = data$info$Time + (data[['ACI']][,1]*60))
    
    for (reductionIndex in reductionIndices) {
      tmp <- reduceAndNormaliseIndices(data[[reductionIndex]], reductionIndex)
      
      tmp <- tmp %>% rename_with(.fn = ~paste0(reductionIndex, "_", .), .cols = -all_of('Index'))
      
      FeatureReductionIndices <- bind_cols(FeatureReductionIndices, tmp %>% select(-Index))
    }
    
    ###########
    #R Indices#
    ###########
    data_R <- readRDS(gsub("Indices_AP", "Indices_R", file))
    
    data_R <- data.frame(Site = data$info$Site,
                         Date = data$info$Date,
                         Time = data$info$Time + (as.numeric(rownames(data_R))*60),
                         data_R[,5:ncol(data_R)])
    
    #Bind outputs
    Indices_Summary <- bind_rows(Indices_Summary, SummaryIndices)
    Indices_SpectralAggregated <- bind_rows(Indices_SpectralAggregated, SpectralIndexAggregation)
    Indices_FeatureReduction <- bind_rows(Indices_FeatureReduction, FeatureReductionIndices)
    Indices_R <- bind_rows(Indices_R, data_R)
  }
  end.time <- Sys.time() #5.019173 mins - per folder
  
  
  output <- list(Indices_Summary = Indices_Summary, 
                 Indices_SpectralAggregated = Indices_SpectralAggregated, 
                 Indices_FeatureReduction = Indices_FeatureReduction,
                 Indices_R = Indices_R)
  
  saveRDS(output, file = paste0("E:/_combinedIndices/", data$info$Site, ".RDS"))
}