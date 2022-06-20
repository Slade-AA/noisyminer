library(tidyverse)
library(tuneR)
library(soundecology)
library(seewave)
library(foreach)
library(doParallel)

wavFiles <- c(list.files(path = "E:/NoisyMiner_Recordings/Y3", pattern = "*.wav", recursive = TRUE, full.names = TRUE))

#check if indices have already been calculated for any of these files
originalLength <- length(wavFiles)
wavFiles <- wavFiles[!file.exists(paste0(gsub("NoisyMiner_Recordings", "NoisyMiner_Indices", dirname(wavFiles)), "/", gsub(".wav", ".RDS", basename(wavFiles))))]
if (originalLength != length(wavFiles)) {
  print(paste0(originalLength - length(wavFiles), " recordings have already had indices generated. Generating indices for ", length(wavFiles), " new files."))
}

#set-up parallel backend
numCores <- detectCores()
registerDoParallel(numCores)

time0 <- Sys.time()

#recordings <- list()
for (wavFile in wavFiles) {
  
  try({wavHeader <- readWave(wavFile, header = TRUE)})
  if (exists("wavHeader") == FALSE) {
    print(paste0("Issue with audio recording - Skipping file: ", wavFile))
    next
  }
  
  wavDuration <- floor((wavHeader$samples/wavHeader$sample.rate)/60)
  
  #calculate site, date, and time once per file
  site <- gsub(".*NoisyMiner_Recordings/([0-9A-Z]{2,3})/.*", "\\1", wavFile)
  date <- as.POSIXct(gsub(".*([0-9]{8}).*", "\\1", basename(wavFile)), format = "%Y%m%d") #extract date of recording from filename
  
  #extract time from filename - multiple file naming conventions are in use in this dataset!
  if (grepl("T[0-9]{6}\\+", basename(wavFile))) {
    time <- strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(wavFile)), gsub(".*T([0-9]{6})\\+.*", "\\1", basename(wavFile))), "%Y%m%d%H%M%S")
  } else if (grepl(".*[0-9]{8}_[0-9]{6}_", basename(wavFile))) {
    time <- strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(wavFile)), gsub(".*_([0-9]{6})_.*", "\\1", basename(wavFile))), "%Y%m%d%H%M%S")
  } else {
    print(paste0("Unrecognised file name format - Skipping file: ", wavFile))
    next
  }
  
  
  results <- foreach (minute = 0:(wavDuration-1), .packages = c("dplyr", "ineq", "tuneR", "soundecology", "seewave")) %dopar% {
    wavMinute <- readWave(wavFile, from = minute, to = minute + 1, units = "minutes")
    
    ACI_minute_soundecology <- acoustic_complexity(wavMinute, min_freq = 0, max_freq = 11025)
    #ACI_minute_seewave <- ACI(wavMinute)
    ADI_minute <- acoustic_diversity(wavMinute, max_freq = 10000, freq_step = 1000)
    #AE_minute <- acoustic_evenness(wavMinute, max_freq = 10000, freq_step = 1000)
    #NDSI_minute_seewave <- NDSI(soundscapespec(wavMinute, plot=FALSE))
    NDSI_minute_soundecology <- ndsi(wavMinute)
    wav_env <- env(wavMinute, plot = FALSE) #amplitude envelope calculated separately as it is used by M and Ht
    M_minute <- median(wav_env)*(2^(1-wavHeader$bits))
    Ht_minute <- th(wav_env) #temporal entropy
    Hf_minute <- sh(meanspec(wavMinute, plot = FALSE)) #spectral entropy
    H_minute <- Ht_minute*Hf_minute #manual computation using Ht and Hf - significant speed improvement
    BI_minute <- bioacoustic_index(wavMinute, min_freq = 2000, max_freq = 8000) #default is: "min_freq = 2000, max_freq = 8000"
    BI_minute_chur <- bioacoustic_index(wavMinute, min_freq = 1166, max_freq = 3646)
    
    indices <- tibble(Site = site,
                      Date = date,
                      Time = time + (minute * 60), #extract date and time from filename
                      Filename = wavFile,
                      ACI_soundecology = ACI_minute_soundecology$AciTotAll_left, 
                      ACI_soundecology_f = list(ACI_minute_soundecology$aci_fl_left_vals),
                      #ACI_seewave = ACI_minute_seewave,
                      ADI = ADI_minute$adi_left,
                      ADI_values = list(ADI_minute$left_band_values),
                      ADI_names = list(ADI_minute$left_bandrange_values),
                      AE = Gini(ADI_minute$left_band_values),#AE_minute$aei_left, 
                      #NDSI_seewave = NDSI_minute_seewave,
                      NDSI_soundecology = NDSI_minute_soundecology$ndsi_left,
                      NDSI_bio = NDSI_minute_soundecology$biophony_left,
                      NDSI_anthro = NDSI_minute_soundecology$anthrophony_left,
                      M = M_minute,
                      H = H_minute,
                      Ht = Ht_minute, 
                      Hf = Hf_minute, 
                      BI = BI_minute$left_area,
                      BI_chur = BI_minute_chur$left_area)
    
    return(indices)
  }
  results <- do.call(rbind, results)
  dir.create(path = gsub("NoisyMiner_Recordings", "NoisyMiner_Indices", dirname(wavFile)), recursive = TRUE, showWarnings = FALSE)
  saveRDS(results, paste0(gsub("NoisyMiner_Recordings", "NoisyMiner_Indices", dirname(wavFile)), "/", gsub(".wav", ".RDS", basename(wavFile))))
  
  remove(wavHeader, wavDuration, site, date, time, results)
}
time1 <- Sys.time()
time1 - time0
stopImplicitCluster()



#2.051814 mins for 30min file (~55 days for 800 days of recordings) with seewave ACI and NDSI
#1.470522 mins for 30min file (~39 days for 800 days of recordings) without seewave ACI and NDSI
#Manually calculating AE from ADI may be fractionally faster than using soundecology function
#ADI and AEI separate - Time difference of 1.474172 mins
#ADI with AEI manually calculated - Time difference of 1.434434 mins (33.5561 secs on home PC - 15 days for 800 days of recordings)

