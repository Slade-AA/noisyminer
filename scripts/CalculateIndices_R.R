library(tidyverse)
library(tuneR)
library(soundecology)
library(seewave)
library(foreach)
library(doParallel)

wavFiles <- "E:/NoisyMiner_Recordings/BN3/20211109_STUDY/20211109T080000+1100_REC.wav"

numCores <- detectCores() - 2
registerDoParallel(numCores)

time0 <- Sys.time()

recordings <- list()
for (wavFile in wavFiles) {
  
  wavHeader <- readWave(wavFile, header = TRUE)
  wavDuration <- floor((wavHeader$samples/wavHeader$sample.rate)/60)
  
  #calculate site, date, and time once per file
  site <- gsub(".*NoisyMiner_Recordings/(.{3})/.*", "\\1", wavFile)
  date <- as.POSIXct(gsub("([0-9]{8}).*", "\\1", basename(wavFile)), format = "%Y%m%d") #extract date of recording from filename
  if (grepl("REC", wavFile)) {
    time <- strptime(paste0(gsub("([0-9]{8}).*", "\\1", basename(wavFile)), gsub(".*T([0-9]{6})\\+.*", "\\1", basename(wavFile))), "%Y%m%d%H%M%S")
  } else {
    time <- strptime(paste0(gsub("([0-9]{8}).*", "\\1", basename(wavFile)), gsub(".*_([0-9]{6})_.*", "\\1", basename(wavFile))), "%Y%m%d%H%M%S")
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
    BI_minute <- bioacoustic_index(wavMinute, min_freq = 400, max_freq = 5000) #default is: "min_freq = 2000, max_freq = 8000"
    
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
                      BI = BI_minute$left_area)
    
    return(indices)
  }
  results <- do.call(rbind, results)
  #write.csv(results, paste0("./Indices/", gsub(".*/(.+).wav", "\\1", wavFile), ".csv"), row.names = F) #save results to csv for each recording
  recordings[[wavFile]] <- results
}
time1 <- Sys.time() #2.051814 mins for 30min file (~55 days for 800 days of recordings) with seewave ACI and NDSI
#1.470522 mins for 30min file (~39 days for 800 days of recordings) without seewave ACI and NDSI
time1 - time0
stopImplicitCluster()

#Manually calculating AE from ADI may be fractionally faster than using soundecology function
#ADI and AEI separate - Time difference of 1.474172 mins
#ADI with AEI manually calculated - Time difference of 1.434434 mins

