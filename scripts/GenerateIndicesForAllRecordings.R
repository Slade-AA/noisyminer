library(tidyverse)
library(tuneR)
library(soundecology)
library(seewave)
library(foreach)
library(doParallel)

ListOfRecordings <- readRDS(file = "recordingsList/ListOfRecordings_within2OfSurveys.rds")

setwd("C:/Users/jc696551/Documents/Projects/noisyminer")

RecordingsToAnalyse <- ListOfRecordings[1:3,]

#set-up parallel backend to process files quicker
numCores <- detectCores()
registerDoParallel(numCores)

# Start of analysis loop - iterate through each file
loop.times <- c()

#for remaining files loop through and generate indices
for (recording in 1:nrow(RecordingsToAnalyse)) {
  start.time <- Sys.time()
  # print progress
  cat('\n','Processing recording', recording, 'of', nrow(RecordingsToAnalyse),'\n')
  
  
  #download audio recording from owncloud
  command <- paste0('curl -u "zMIyOKUE3jdK0kS:Bioacoustics" -O -J "https://cloud.une.edu.au/public.php/webdav/"', RecordingsToAnalyse$Recording[recording], '"')
  
  system(command)
  
  
  
  wavFile <- basename(RecordingsToAnalyse$Recording[recording])
  
  #Skip files that have issues - encountered a couple of files with no data that would cause the script to stop
  try({wavHeader <- readWave(wavFile, header = TRUE)})
  if (exists("wavHeader") == FALSE) {
    print(paste0("Issue with audio recording - Skipping file: ", wavFile))
    file.remove(wavFile)
    next
  }
  
  if (wavHeader$samples < 44100) {
    print(paste0("Issue with audio recording (recording too short!) - Skipping file: ", wavFile))
    file.remove(wavFile)
    next
  }
  
  wavDuration <- floor((wavHeader$samples/wavHeader$sample.rate)/60)
  
  
  #calculate site, date, and time once per file
  site <- RecordingsToAnalyse$Site[recording]
  date <- RecordingsToAnalyse$Date[recording]
  time <- RecordingsToAnalyse$Time[recording]
  
  #generate indices
  
  #R indices
  results <- foreach (minute = 0:(wavDuration-1), .packages = c("dplyr", "ineq", "tuneR", "soundecology", "seewave")) %dopar% {
    wavMinute <- readWave(wavFile, from = minute, to = minute + 1, units = "minutes")
    
    ACI_minute_soundecology <- acoustic_complexity(wavMinute, min_freq = 0, max_freq = 11025) #can use the values to extract ACI for specific bands (bin size = 85.46512Hz)
    ADI_minute <- acoustic_diversity(wavMinute, max_freq = 10000, freq_step = 1000) #default
    NDSI_minute_soundecology <- ndsi(wavMinute, anthro_min = 1000, anthro_max = 2000, bio_min = 2000, bio_max = 11000) #default frequency settings
    NDSI_minute_soundecology_2 <- ndsi(wavMinute, anthro_min = 0, anthro_max = 1000, bio_min = 1000, bio_max = 11000) #drops the default anthro band - which overlaps with biophony
    wav_env <- env(wavMinute, plot = FALSE) #amplitude envelope calculated separately as it is used by M and Ht
    M_minute <- median(wav_env)*(2^(1-wavHeader$bits))
    Ht_minute <- th(wav_env) #temporal entropy
    Hf_minute <- sh(meanspec(wavMinute, plot = FALSE)) #spectral entropy
    H_minute <- Ht_minute*Hf_minute #manual computation using Ht and Hf - significant speed improvement
    BI_minute <- bioacoustic_index(wavMinute, min_freq = 2000, max_freq = 8000) #default is: "min_freq = 2000, max_freq = 8000"
    BI_minute_chur <- bioacoustic_index(wavMinute, min_freq = 1166, max_freq = 3646) # I don't think BI is a great index for restricted frequency ranges - the area under the curve may not make sense as the lowest value is used for calculating the area
    SoundScape <- soundscapespec(wavMinute, plot = F)[1:10,2] #seewave output that can be used to calculate 'NDSI' using any combination of bands - NDSI = (biophony - anthropophony) / (biophony + anthropophony). Bands are 1-10kHz

  
    
    indices <- tibble(Site = site,
                      Date = date,
                      Time = time + (minute * 60),
                      Filename = wavFile,
                      ACI_soundecology = ACI_minute_soundecology$AciTotAll_left, 
                      #ACI at specific bands - can use these raw or calculate ration between different bands similar to NDSI
                      #[18:47] = ~1500-4000Hz
                      ACI_1500_4000 = sum(ACI_minute_soundecology$aci_fl_left_vals[18:47]),
                      #[24:36] = ~2000-3000Hz
                      ACI_2000_3000 = sum(ACI_minute_soundecology$aci_fl_left_vals[24:36]),
                      #[47:82] = ~4000-7000Hz
                      ACI_4000_7000 = sum(ACI_minute_soundecology$aci_fl_left_vals[47:82]),
                      #[59:71] = ~5000-6000Hz
                      ACI_5000_6000 = sum(ACI_minute_soundecology$aci_fl_left_vals[59:71]),
                      ACI_soundecology_f = list(ACI_minute_soundecology$aci_fl_left_vals), #keep all ACI values incase we want to look at other bands
                      ADI = ADI_minute$adi_left,
                      ADI_values = list(ADI_minute$left_band_values),
                      ADI_names = list(ADI_minute$left_bandrange_values),
                      AE = Gini(ADI_minute$left_band_values), #Acoustic evenness index calculated from ADI values - quicker
                      NDSI_soundecology = NDSI_minute_soundecology$ndsi_left,
                      NDSI_bio = NDSI_minute_soundecology$biophony_left,
                      NDSI_anthro = NDSI_minute_soundecology$anthrophony_left,
                      NDSI_soundecology_2 = NDSI_minute_soundecology_2$ndsi_left,
                      NDSI_bio_2 = NDSI_minute_soundecology_2$biophony_left,
                      NDSI_anthro_2 = NDSI_minute_soundecology_2$anthrophony_left,
                      SoundScapeSpec = list(SoundScape), #values for calculate NDSI using any combination of 1kHz frequency bands from 1-10kHz
                      #Ratio_23_47 = (sum(SoundScape[2])-sum(SoundScape[4:7]))/(sum(SoundScape[2])+sum(SoundScape[4:7])),
                      M = M_minute,
                      H = H_minute,
                      Ht = Ht_minute, 
                      Hf = Hf_minute, 
                      BI = BI_minute$left_area,
                      BI_chur = BI_minute_chur$left_area)
    
    return(indices)
  }
  results <- do.call(rbind, results)
  dir.create(path = paste0("C:/Users/jc696551/Documents/Projects/noisyminer/indices_R/", site, "/", date), recursive = TRUE, showWarnings = FALSE)
  saveRDS(results, paste0("C:/Users/jc696551/Documents/Projects/noisyminer/indices_R/", site, "/", date, "/", gsub(".wav", ".rds", wavFile)))
  
  remove(wavHeader, wavDuration, results)
  
  #AP indices
  # make a folder for results - name contains audio file name
  output_folder <- normalizePath(paste0("C:/Users/jc696551/Documents/Projects/noisyminer/indices_AP/", site, "/", date, "/", gsub(".wav", "", wavFile)), mustWork = FALSE)
  
  #output_folder <- normalizePath(file.path(rawoutput.directory, file_name), mustWork = FALSE)
  dir.create(output_folder, recursive = TRUE)
  command <- sprintf("audio2csv %s C:/AP/ConfigFiles/Towsey.Acoustic.yml %s --quiet --parallel --when-exit-copy-config", paste0('"', wavFile, '"'), paste0('"', output_folder, '"'))
  
  # execute the command
  system2('C:\\AP\\AnalysisPrograms.exe', command)
  
  
  
  #remove downloaded audio
  file.remove(wavFile)
  
  remove(command, wavFile, site, date, time)
  
  #print progress
  end.time <- Sys.time()
  
  loop.time <- as.numeric(difftime(end.time, start.time, unit = "secs"))
  
  loop.times <- append(loop.times, loop.time)
  
  current_progress <- paste0("Estimated time remaining: ", round(mean(loop.times)*(nrow(RecordingsToAnalyse)-recording)/60, 1), " minutes (", round(mean(loop.times), 1), " secs per recording)")
  cat("\r", current_progress)
}
stopImplicitCluster() # stop parallel cluster