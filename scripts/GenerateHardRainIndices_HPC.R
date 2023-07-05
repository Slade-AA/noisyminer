library(tidyverse)
library(ineq)
library(tuneR)
library(soundecology)
library(seewave)
#library(foreach)
#library(doParallel)

recordingsList <- readRDS("/home/jc696551/NoisyMiner/RecordingList_20230412.rds")

rowNum <- as.numeric(Sys.getenv("rowNum"))

wavFile <- paste0("/scratch/jc696551/", recordingsList$Site[rowNum], "/", recordingsList$recordingName[rowNum])

#Skip files that have issues - encountered a couple of files with no data that would cause the script to stop
try({wavHeader <- readWave(wavFile, header = TRUE)})
if (exists("wavHeader") == FALSE) {
  print(paste0("Issue with audio recording - Skipping file: ", wavFile))
  #file.remove(wavFile)
  quit()
}

if (wavHeader$samples < 44100) {
  print(paste0("Issue with audio recording (recording too short!) - Skipping file: ", wavFile))
  #file.remove(wavFile)
  quit()
}

wavDuration <- floor((wavHeader$samples/wavHeader$sample.rate)/60)


#calculate site, date, and time once per file
site <- recordingsList$Site[rowNum]
date <- recordingsList$Date[rowNum]
time <- recordingsList$Time[rowNum]

#generate indices

#set-up parallel backend to process files quicker
#numCores <- detectCores()
#registerDoParallel(numCores)

#R indices
results <- list()
for (minute in 0:(wavDuration-1)) {
#results <- foreach (minute = 0:(wavDuration-1), .packages = c("dplyr", "ineq", "tuneR", "soundecology", "seewave")) %dopar% {  
  wavMinute <- readWave(wavFile, from = minute, to = minute + 1, units = "minutes")
  
  #while most of the recordings are mono, some are stereo - deciding which channel to use based on amount of sound present
  if (wavMinute@stereo) {
    if (sum(abs(wavMinute@left)) > sum(abs(wavMinute@right))) {
      wavMinute <- mono(wavMinute, which = "left")
    } else {
      wavMinute <- mono(wavMinute, which = "right")
    }
  }
  
  #hardRain calculations
   wl <- length(wavMinute)
   wl <- wl - wl%%2
    
   fs <- seewave::spectro(wavMinute, wl = wl, wn = "rectangle", fftw = F, plot = F, dB = NULL)
    
   mfs.lst <- list(mapply(function(lo,hi) fs$amp[fs$freq > lo & fs$freq < hi, ,drop = F],
                          c(0.6, 4.4), c(1.2,5.6), SIMPLIFY = F))
    
   res <- lapply(mfs.lst, function(x) {
     psd <- sapply(x, colMeans) # psd of filtered frequency window
     s2n <- sapply(x, function(y) apply(y, 2, function(z) mean(z)/sd(z))) # sig2noise ratio
     list(psd=matrix(psd, ncol = length(c(0.6, 4.4))), s2n=matrix(s2n, ncol = length(c(0.6, 4.4))))
     # above, make format consistent as matrix..  if slow, then use if(is.null(t.step))
   })
    
   tmp <- lapply(1:2, function(x) do.call(rbind, sapply(res, function(y) y[x])))
   res2 <- do.call(cbind, tmp)
  
  
  results[[as.character(minute)]] <- tibble(Site = site,
                      Date = date,
                      Time = time + (minute * 60),
                      Filename = wavFile,
                      band.1.psd = res2[,1],
                      band.2.psd = res2[,2],
                      band.1.s2n = res2[,3],
                      band.2.s2n = res2[,4])
  
  #return(indices)
}
results <- do.call(rbind, results)
dir.create(path = paste0("/scratch/jc696551/", recordingsList$dirname[rowNum], "/indices_HardRain"), recursive = TRUE, showWarnings = FALSE)
saveRDS(results, paste0("/scratch/jc696551/", recordingsList$dirname[rowNum], "/indices_HardRain/", basename(dirname(recordingsList$Recording[rowNum])), ".rds"))

#stopImplicitCluster()