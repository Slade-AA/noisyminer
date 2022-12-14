#test of hardRain method

library(tidyverse)
library(tuneR)
library(soundecology)
library(seewave)
library(foreach)
library(doParallel)

wavFiles <- c(list.files(path = "S:/JCU/NoisyMiner_Recordings/RC6", pattern = "*.wav", recursive = TRUE, full.names = TRUE))


#check if indices have already been calculated for any of these files
originalLength <- length(wavFiles)
wavFiles <- wavFiles[!file.exists(paste0(gsub("NoisyMiner_Recordings", "NoisyMiner_Indices_R", dirname(wavFiles)), "/", gsub(".wav", "_hardRain.RDS", basename(wavFiles))))]
if (originalLength != length(wavFiles)) {
  print(paste0(originalLength - length(wavFiles), " recordings have already had indices generated. Generating indices for ", length(wavFiles), " new files."))
}

#set-up parallel backend to process files quicker
numCores <- detectCores()
registerDoParallel(numCores)

# Start of analysis loop - iterate through each file
loop.times <- c()

#parallel loop
for (wavFile in wavFiles) {
  start.time <- Sys.time()
  # print progress
  cat('\n','Processing recording', which(wavFiles == wavFile), 'of', length(wavFiles),'\n')
  
  #Skip files that have issues - encountered a couple of files with no data that would cause the script to stop
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
    
    #hardRain calculations
    wl <- length(wavMinute)
    wl <- wl - wl%%2
    
    fs <- seewave::spectro(wavMinute, wl = wl, wn = "rectangle", fftw = T, plot = F, dB = NULL)
    
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
    
    #join indices into a tibble
    indices <- tibble(Site = site,
                      Date = date,
                      Time = time + (minute * 60),
                      Filename = wavFile,
                      band.1.psd = res2[,1],
                      band.2.psd = res2[,2],
                      band.1.s2n = res2[,3],
                      band.2.s2n = res2[,4])
    
    return(indices)
  }
  results <- do.call(rbind, results)
  dir.create(path = gsub("NoisyMiner_Recordings", "NoisyMiner_Indices_R", dirname(wavFile)), recursive = TRUE, showWarnings = FALSE)
  saveRDS(results, paste0(gsub("NoisyMiner_Recordings", "NoisyMiner_Indices_R", dirname(wavFile)), "/", gsub(".wav", "_hardRain.RDS", basename(wavFile))))
  
  remove(wavHeader, wavDuration, site, date, time, results)
  
  end.time <- Sys.time()
  
  loop.time <- as.numeric(difftime(end.time, start.time, unit = "secs"))
  
  loop.times <- append(loop.times, loop.time)
  
  current_progress <- paste0("Estimated time remaining: ", round(mean(loop.times)*(length(wavFiles)-which(wavFiles == wavFile))/60, 1), " minutes (", round(mean(loop.times), 1), " secs per recording)")
  cat("\r", current_progress)
}
stopImplicitCluster() # stop parallel cluster