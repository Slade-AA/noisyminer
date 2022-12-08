#test of hardRain method

library(seewave)

wavFiles <- c(list.files(path = "S:/JCU/NoisyMiner_Recordings/RC6", pattern = "*.wav", recursive = TRUE, full.names = TRUE))[1]

results <- foreach (minute = 0:(wavDuration-1), .packages = c("dplyr", "ineq", "tuneR", "soundecology", "seewave")) %dopar% {
  wavMinute <- readWave(wavFile, from = minute, to = minute + 1, units = "minutes")
  
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
}

hardRain_values <- hardRain::getMetrics(wavMinute)

