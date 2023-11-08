#Comparing indice components - e.g., how does BI relate to the anthro and bio components of ndsi?

library(soundecology)

AudioFile <- tuneR::readWave("C:/Users/jc696551/Documents/20191114_190000_Continuous_1minute.wav")

BI <- bioacoustic_index(AudioFile, min_freq = 2000, max_freq = 8000)
NDSI <- ndsi(AudioFile, anthro_min = 1000, anthro_max = 2000, bio_min = 2000, bio_max = 8000)
NDSI <- ndsi(AudioFile, anthro_min = 1500, anthro_max = 4000, bio_min = 4000, bio_max = 7000)
ndsi <- seewave::NDSI(seewave::soundscapespec(AudioFile))

ADI <- acoustic_diversity(AudioFile)

#Load long file and scatterplot each minute against one another - are they correlated?



SoundScape <- seewave::soundscapespec(AudioFile, plot = F)


seewave::NDSI(seewave::soundscapespec(AudioFile, plot = F), anthropophony = 1, biophony = 4:7)

(sum(SoundScape[4:7,2]) - sum(SoundScape[1,2]))/(sum(SoundScape[4:7,2]) + sum(SoundScape[1,2]))


#seewave
seewave::NDSI(seewave::soundscapespec(AudioFile, plot = F), anthropophony = 1:3, biophony = 4:7)

#seewave - manual
(sum(SoundScape[4:7,2]) - sum(SoundScape[1:3,2]))/(sum(SoundScape[4:7,2]) + sum(SoundScape[1:3,2]))

#soundecology
ndsi(AudioFile, anthro_min = 1000, anthro_max = 3000, bio_min = 4000, bio_max = 7000)







ACI_total <- acoustic_complexity(AudioFile, min_freq = 0, max_freq = 11025)
#ACI at specific bands
acoustic_complexity(AudioFile, min_freq = 1500, max_freq = 4000) #[18:47] = ~1500-4000Hz
sum(ACI_total$aci_fl_left_vals[18:47])

acoustic_complexity(AudioFile, min_freq = 2000, max_freq = 3000) #[24:36] = ~2000-3000Hz
sum(ACI_total$aci_fl_left_vals[24:36])

acoustic_complexity(AudioFile, min_freq = 4000, max_freq = 7000) #[47:82] = ~4000-7000Hz
sum(ACI_total$aci_fl_left_vals[47:82])


acoustic_complexity(AudioFile, min_freq = 5000, max_freq = 6000) #[59:71] = ~5000-6000Hz
sum(ACI_total$aci_fl_left_vals[59:71])




#Bioacoustic index
BI_minute <- bioacoustic_index(AudioFile, min_freq = 2000, max_freq = 8000, fft_w = 512)

#Manual calculation
spec_left <- spectro(AudioFile, plot = F, dB = "max0", wl = 512)$amp
specA_left <- apply(spec_left, 1, meandB)
rows_width = length(specA_left) / (AudioFile@samp.rate/2)
min_row = 2000 * rows_width
max_row = 8000 * rows_width
specA_left_segment <- specA_left[min_row:max_row]
freq_range <- 8000 - 2000
freqs <- seq(from = 2000, to = 8000, length.out = length(specA_left_segment))

specA_left_segment_normalized <- specA_left_segment - min(specA_left_segment)

left_area <- sum(specA_left_segment_normalized * rows_width)




#benchmark
library(rbenchmark)

benchmark("standard" = {
  NDSI <- ndsi(AudioFile, anthro_min = 1000, anthro_max = 2000, bio_min = 2000, bio_max = 8000)
},
"capture.output nullfile" = {
  capture.output(NDSI <- ndsi(AudioFile, anthro_min = 1000, anthro_max = 2000, bio_min = 2000, bio_max = 8000), file = nullfile())
},
"invisible capture.output" = {
  invisible(capture.output(NDSI <- ndsi(AudioFile, anthro_min = 1000, anthro_max = 2000, bio_min = 2000, bio_max = 8000)))
},
replications = 20,
columns = c("test", "replications", "elapsed",
            "relative", "user.self", "sys.self"))

#                      test replications elapsed relative user.self sys.self
#2  capture.output nullfile           20   51.95    1.025     50.48     1.46
#3 invisible capture.output           20   50.69    1.000     49.49     1.17
#1                 standard           20   52.47    1.035     51.16     1.31