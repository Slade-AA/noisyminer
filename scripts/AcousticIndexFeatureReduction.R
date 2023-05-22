#Produce reduced Acoustic Index Feature Sets for all the days of an ARU deployment"
#selects five indices from the concatenated index.csv files and converts them to a reduced feature set suitable for machine learning.
#Code adapted from Mike Towsey

#Mike has generated reduced feature sets of acoustic indices for machine learning purposes:
#The basic method is to aggregate spectral indices in certain frequency bins to reduce the set down from 256 to 16

#$reductionBounds = (8, 8, 8, 8, 8, 8, 10, 12, 14, 16, 18, 21, 24, 27, 31, 35)

#Experience suggests the following indices are the most useful combination: (\"BGN\", \"PMN\", \"ACI\", \"ENT\", \"RHZ\"). For documentation on the meaning of these indices, see https://eprints.qut.edu.au/110634/ 
#The available indices are: ACI, BGN, CVR, ENT, EVN, OSC, PMN, RHZ, RNG, RPS, RVT, SPT. However some of these are highly correlated and therfore less useful for machine learning purposes


# Visualisation of the process ----

library(tidyverse)

data <- read.csv("acousticIndex_reduction.csv")

data$label <- c(8, 8, 8, 8, 8, 8, 10, 12, 14, 16, 18, 21, 24, 27, 31, 35, rep("", nrow(data)-16))

ggplot(data = data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_rect(colour = "black", fill = "transparent") +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 11000, 1000)) +
  scale_x_continuous(breaks = c(0.5, 1.5),labels = c("original", "reduced"), expand = c(0,0)) +
  labs(y = "Frequency (Hz)", x = "") +
  geom_text(aes(x = xmin + (xmax - xmin)/2, y = ymin + (ymax - ymin)/2,
                label = label)) +
  theme_classic()

# Functions to reduce and normalize acoustic indices ----

testIndexSet <- read.csv("E:/AcousticIndices_AP/Trip1-Dagworth-A/20220719T160000+1000_Trip1-Dagworth-A_1008927.wav/Towsey.Acoustic/20220719T160000+1000_Trip1-Dagworth-A_1008927__Towsey.Acoustic.ACI.csv")

# ├ Reduce feature set ----

reduceFeatureSet <- function(inputIndices,
                             reductionBounds = c(8, 8, 8, 8, 8, 8, 10, 12, 14, 16, 18, 21, 24, 27, 31, 35)) {
  
  columnIndices <- data.frame(firstColumn = 2 + (cumsum(reductionBounds) - reductionBounds),
                              lastColumn = 1 + cumsum(reductionBounds))
  
  outputIndices <- data.frame(Index = inputIndices$Index)
  
  for (outputIndex in 1:length(reductionBounds)) {
    
    outputIndices[[as.character(outputIndex)]] <- rowMeans(inputIndices[,c(columnIndices$firstColumn[outputIndex]:columnIndices$lastColumn[outputIndex])])
    
  }
  return(outputIndices)
}

testOutput <- reduceFeatureSet(inputIndices = testIndexSet)

# Normalise Index ----

normaliseIndex <- function(inputIndices,
                           index) {
  
  # The following min and max values are used for normalisation of the narrowband index value.
  # These are obtained directly from the IndexPropertiesConfig.yml file and should not need to be changed.
  
  #Why do some of my actual measured values fall outside of the min and max values though? - resulting in negative values and values > 1
  minIndexNormValues = c(
    "ACI" = 0.4,
    "BGN" = -85.0,
    "CVR" = 0.0,
    "ENT" = 0.0,
    "EVN" = 0.0,
    "OSC" = 0.1,
    "PMN" = 0.0,
    "RHZ" = 0.1,
    "RNG" = 0.1,
    "RPS" = 0.0,
    "RVT" = 0.0,
    "SPT" = 0.05
  )
  
  maxIndexNormValues = c(
    "ACI" = 0.7,
    "BGN" = -30.0,
    "CVR" = 0.4,
    "ENT" = 0.6,
    "EVN" = 4.0,
    "OSC" = 5.0,
    "PMN" = 15.0,
    "RHZ" = 1.0,
    "RNG" = 0.4,
    "RPS" = 0.4,
    "RVT" = 0.4,
    "SPT" = 0.5
  )
  
  df = inputIndices[,c(2:ncol(inputIndices))]
  df[] = lapply(df, function(x) (x - minIndexNormValues[index]) / (maxIndexNormValues[index] - minIndexNormValues[index]))
  #normValue = (inputValue - minIndexNormValues[index]) / (maxIndexNormValues[index] - minIndexNormValues[index])
  
  outputIndices <- cbind(inputIndices[,1, drop = FALSE], df)
  return(outputIndices)
}

testOutput2 <- normaliseIndex(testOutput, "ACI")

# ├ Reduce and Normalise Indices ----

reduceAndNormaliseIndices <- function(inputIndices, 
                                      index, 
                                      reductionBounds = c(8, 8, 8, 8, 8, 8, 10, 12, 14, 16, 18, 21, 24, 27, 31, 35)) {
  output1 <- reduceFeatureSet(inputIndices = inputIndices, reductionBounds = reductionBounds)
  output2 <- normaliseIndex(inputIndices = output1, index = index)
  
  return(output2)
}

testOutput3 <- reduceAndNormaliseIndices(testIndexSet, "ACI")
