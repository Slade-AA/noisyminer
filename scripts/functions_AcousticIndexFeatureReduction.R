# Functions to reduce and normalize acoustic indices ----

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

# ├ Reduce and Normalise Indices ----

reduceAndNormaliseIndices <- function(inputIndices, 
                                      index, 
                                      reductionBounds = c(8, 8, 8, 8, 8, 8, 10, 12, 14, 16, 18, 21, 24, 27, 31, 35)) {
  output1 <- reduceFeatureSet(inputIndices = inputIndices, reductionBounds = reductionBounds)
  output2 <- normaliseIndex(inputIndices = output1, index = index)
  
  return(output2)
}