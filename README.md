# noisyminer
Project using audio to monitor for Noisy miners. Audio recordings from Paul McDonald

***
## Scripts
Add list of scripts and what they do

1. Scripts for generating acoustic indices
  + [CalculateIndices_R.R](scripts/CalculateIndices_R.R)
  + [CalculateIndices_AP.R](scripts/CalculateIndices_AP.R)

***
## Methods

### Generation of acoustic indices
Acoustic indices were generated for all recordings at a 1-minute temporal resolution using a combination of R (packages: `seewave` & `soundecology`), and QUT's Analysis Programs (https://github.com/QutEcoacoustics/audio-analysis) using the following two scripts: 

* [CalculateIndices_R.R](scripts/CalculateIndices_R.R)
* [CalculateIndices_AP.R](scripts/CalculateIndices_AP.R)

Acoustic Index | Defition, packages, settings etc.
-------|---------------
**R Indices**|
Acoustic Complexity Index (ACI) | Default values from `soundecology`
Acoustic Diveristy Index (ADI) | sdsds
Acoustic Evenness Index (AEI) | sdsds
Normalised (NDSI) | `soundecology`
Normalised (NDSI_bio) | Biophony component of NDSI
Normalised (NDSI_anthro) | Athropophony component of NDSI
M | Median of the amplitude envelope `seewave`
H | Total entropy `seewave`
Ht | Temporal entropy `seewave`
Hf | Spectral entropy `seewave`
BI | Bioacoustic index `soundecology`
**QUT Analysis Programs Indices**|
Activity (ACT) |
EventsPerSecond (ENV) | 
SpectralCentroid (CENT) | 
HighFreqCover (HFC) |
MidFreqCover (MFC) | 
LowFreqCover (LFC) |
AcousticComplexity (ACI) | 
TemporalEntropy (ENT) |
EntropyOfAverageSpectrum (EAS) |  
EntropyOfVarianceSpectrum (EVS) |
EntropyOfPeaksSpectrum (EPS) |
EntropyOfCoVSpectrum (ECS) |
ClusterCount (CLS) | 
ThreeGramCount (TGC) | 
Ndsi (NDSI) | 
SptDensity (SPD) |
CVR??? |


Noisy miner specific acoustic indices

Acoustic Index | Def
-------|---------------
Bio_chur | Bioacoustic index calculated between 1166 and 3646 Hz. Based on the peak frequency (+-1 SD) of the 'chur' vocalisation in [Holt et al. 2017](https://www.tandfonline.com/doi/full/10.1080/01584197.2016.1252508)
CVR_ | Cover index between xxx Hz and xxx Hz - 
ENT_ | TemporalEntropy index between xxx Hz and xxx Hz -



### Acoustic index aggregation
Acoustic indices were aggregated into average values for different time periods:

* "dawn" - 2 hour period from 30 mins before sunrise until 1.5 hours after sunrise
* "solarNoon" - 2 hour period from 1 hour before solar noon until 1 hour after solar noon
* "dusk" - 2 hour period from 1.5 hours before sunset until 30 mins after sunset
* "day" - variable time period between sunrise and sunset

Sunrise and sunset times for all survey dates were extracted using the `suncalc` package (see [CalculateSunriseSunsetTimes.R](scripts/CalculateSunriseSunsetTimes.R))

Data were removed if less than 70% of audio was available for that time period.

### Biodiversity data
The aggreated acoustic indices and biodiversity data for [Replicate 1](rawdata/FinalDataR1Only.csv) and [Replicate 2](rawdata/FinalMeanDataAllRepeats.csv) were joined using [CombineIndices&Biodiversity.R](scipts/CombineIndices&Biodiversity.R) for use in further analyses.

*Note: Noisy miner threshold columns appear to be incorrect in provided rawdata for [Replicate 1](rawdata/FinalDataR1Only.csv), and are recalculated in the [CombineIndices&Biodiversity.R](scipts/CombineIndices&Biodiversity.R) script.*

### Plotting and analyses

#### Correlation between biodiversity and individual indices
Bootstrap spearman correlations are calculated for all acoustic indices and bird biodiversity measures (all birds and miner measures) in the [BootstrapCorrelationPlots_Spearman.R](scripts/BootstrapCorrelationPlots_Spearman.R) script.

***
## Results

#### Correlation plots
![](outputs/figures/bootstrapcorrelations/R1Only_correlationPlot_total_diversity_R_spearman.png)