# noisyminer
Estimating Noisy miner presence and woodland bird diviersity using acoustic indices

***
## Acoustic & biodiversity data

### Acoustic data
Audio recordings were collected from 50 sites in 9 regions from November 2019 until February 2022.

A total of xxx minutes of audio were analysed.

### Biodiversity data 
Bird surveys were conducted on two seperate occassions during each sampling trip,

Biodiversity metric | Definition
-------|---------------
**Total Bird Diversity** |
Detected20 | Number of species detected in 20 minute surveys
Detected40 | Number of species detected in 40 minute surveys
Mean20m | Number of individuals detected in 20 minute surveys
Mean40m | Number of individuals detected in 40 minute surveys
**Noisy Miner Diversity** | 
NMPresent | Whether Noisy miners were detected during either of the two replicate surveys
Threshold20m | Whether Noisy miners were present in numbers above a threshold (2.4 individuals)
Threshold40m | Whether Noisy miners were present in numbers above a threshold (7 individuals)
MeanMiner20m | The mean number of Noisy miners detected during 20 minute surveys
MeanMiner40m | The mean number of Noisy miners detected during 40 minute surveys

***
## Methods

### Generation of acoustic indices
Acoustic indices were generated for all recordings at a 1-minute temporal resolution using a combination of QUT's Analysis Programs (https://github.com/QutEcoacoustics/audio-analysis) and R (packages: `seewave` & `soundecology`), and QUT's Analysis Programs (https://github.com/QutEcoacoustics/audio-analysis) using the following two scripts: 

* [CalculateIndices_R.R](scripts/CalculateIndices_R.R)
* [CalculateIndices_AP.R](scripts/CalculateIndices_AP.R)

The following acoustic indices have been generated (all QUT indices are using default settings):

Acoustic Index | Defition, packages, settings etc.
-------|---------------
**R Indices**|
Acoustic Complexity Index (ACI) | Generated using the `soundecology` package. Settings: `min_freq = 0`, `max_freq = 11025`
Acoustic Diveristy Index (ADI) | Generated using the `soundecology` package. Settings: `max_freq = 10000`, `freq_step = 1000`
Acoustic Evenness Index (AEI) | Generated using the `soundecology` package. Settings: `max_freq = 10000`, `freq_step = 1000`
Normalised (NDSI) | Generated using the `soundecology` package. Default values used: `anthro_min = 1000`, `anthro_max = 2000`, `bio_min = 2000`, `bio_max = 11000`
Normalised (NDSI_bio) | Biophony component of NDSI (i.e., 2-11kHz) *These values should probably be changed*
Normalised (NDSI_anthro) | Anthropophony component of NDSI (i.e., 1-2kHz) *These values should probably be changed*
M | Median of the amplitude envelope `seewave`
H | Total entropy `seewave`
Ht | Temporal entropy `seewave`
Hf | Spectral entropy `seewave`
BI | Bioacoustic index `soundecology`
**QUT Analysis Programs Indices**| 
Activity (ACT) | The fraction of values in the noise-reduced decibel envelope that exceed the threshold, θ = 3 dB.
EventsPerSecond (ENV) | A measure of the number of acoustic events per second, averaged over the same noise-reduced one-minute segment. An acoustic event is defined as starting when the decibel envelope crosses a threshold, θ, from below to above, where θ = 3 dB.
LowFreqCover (LFC) | The fraction of noise-reduced spectrogram cells that exceed 3 dB in the low-frequency band (1-1000 Hz).
MidFreqCover (MFC) | As for LFC but in the mid-frequency band (1000-8000 Hz).
HighFreqCover (HFC) | As for LFC but in the high-frequency band (8000–11025 Hz).
AcousticComplexity (ACI) | A measure of the relative change in acoustic intensity in each frequency bin.
TemporalEntropy (ENT) | Entropy of the energy (squared amplitude) values of the signal waveform.
ClusterCount (CLS) | The number of distinct spectral clusters in the mid-frequency band of a one-minute segment of recording.
Ndsi (NDSI) | NDSI aims at estimating the level of anthropogenic disturbance on the soundscape by computing the ratio of human-generated (anthropophony) to biological (biophony) acoustic components 
SptDensity (SPD) | A measure of the number of cells in the mid-frequency band of a one-minute spectrogram that are identified as being local maxima.


### Noisy miner specific indices
The following Noisy miner specific acoustic indices have been tried so far:

We calculated custom acoustic indices for specific frequency bands in an attempt to tailor them for predicting the presence and number of Noisy miners.

We aggregated QUT's spectral indices (ACI, CVR, ENT, and PMN) at the following bands:
*1.5kHz - 4.0kHz
*2.0kHz - 3.0kHz
*4.0kHz - 7.0kHz
*5.0kHz - 6.0kHz

### Feature reduction
Spectral indices were reduced from 256 to 16

IMAGE

### Outlier removal
Initial inspection of acoustic index values identified some extreme values...
We removed the bottom and top 0.5% of values for each acoustic index per site.


### Number of recording days used
To investigate the effect of recording duration on the relationship between acoustic indices and total bird biodiversity and Noisy miner presence and abundance, acoustic indices were aggregated using between 1 and 9 days of recordings accompanying each survey period.

Days were selected at random from the audio within 2 days either side of the two replicate bird survey dates.

### Acoustic index aggregation
Acoustic indices were aggregated into median values for different time periods:

* "dawn" - 2 hour period from 30 mins before sunrise until 1.5 hours after sunrise
* "solarNoon" - 2 hour period from 1 hour before solar noon until 1 hour after solar noon
* "dusk" - 2 hour period from 1.5 hours before sunset until 30 mins after sunset
* "day" - variable time period between sunrise and sunset

Sunrise and sunset times for all survey dates were extracted using the `suncalc` package (see [CalculateSunriseSunsetTimes.R](scripts/CalculateSunriseSunsetTimes.R))

Data were removed if less than 70% of audio was available for the respective time period.

### Biodiversity data
The aggreated acoustic indices and biodiversity data for [Replicate 1](rawdata/FinalDataR1Only.csv) and [Replicate 2](rawdata/FinalMeanDataAllRepeats.csv) were joined using [CombineIndices&Biodiversity.R](scipts/CombineIndices&Biodiversity.R) for use in further analyses.

*Note: Noisy miner threshold columns appear to be incorrect in provided rawdata for Replicate 1, and are recalculated in the CombineIndices&Biodiversity.R script.*

### Plotting and analyses

All the following are done for both Replicate 1 only and Replicate 1&2 combined datasets.

#### Basic plots
[BasicPlots.R](scripts/BasicPlots.R) produces scatterplots for each acoustic index and each continuous biodiversity measure (Total20m, Total40m, Diversity20m, Diversity40m, TotalMiner20, TotalMiner40) and boxplots for each acoustic index and each binary biodiversity measure (Noisy miner presence-absence, Threshold20m, and Threshold40m).

For example (boxplots of Noisy miner presence for all acoustic indices calculated at dawn):

<p>
<img src="./outputs/figures/basicplots/R1Only/boxplots/boxplot_NMPresent_dawn.png" width=50% height=50%>
</p>

See [outputs/figures/basicplots](outputs/figures/basicplots) for all plots.


#### Correlation between biodiversity and individual indices
Bootstrap spearman correlations are calculated for all acoustic indices and bird biodiversity measures (all birds and miner measures) in the [BootstrapCorrelationPlots_Spearman.R](scripts/BootstrapCorrelationPlots_Spearman.R) script.

#### Multiple indices to predict Noisy miner presence (PCA - LDA)

PCA plots were produced for all Noisy miner presence variables (NMPresent, Threshold20m, Threshold40m) using all acoustic indices for the four time periods (dawn, solarNoon, dusk, day).

Linear discriminant analysis (LDA) was used to try to develop predictive models of Noisy miner presence using all principal components with eigenvalues greater than 1. 


***
## Results

### Total bird biodiversity

#### Correlation between biodiversity and individual indices

In general, all acoustic indices had relatively low (or no) correlation with any of the bird biodiversity measures examined at any daily time period for both Replicate 1 (Figure 1) and Replicate 1 and Replicate 2 combined (Figure 2).


![](outputs/figures/bootstrapcorrelations/R1Only_correlationPlot_total_diversity_R_spearman.png)
*Figure 1. Bootstrap spearman correlation estimates of individual acoustic indices and bird biodiversity measures (Total 20 minutes, Total 40 minutes, Species Diversity 20 minutes, Species Diversity 40 minutes) for Replicate 1.*


![](outputs/figures/bootstrapcorrelations/R1R2Combined_correlationPlot_total_diversity_R_spearman.png)
*Figure 2. Bootstrap spearman correlation estimates of individual acoustic indices and bird biodiversity measures (Total 20 minutes, Total 40 minutes, Species Diversity 20 minutes, Species Diversity 40 minutes) for Replicates 1 & 2 combined.*

Note: The above figures are for the indices generated using R. There are similar plots for indices created using QUT's Analysis Programs [here](outputs/figures/bootstrapcorrelations/R1Only_correlationPlot_total_diversity_AP_spearman.png) and [here](outputs/figures/bootstrapcorrelations/R1R2Combined_correlationPlot_total_diversity_AP_spearman.png), as well as a single plot with a reduced set of combined indices [here](outputs/figures/bootstrapcorrelations/R1Only_correlationPlot_total_diversity_ReducedSet_spearman.png) and [here](outputs/figures/bootstrapcorrelations/R1R2Combined_correlationPlot_total_diversity_ReducedSet_spearman.png).

Similarly low correlations were found for the Number of noisy miners. See [here](outputs/figures/bootstrapcorrelations/R1Only_correlationPlot_NumberNoisyMiner_spearman.png) and [here](outputs/figures/bootstrapcorrelations/R1R2Combined_correlationPlot_NumberNoisyMiner_spearman.png) for R1Only and R1R2Combined plots respectively.

### Noisy Miner presence/absence and abundance

#### Individual Acoustic Indices and Noisy miner presence

Using the spectral indices calculated at specific bands to detect Noisy miner presence, there seems to be some evidence that ACI and CVR (occasionally ENT and PMN too) are lower in the 4kHz-7kHz band when Noisy miners are present (during dawn and dusk). However, the effect sizes are small.

![]("outputs/figures_2023/boxplots_NMPresence/Spectral Indices - NMPresent - dusk - 5days.png")
*Figure X. Raincloud plots of Spectral Indices for Noisy miner presence (1) and absence (0) at dusk using 5 days of audio.*


Using the summary indices, there appears to be some evidence that there are lower values of HFC and NDSI and higher values of LFC when Noisy miners are present. This was detected during both dawn and dusk periods (e.g. Figure 3). However, like above, effect sizes are small.

![](outputs/figures_2023/boxplots_NMPresence/SummaryIndices/Summary Indices - NMPresent - dawn - 5days.png)
*Figure 3. Raincloud plots of Summary Indices for Noisy miner presence (1) and absence (0) at dawn using 5 days of audio.*

#### PCA plots of Noisy miner presence

There was poor separation between Noisy miner presence variables using combinations of acoustic indices in a PCA at any time period (Figure 4).

![](outputs/figures_2023/pca/SpectralAggregateIndices/Spectral Aggregate Indices - NMPresent - 5days.png)
*Figure 4. PCA plots for Spectral Indices and the 'NMPresent' response (A:'dawn', B:'solarNoon', C:'dusk', D:'day'). Spectral indices used were ACI, CVR, ENT and PMN aggregated at four frequency bands (1.5kHz-4.0kHz, 2.0kHz - 3.0kHz, 4.0kHz - 7.0kHz, and 5.0kHz - 6.0kHz).*

Attempting to use unique time-of-day periods per acoustic index (selected based on their highest correlation with MeanMiner40m) produced similar results (Figure 5).

![](outputs/figures/pca/BestTimePeriodPerIndex_Threshold40m.png)
*Figure 5. PCA plot for Replicate 1 and 2 combined and the Threshold40m response using unique time periods per acoustic index*

See [outputs/figures_2023/pca](outputs/figures_2023/pca) for all PCA plots.

#### Performance of LDA models fit to Principal Components



### 