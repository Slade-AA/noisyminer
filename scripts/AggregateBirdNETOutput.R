#Aggregate BirdNET output
library(tidyverse)
library(lubridate)

BirdNETFiles <- list.files(path = "E:/NoisyMiner_BirdNET",
                           full.names = TRUE,
                           recursive = TRUE)

BirdNETDetections <- list()
for (file in BirdNETFiles) {
  tmp <- read.table(file, header = TRUE, sep = "\t")
  if (nrow(tmp) == 0) {
    next
  }
  tmp$Site <- gsub(".*NoisyMiner_BirdNET/([0-9A-Z]{2,4})/.*", "\\1", file)
  
  #extract time from filename - multiple file naming conventions are in use in this dataset!
  if (grepl("T[0-9]{6}\\+", basename(file))) {
    tmp$Time <- strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(file)), gsub(".*T([0-9]{6})\\+.*", "\\1", basename(file))), "%Y%m%d%H%M%S")
  } else if (grepl(".*[0-9]{8}_[0-9]{6}_", basename(file))) {
    tmp$Time <- strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(file)), gsub(".*_([0-9]{6})_.*", "\\1", basename(file))), "%Y%m%d%H%M%S")
  } else {
    print(paste0("Unrecognised file name format - Skipping file: ", file))
    next
  }
  
  BirdNETDetections[[file]] <- tmp
}

BirdNETDetections <- do.call(rbind, BirdNETDetections)

NoisyMinerDetections <- test %>% filter(Common.Name == 'Noisy Miner')
NoisyMinerDetections <- NoisyMinerDetections %>% mutate(Date = date(Time))


# Load biodiversity data ----

# ├ richness ----

richness_abundance <- read.csv("rawdata/SummarySurveyData2019_2021.csv")

richness_abundance <- richness_abundance[complete.cases(richness_abundance),] #remove any NA rows
richness_abundance$SurveyID <- gsub(" ", "", richness_abundance$SurveyID) #remove spaces from SurveyID
richness_abundance$SiteID <- gsub(" ", "", richness_abundance$SiteID) #remove spaces from SiteID

richness_abundance <- richness_abundance %>% 
  rename(Site = SiteID) %>% 
  mutate(Date = as.character(as.Date(Date, "%d/%m/%Y")),
         Site = as.character(Site))

#richness data frame has some duplicates (127, 129, 6 look to be errors)
richness_abundance_dups <- richness_abundance[richness_abundance$SurveyID %in% richness_abundance$SurveyID[which(duplicated(richness_abundance$SurveyID))],]

richness_abundance <- richness_abundance[-c(127, 129, 6),] #remove incorrect duplicates

#extract 'season', 'seasonYear' and 'replicate' from 'SurveyID'
richness_abundance <- richness_abundance %>% mutate(season = gsub("^[A-Z]{1,2}[0-9]{1,2}([A-Za-z]{6})[0-9]{5}", "\\1", SurveyID),
                                                    seasonYear = gsub("^[A-Z]{1,2}[0-9]{1,2}([A-Za-z]{6}[0-9]{4})[0-9]{1}", "\\1", SurveyID),
                                                    replicate = gsub("^[A-Z]{1,2}[0-9]{1,2}[A-Za-z]{6}[0-9]{4}", "", SurveyID))

Plots <- list()
for (confidence in c(0.3, 0.5, 0.7, 0.9)) {
  tmp <- NoisyMinerDetections %>% 
    filter(Confidence >= confidence) %>% 
    mutate(Date = as.character(Date)) %>% 
    group_by(Date, Site) %>% 
    summarise(nDetections = n()) %>% 
    left_join(richness_abundance)
  
  print(paste0("Pearson correlation: ", cor(x = tmp$nDetections, y = tmp$NumberNoisyMiner, method = "pearson")))
  print(paste0("Spearman correlation: ", cor(x = tmp$nDetections, y = tmp$NumberNoisyMiner, method = "spearman")))
  
  Plots[[as.character(confidence)]] <- ggplot(data = tmp, aes(x = nDetections, y = NumberNoisyMiner)) +
    geom_point() +
    theme_bw()
}