library(tidyverse)

ListOfRecordings <- read.csv(file = "recordingsList/ListOfRecordings.csv", stringsAsFactors = F)

#extract site and date from recording path
ListOfRecordings <- ListOfRecordings %>% rowwise() %>%  mutate(Site = strsplit(as.character(Recording), "/")[[1]][1],
                                                               Date = as.Date(gsub(".*([0-9]{8}).*", "\\1",strsplit(as.character(Recording), "/")[[1]][2]), format = "%Y%m%d"),
                                                               Time = case_when(
                                                                 grepl("T[0-9]{6}", basename(Recording)) ~ strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(Recording)), gsub(".*T([0-9]{6}).*", "\\1", basename(Recording))), "%Y%m%d%H%M%S"),
                                                                 grepl(".*[0-9]{8}_[0-9]{6}_", basename(Recording)) ~ strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(Recording)), gsub(".*_([0-9]{6})_.*", "\\1", basename(Recording))), "%Y%m%d%H%M%S")
                                                               ),
                                                               .before = Recording)


#filter out recordings not within n days of on ground survey

#load surveyDates
#surveyDates <- read.csv(file = "recordingsList/surveyDates.csv")
surveyDates <- read.csv(file = "recordingsList/surveyDates_updated.csv")

ListOfRecordings <- ListOfRecordings %>% filter(Site %in% unique(surveyDates$SiteID)) #Only keep 50 survey sites

surveyDates <- surveyDates %>% rowwise() %>% mutate(Date = as.Date(as.character(Date), format = "%d/%m/%Y"))

ListOfRecordings$Within2 <- NA
for (row in 1:nrow(ListOfRecordings)) {
  ListOfRecordings$Within2[row] <- ifelse(min(as.numeric(abs(ListOfRecordings$Date[row] - surveyDates$Date[surveyDates$SiteID == ListOfRecordings$Site[row]]))) <= 2, 'Yes', 'No')
}

ListOfRecordings <- ListOfRecordings %>% filter(Within2 == "Yes")

#saveRDS(ListOfRecordings, file = "outputs/ListOfRecordings_within2OfSurveys.rds")
saveRDS(ListOfRecordings, file = "outputs/ListOfRecordings_within2OfSurveys_updated.rds")

#check whether recordings have already been analysed