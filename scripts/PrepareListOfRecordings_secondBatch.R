originalRecordingList <- readRDS("recordingsList/RecordingList_20230412.rds")

newRecordingList <- readRDS("recordingsList/ListOfRecordings_within2OfSurveys_updated.rds")
newRecordingList$dirname <- dirname(newRecordingList$Recording)
newRecordingList$recordingName <- basename(newRecordingList$Recording)


difference <- dplyr::anti_join(newRecordingList, originalRecordingList, by = c("Site", "Date", "Time"))

saveRDS(difference, file = "recordingsList/RecordingList_20230803.rds")