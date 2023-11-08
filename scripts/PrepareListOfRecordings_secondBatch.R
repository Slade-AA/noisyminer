originalRecordingList <- readRDS("recordingsList/RecordingList_20230412.rds")

newRecordingList <- readRDS("recordingsList/ListOfRecordings_within2OfSurveys_updated.rds")
newRecordingList$dirname <- gsub(".wav", "", newRecordingList$Recording)
newRecordingList$recordingName <- basename(newRecordingList$Recording)


#remove lower case 'b' and 'd' from 'recordingName'
unique(unlist(lapply(originalRecordingList$recordingName, function(x) unique(unlist(strsplit(as.character(x), ""))))))
unique(unlist(lapply(newRecordingList$recordingName, function(x) unique(unlist(strsplit(as.character(x), ""))))))

#Check where and why these characters occur
b <- newRecordingList[grep("b", newRecordingList$recordingName), ]
d <- newRecordingList[grep("d", newRecordingList$recordingName), ]
#'e' and 'c' look like they should stay - not part of URL encoding and don't get altered when downloaded
e <- newRecordingList[grep("e", newRecordingList$recordingName), ]
c <- newRecordingList[grep("c", newRecordingList$recordingName), ]

newRecordingList$recordingName <- gsub("b", "B", newRecordingList$recordingName)
newRecordingList$recordingName <- gsub("d", "D", newRecordingList$recordingName)



#Calculate difference
difference <- dplyr::anti_join(newRecordingList, originalRecordingList, by = c("Site", "Date", "Time"))



saveRDS(difference, file = "recordingsList/RecordingList_20230803.rds")

output.file <- file("recordingsList/RecordingList_20230803.csv", "wb")
write.table(difference, file=output.file, quote = FALSE, row.names = FALSE, sep=",", eol="\n")
close(output.file)