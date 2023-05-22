output.file <- file("recordingsList/RecordingList_20230412.csv", "wb")
write.table(ListOfRecordings, file=output.file, quote = FALSE, row.names = FALSE, sep=",", eol="\n")
close(output.file)