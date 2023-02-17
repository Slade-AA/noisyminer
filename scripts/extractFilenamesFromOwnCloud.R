data <- readtext::readtext("recordingsList/owncloud_propfind.txt")

stringExtract <- stringr::str_extract_all(data$text, "/public.php/webdav/([/%\\[\\]\\.\\-0-9A-z]{2,125}.wav)</d")

ListOfRecordings <- data.frame(Recording = unlist(stringExtract))

ListOfRecordings$Recording <- gsub("/public.php/webdav/([-/%\\.0-9A-z]{2,125}.wav)</d", "\\1", ListOfRecordings$Recording)

write.csv(ListOfRecordings, file = "recordingsList/ListOfRecordings.csv", row.names = F)