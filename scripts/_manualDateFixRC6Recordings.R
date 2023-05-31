#Manually fix Date for RC6 recordings with different naming structure

load("tzs.RData")

files_diff <- files[which(grepl("[A-Z0-9]{2,4}_NA_.*", basename(files)))]

for (file_diff in files_diff) {
  data <- readRDS(file_diff)
  
  data$info$Date <- as.Date(data$info$Time)
  
  saveRDS(data, file = file_diff)
}