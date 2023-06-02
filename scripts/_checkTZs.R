siteDirectories <- list.dirs("E:/_combinedIndices/Indices_AP", recursive = F)

files <- list.files(path = siteDirectories, full.names = T)


tzs <- c()

for (file in files) {
  data <- readRDS(file)
  
  tzs <- c(tzs, data$info$TZ)
}

unique(tzs)
[1] "1100" "1030" "0000" NA     "1000"

save(files, tzs, file = "tzs.RData")

#What are the "1030" ones about????? - how to handle?

#1030 tz issues
files_1030 <- files[which(tzs == "1030")]
#Sites
unique(gsub("_.*","",basename(files_1030))) #All from G2
#Dates
unique(gsub("G2_([-0-9]{10})_.*","\\1",basename(files_1030)))


#0000 tz issues
files_0000 <- files[which(tzs == "0000")]
#Sites
unique(gsub("_.*","",basename(files_0000)))
#Dates
unique(gsub("G4_([-0-9]{10})_.*","\\1",basename(files_0000[which(gsub("_.*","",basename(files_0000)) == "G4")])))
#[1] "2020-07-04" "2020-07-05" "2020-07-06" "2020-07-07" "2020-07-08" "2020-07-09" "2020-07-10" "2020-07-11"

unique(gsub("RC1_([-0-9]{10})_.*","\\1",basename(files_0000[which(gsub("_.*","",basename(files_0000)) == "RC1")])))


#No tz in filename issues
files_NA <- files[which(is.na(tzs))]
#Sites
unique(gsub("_.*","",basename(files_NA)))
#Dates
unique(gsub("GP1_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "GP1")])))

unique(gsub("Y1_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "Y1")])))

unique(gsub("RC6_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "RC6")])))


#Dates - 'B' Sites
unique(gsub("BC1_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BC1")])))
unique(gsub("BC2_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BC2")])))
unique(gsub("BN1_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BN1")])))
unique(gsub("BN2_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BN2")])))
unique(gsub("BN3_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BN3")])))
unique(gsub("BN4_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BN4")])))
unique(gsub("BN5_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BN5")])))
unique(gsub("BN6_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BN6")])))

unique(gsub("BS1_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BS1")])))
unique(gsub("BS2_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BS2")])))
unique(gsub("BS3_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BS3")])))
unique(gsub("BS4_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BS4")])))
unique(gsub("BS5_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BS5")])))
unique(gsub("BS6_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BS6")])))
unique(gsub("BS7_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BS7")])))
unique(gsub("BS8_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BS8")])))
unique(gsub("BS9_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BS9")])))
unique(gsub("BS10_([-0-9]{10})_.*","\\1",basename(files_NA[which(gsub("_.*","",basename(files_NA)) == "BS10")])))


#1000 tz files - why do these have 1000 as the UTC offset when most other files have 1100 (even when incorrect)?
files_1000 <- files[which(tzs == "1000")]
#Sites
unique(gsub("_.*","",basename(files_1000)))
#Dates
unique(gsub("SR1_([-0-9]{10})_.*","\\1",basename(files_1000)))



#Fixing


lubridate::with_tz(lubridate::force_tz(as.POSIXct("2020-10-04 02:00:00") - (3600*11), "UTC"), "Australia/Sydney")

lubridate::with_tz(lubridate::force_tz(as.POSIXct("2020-10-04 03:00:00") - (3600*11), "UTC"), "Australia/Sydney")





#after manually adding a UTC offset for all the files with 'NA' (hopefully all 1100?) then the following should correct all times

data$info$Time

#becomes

lubridate::with_tz(lubridate::force_tz(data$info$Time - (3600*as.numeric(gsub("([0-9]{2}).*","\\1",data$info$TZ))), "UTC"), "Australia/Sydney")