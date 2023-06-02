#Code to combine QUTs AnalysisPrograms output files (acoustic index csv files) into a single .RDS file
#Motivation for this is to reduce file size for storage (~3x reduction in size, >5x reduction in size on disk), and reduce time to transfer files for backup

library(tidyverse)

dirs <- list.dirs("E:/SR3")
dirs <- dirs[grepl("Towsey.Acoustic", dirs)]

dirs <- dirs[1:20]

for (dir in dirs) {
  csvFiles <- list.files(dir, pattern = ".csv$", full.names = T)
  
  indices <- list()
  for (csvFile in csvFiles) {
    indices[[gsub(".*Towsey.Acoustic.([A-z]{3,7})\\.csv", "\\1",basename(csvFile))]] <- read.csv(csvFile)
  }
  
  #add an 'info' element to list that contains site, date, time and timezone information
  indices[["info"]] <- list(Site = gsub("[A-Z]:/([A-Z0-9]{2,4})/.*", "\\1", dir),
                            Date = as.Date(gsub("[A-Z]:/([A-Z0-9]{2,4})/([0-9]{8})_.*", "\\2", dir), "%Y%m%d"), #this doesn't work on a handful of files with different naming structure (possible fix is to add '.*' before second group capture - need to test)
                            Time = case_when(
                              grepl("T[0-9]{6}", basename(csvFiles[1])) ~ strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(csvFiles[1])), gsub(".*T([0-9]{6}).*", "\\1", basename(csvFiles[1]))), "%Y%m%d%H%M%S"),
                              grepl(".*[0-9]{8}_[0-9]{6}_", basename(csvFiles[1])) ~ strptime(paste0(gsub(".*([0-9]{8}).*", "\\1", basename(csvFiles[1])), gsub(".*_([0-9]{6})_.*", "\\1", basename(csvFiles[1]))), "%Y%m%d%H%M%S")
                            ),
                            TZ = gsub(".*%2B([0-9]{4})_.*", "\\1", basename(csvFiles[1])))
  
  #some recordings appear to be missing timezone information in filename - in this case we will set to NA for now
  if (nchar(indices[["info"]]$TZ) > 4) {
    indices[["info"]]$TZ = NA
  }
  
  #saveRDS(object = indices,
  #        file = paste0(dir, "/indices.RDS"))
  
  #create directory to save files
  dir.create(path = paste0(gsub("([A-Z]:/).*", "\\1",dir), "_combinedIndices/Indices_AP/", indices[["info"]]$Site),
             recursive = T, showWarnings = F)
  dir.create(path = paste0(gsub("([A-Z]:/).*", "\\1",dir), "_combinedIndices/Indices_R/", indices[["info"]]$Site),
             recursive = T, showWarnings = F)
  
  #save RDS object with unique filename
  saveRDS(object = indices,
          file = paste0(gsub("([A-Z]:/).*", "\\1", dir), "_combinedIndices/Indices_AP/", indices[["info"]]$Site, "/", indices[["info"]]$Site, "_", indices[["info"]]$Date, "_", gsub("__Towsey.Acoustic.*", "", basename(csvFiles[1])), ".RDS"))
  
  #copy R indices over to same folder structure
  file.copy(from = list.files(gsub("Towsey.Acoustic", "indices_R", dir), full.names = T),
            to = paste0(gsub("([A-Z]:/).*", "\\1", dir), "_combinedIndices/Indices_R/", indices[["info"]]$Site, "/", indices[["info"]]$Site, "_", indices[["info"]]$Date, "_", gsub("__Towsey.Acoustic.*", "", basename(csvFiles[1])), ".RDS"))
}