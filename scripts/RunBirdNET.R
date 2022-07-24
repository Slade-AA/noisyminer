#Run BirdNET

folders <- list.dirs('S:/JCU/NoisyMiner_Recordings',
                     recursive = TRUE)

folders <- folders[-which(nchar(folders) < 35)]

for (folder in folders) {
  
  inputFolder <- folder
  
  outputFolder <- gsub("Recordings", "BirdNET", inputFolder) #create output folder from input folder
  
  if (dir.exists(outputFolder)) {
    print(paste0("Skipping folder ", inputFolder, ". Output already detected!"))
    next
  }
  
  dir.create(outputFolder, recursive = TRUE)
  
  #get lat and lon values from csv
  site <- gsub(".*Recordings/([0-9A-Z]{2,3})/.*", "\\1", inputFolder)
  
  GPS_Site <- read.csv("outputs/GPS_Site.csv")
  
  lat <- GPS_Site$lat[GPS_Site$Site == site]
  lon <- GPS_Site$lon[GPS_Site$Site == site]
  
  #calculate week from folder name or use -1
  week <- -1 #Values in [1, 48] (4 weeks per month). Set -1 for year-round species list.
  
  threads <- 16
  batchsize <- 4
  
  
  # prepare command
  # can also specify start and end offset with -s and -e options
  #command <- sprintf("audio2csv %s C:/AP/ConfigFiles/Towsey.Acoustic.yml %s --quiet --parallel --when-exit-copy-config", paste0('"', file, '"'), paste0('"', output_folder, '"'))
  command <- sprintf("--i %s --o %s --lat %s --lon %s --week %s --threads %s --batchsize %s", inputFolder, outputFolder, lat, lon, week, threads, batchsize)
  
  start.time <- Sys.time()
  # execute the command
  system2('C:\\BirdNET-Analyzer\\BirdNET-Analyzer.exe', command)
  end.time <- Sys.time()
  print(end.time - start.time)
  
  #~7.5mins per day of recording
}