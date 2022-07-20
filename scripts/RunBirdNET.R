#Run BirdNET

folders <- list.dirs()

for (folder in folders) {
  
}

inputFolder <- 'ssd/recordings'

outputFolder <- 'ssd/outputs' #create output folder from input folder
dir.create(outputFolder, recursive = TRUE)

#get lat and lon values from csv
lat <- -34
lon <- 155

#calculate week from folder name or use -1
week <- 42 #Values in [1, 48] (4 weeks per month). Set -1 for year-round species list.

threads <- 12
batchsize <- 1

BirdNET-Analyzer.exe --i inputFolder --o outputFolder --lat lat --lon lon --week week --threads threads --batchsize batchsize



# prepare command
# can also specify start and end offset with -s and -e options
#command <- sprintf("audio2csv %s C:/AP/ConfigFiles/Towsey.Acoustic.yml %s --quiet --parallel --when-exit-copy-config", paste0('"', file, '"'), paste0('"', output_folder, '"'))
command <- sprintf("--i %s --o %s --lat %s --lon %s --week %s --threads %s --batchsize %s", inputFolder, outputFolder, lat, lon, week, threads, batchsize)


# execute the command
system2('C:\\BirdNET\\BirdNET-Analyzer.exe', command)