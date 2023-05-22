# Alter HPC jobscripts with new cloudUNE password as well as removing png files after index generation


files <- list.files("HPCJobScripts/", pattern = ".sh$", full.names = TRUE)

for (file in files) {
  fileContent = readLines(file, warn = FALSE)
  
  fileContent[70] = '\tcurl -u "dF57xj7DmPOE3Oi:FiveCorners" -O -J "https://cloud.une.edu.au/public.php/webdav/Acoustic%20recordings/$recordingPath"'
  
  fileContent[75] = "\trm $dirname/Towsey.Acoustic/*.png"
  
  
  charVector <- strsplit(fileContent,"")
  rawFileContent <- NULL
  for(currentString in charVector) {
    currentString <- unlist(lapply(currentString, charToRaw))
    #convert every char to raw
    rawFileContent <- c(rawFileContent, currentString, charToRaw("\n"))
    #concatenate raw string with \n, to differentiate strings
  }
  fileConn <- file(file, "wb")
  writeBin(rawFileContent, fileConn)  
  close(fileConn)
}