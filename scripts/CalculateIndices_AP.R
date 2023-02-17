generate.fcs(audio.directory <- c("S:/JCU/NoisyMiner_Recordings/G4",
                                  "S:/JCU/NoisyMiner_Recordings/RC4"),
             rawoutput.directory <- "S:/JCU/NoisyMiner_Indices_AP")




#Function ----

generate.fcs <- function(audio.directory, rawoutput.directory) {
  
  # Get a list of audio files in the directory
  audio.files <- list.files(audio.directory, pattern = "*.wav|*.flac|*.mp3|*.wma|*.ogg", recursive=TRUE, full.names = TRUE)
  
  if(length(audio.files) == 0){
    cat("\n"); stop("Warning: No audio files found in folder")
  }
  
  #if (dir.exists(fcsoutput.directory) == FALSE) {
  #  dir.create(fcsoutput.directory, recursive = TRUE)
  #}
  
  # Start of analysis loop - iterate through each file
  loop.times <- c()
  
  for(i in 1:length(audio.files)) {
    start.time <- Sys.time()
    # print progress
    cat('\n','Processing recording', i, 'of', length(audio.files),'\n')
    
    file <- audio.files[i]
    
    # parse base name of file
    #file_name <- basename(file)
    
    # make a folder for results - name contains audio file name
    output_folder <- normalizePath(gsub(".wav", "", gsub("NoisyMiner_Recordings", "NoisyMiner_Indices_AP", file)), mustWork = FALSE)
    
    #output_folder <- normalizePath(file.path(rawoutput.directory, file_name), mustWork = FALSE)
    dir.create(output_folder, recursive = TRUE)
    
    # prepare command
    # can also specify start and end offset with -s and -e options
    command <- sprintf("audio2csv %s C:/AP/ConfigFiles/Towsey.Acoustic.yml %s --quiet --parallel --when-exit-copy-config", paste0('"', file, '"'), paste0('"', output_folder, '"'))
    
    # execute the command
    system2('C:\\AP\\AnalysisPrograms.exe', command)
    
    # find the ACI false-colour spectrogram and copy to a separate folder
    #fcs.png <- list.files(output_folder, pattern="_ACI-ENT-EVN\\.png", recursive=TRUE, full.names = TRUE)
    #file.copy(fcs.png, fcsoutput.directory)
    
    end.time <- Sys.time()
    
    loop.time <- as.numeric(difftime(end.time, start.time, unit = "secs"))
    
    loop.times <- append(loop.times, loop.time)
    
    current_progress <- paste0("Estimated time remaining: ", round(mean(loop.times)*(length(audio.files)-i)/60, 1), " minutes (", round(mean(loop.times), 1), " secs per recording)")
    cat("\r", current_progress)
  }
  # end of loop
}