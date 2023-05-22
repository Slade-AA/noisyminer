#Generate JobScripts for NoisyMiner analysis

library(tidyverse)

batchJobTemplate = "scripts/batchJobScript_calculateIndices.sh"

#load list of recordings
ListOfRecordings <- readRDS("recordingsList/RecordingList_20230412.rds")


#break rows into groups of =< 100 for batching and generate job scripts
numberFullJobs = floor(nrow(ListOfRecordings)/(100*5))
lengthLastJob = floor(nrow(ListOfRecordings)%%(100*5)/5) #this may leave off a few recordings at the end if they are not divisible by 5!

batchJobArrayIndices <- data.frame(startIndex = c(seq(from = 0, by = 100, length.out = numberFullJobs),
                                                  numberFullJobs*100),
                                   endIndex = c(seq(from = 99, by = 100, length.out = numberFullJobs),
                                                (numberFullJobs*100)+(lengthLastJob)))

#JobScriptDirectory <- paste0(paste(unique(gsub("-.*", "", topDetections$survey)), collapse = ""), "_", length(unique(topDetections$template)), "templates")
dir.create(path = "HPCJobScripts", recursive = TRUE, showWarnings = FALSE)



for (row in 1:nrow(batchJobArrayIndices)) {
  scriptFile = readLines(batchJobTemplate, warn = FALSE)
  
  #JobName
  scriptFile[4]=paste0("#PBS -N GenerateIndices", row)
  #ArrayIndices
  scriptFile[8]=paste0("#PBS -J ", batchJobArrayIndices$startIndex[row], "-", batchJobArrayIndices$endIndex[row])
  
  currentFile = paste0("HPCJobScripts/JobScript", row, ".sh")
  
  charVector <- strsplit(scriptFile,"")
  rawFileContent <- NULL
  for(currentString in charVector) {
    currentString <- unlist(lapply(currentString, charToRaw))
    #convert every char to raw
    rawFileContent <- c(rawFileContent, currentString, charToRaw("\n"))
    #concatenate raw string with \n, to differentiate strings
  }   
  fileConn <- file(currentFile, "wb")
  writeBin(rawFileContent, fileConn)  
  close(fileConn)
}

#generate file to submit jobs to HPC in sequential order with each job waiting until the last one finished before starting
JobSubmission <- character()
JobSubmission[1] <- paste0("FIRST=$(qsub \"NoisyMiner/HPCJobScripts/JobScript1.sh\")")
JobSubmission[2] <- "echo $FIRST"

if (nrow(batchJobArrayIndices) > 1) {
  for (job in rownames(batchJobArrayIndices)[-1]) {
    JobSubmission[(2*as.numeric(job))-1] <- paste0(gsub(" |-", "", toupper(english::ordinal(as.numeric(job)))), "=$(qsub -W depend=afterany:$", gsub(" |-", "", toupper(english::ordinal(as.numeric(job)-1))), " \"NoisyMiner/HPCJobScripts/JobScript", job, ".sh\")")
    JobSubmission[(2*as.numeric(job))] <- paste0("echo $", gsub(" |-", "", toupper(english::ordinal(as.numeric(job)))))
  }
}

writeLines(JobSubmission, paste0("HPCJobScripts/_JobSubmission.txt"))