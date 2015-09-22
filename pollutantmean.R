pollutantmean <- function(directory, pollutant, id=1:332 ) {
  finalFilename <- c()
  #print(directory) 
  #print(pollutant)
  #print(id)
  #dataFile <- read.csv(directory/id)
  fileList = list.files(directory)
  
  filenames = as.numeric(sub("\\.csv$","",fileList))
 # print(filenames)
  selectedfiles = fileList[match(id,filenames)]
  selectedfiles = paste(directory , selectedfiles ,sep = "/")
 # print( selectedfiles )
 dataFile <- lapply(selectedfiles,read.csv)
  
  # print(dataFile)
   combinedFile = do.call(rbind,dataFile)
   #print(combinedFile)
  #loadData <- do.call(rbind,dataFile)
  #print(loadData)
  meanValue <- mean(combinedFile[[pollutant]],na.rm = TRUE)
  
  round(meanValue , digits = 3)
}