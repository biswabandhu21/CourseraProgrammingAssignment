complete <- function(directory, id = 1:332) {
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
 # dataFile <- lapply(selectedfiles,read.csv)
  
  # print(dataFile)
  #combinedFile = do.call(rbind,dataFile)
  #print(combinedFile)
  nobs <-c()
  #nrow(combinedFile)
 # complete.cases(combinedFile)
  for(i in seq_along(selectedfiles))
  {
   # print(selectedfiles[i])
    df <- read.csv(selectedfiles[i])
    #print(df)
    countR <-nrow( subset(df,complete.cases(df)))
    #print(countR)
    nobs <- c(nobs,countR)
  }
  
 # print(id)
  #print(nobs)
  data.frame(id,nobs)
}
