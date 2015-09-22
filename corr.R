corr <- function(directory, threshold = 0) {
  finalFilename <- c()
  #print(directory) 
  #print(pollutant)
  #print(id)
  #dataFile <- read.csv(directory/id)
  fileList = list.files(directory)
  
 #print(fileList)
  selectedfiles <- paste(directory,fileList,sep = "/")
  #print(selectedfiles)
  vectorCorelation <-c()
  #nrow(combinedFile)
  # complete.cases(combinedFile)
  for(i in seq_along(selectedfiles))
  {
    # print(selectedfiles[i])
    df <- read.csv(selectedfiles[i])
    completeDf <- subset(df , complete.cases(df))
   # print(completeDf)
    finalDf <- subset(completeDf,nrow(completeDf) > threshold)
   # print(finalDf)
    #a <- nrow(finalDf) == 0
    if(nrow(finalDf) > 0)
    {
      #print(finalDf)
      corelation <- cor(finalDf$nitrate , finalDf$sulfate)
      vectorCorelation <- c(vectorCorelation , corelation)
    }
    
  }
  
  vectorCorelation
}
