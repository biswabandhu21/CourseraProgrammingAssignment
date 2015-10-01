rankhospitalByState<- function(state,columnName , num = "best" ,dataFile){
  
  #dataFile <- read.csv("outcome-of-care-measures.csv" , colClasses = "character")
  dataFrame <- subset(dataFile,dataFile$State == state)
  
  
  #print(namesDataFrame)
  

  #print(isMatch)
  #print(dataFrame[[columnName]])
  # dataFrameNoNA <- dataFrame[complete.cases(dataFrame),]
  #print(dataFrameNoNA)
  #minValue <- min(suppressWarnings(as.numeric((as.character(dataFrame[[columnName]])))) ,na.rm = TRUE)
  sortedData <- dataFrame[order(suppressWarnings(as.numeric(dataFrame[[columnName]])),dataFrame$Hospital.Name,na.last = NA),]
  
  rankReq <- 1
  retValue <- data.frame()
  if(!is.na(suppressWarnings( as.numeric(num))))
  {
    rankReq <- num
    if(num> nrow(dataFrame))
      retValue<- data.frame( c("NA") ,c(state) )
  }
  else if (num=="best")
    rankReq<- 1
  else if(num=="worst")
    rankReq <-nrow(sortedData)
  
  
  x<- sortedData[rankReq ,]
  
  retValue <- data.frame(c(x$Hospital.Name) ,c(state)) 
  
  retValue
}

rankall <- function(outcome , num="best")
{
  library("stringi")
  
  outcome <- stri_trans_totitle(outcome)
  dataFile <- read.csv("outcome-of-care-measures.csv" , colClasses = "character")
  
  outcomeV <- gsub(" ",".",outcome)
  #print(outcomeV)
  columnName<- paste("Hospital.30.Day.Death..Mortality..Rates.from",outcomeV,sep=".")
  #print(columnName)
  namesDataFrame <- names(dataFile)
  isMatch<- match(columnName, as.vector( namesDataFrame))
  
  if( is.na( as.numeric(isMatch) ))
    stop("invalid outcome")
  dataFile <- read.csv("outcome-of-care-measures.csv" , colClasses = "character")
  stateList <- sort(unique(dataFile$State))
 # print(columnName)
  
 result<- lapply(stateList, rankhospitalByState , columnName,num , dataFile)
 # print(result)
  
  X<- do.call(rbind, result)
   
   colnames(X) <- c("hospital", "state")
  X
}