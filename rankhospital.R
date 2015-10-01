rankhospital<- function(state,outcome , num = "best"){
  library("stringi")
  
  outcome <- stri_trans_totitle(outcome)
  dataFile <- read.csv("outcome-of-care-measures.csv" , colClasses = "character")
  
  outcomeV <- gsub(" ",".",outcome)
  #print(outcomeV)
  columnName<- paste("Hospital.30.Day.Death..Mortality..Rates.from",outcomeV,sep=".")
  #print(columnName)
  
  dataFrame <- subset(dataFile,dataFile$State == state)
  if(nrow(dataFrame) == 0)
    stop("invalid state")
  
  namesDataFrame <- names(dataFrame)
  #print(namesDataFrame)
  isMatch<- match(columnName, as.vector( namesDataFrame))
  
  if( is.na( as.numeric(isMatch) ))
    stop("invalid outcome")
  
  #print(isMatch)
  #print(dataFrame[[columnName]])
  # dataFrameNoNA <- dataFrame[complete.cases(dataFrame),]
  #print(dataFrameNoNA)
  #minValue <- min(suppressWarnings(as.numeric((as.character(dataFrame[[columnName]])))) ,na.rm = TRUE)
  sortedData <- dataFrame[order(suppressWarnings(as.numeric(dataFrame[[columnName]])),dataFrame$Hospital.Name,na.last = NA),]
  
  rankReq <- 1
   
   if(!is.na(suppressWarnings( as.numeric(num))))
       {
          rankReq <- num
          if(num> nrow(dataFrame))
            return 
      }
      else if (num=="best")
       rankReq<- 1
      else if(num=="worst")
       rankReq <-nrow(sortedData)
                                  
  
  x<- sortedData[rankReq ,]
  
  as.vector(x$Hospital.Name) 

}