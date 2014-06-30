best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #print(names(outcomeData))
  #stop("XO")
  
  ## Check that state and outcome are valid
  flagSt <- outcomeData["State"] == state
  if(sum(flagSt) == 0) stop("invalid state")
  
  outcomeTypes <- pairlist()
  outcomeTypes["heart attack"]  <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  outcomeTypes["heart failure"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  outcomeTypes["pneumonia"]     <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  if(is.na(outcomeTypes[outcome]))  stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcomeCol <- outcomeTypes[outcome]
  outcomeData <- subset(outcomeData, State == state, select = c("Hospital.Name", outcomeCol))
  outcomeData[,outcomeCol] <- suppressWarnings(as.numeric(outcomeData[,outcomeCol]))
  outcomeData <- outcomeData[ complete.cases(outcomeData), ]
  bestRes <- apply(outcomeData[outcomeCol], 2, min)
  
  outcomeData[ outcomeData[outcomeCol] == bestRes[1] ][1]
}

#print(best("TX", "heart attack"))
#print(best("TX", "heart failure"))
#print(best("MD", "heart attack"))
#print(best("MD", "pneumonia"))
#print(best("BB", "heart attack"))
#print(best("NY", "hert attack"))
