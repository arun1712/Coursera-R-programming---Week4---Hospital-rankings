best<-function(state,outcome) {
  
  files<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  #reading from .csv file
  
  statesd<-unique(files[,7])
  #getting names of states without repetition
  
  outcomesa<-c("heart attack","heart failure", "pneumonia")
  #possible outcomes that will be entered
  
  #checking for valid state and outcomes using %in%
  # is.element can also be used
  
    if (!(state %in% statesd)) {stop("Invalid state")}
    else if (!(outcome %in% outcomesa)) {stop("Invalid outcome")}
  
  #finding column number of the selected outcome
  
  outcol<- if(outcome == outcomesa[1]) {11}
  else if (outcome == outcomesa[2]) {17}
  else {23}
  
  # Creating database of selected outcome and state for finding best
  sfiles<- subset(files, State ==state,na.rm=TRUE)
  
  #selecting required columns
  
  outcomefiles<- suppressWarnings(as.numeric(sfiles[,outcol]))
  #all data is stored as characters; converting to numeric
  sfiles<- sfiles[!(is.na(outcomefiles)),]
  
  outcomefiles<-as.numeric(sfiles[,outcol])
  #finding best hospital wit lowest mortality rate
  
  besthosprow<- which(outcomefiles == min(outcomefiles))
  
  #get hospital name based on slected row
  
  outbesthosp<- sfiles[besthosprow,2]
  
  sortedlist<- sort(outbesthosp)
  
  return(outbesthosp)
}