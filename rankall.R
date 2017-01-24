rankall <- function(outcome, num = "best") {
  
  files<-read.csv("outcome-of-care-measures.csv")
  #reading from .csv file
  
  outcomesa<-c("heart attack","heart failure", "pneumonia")
  #possible outcomes that will be entered
  
  states<-levels(files[,7])
  #getting vector of state names
  
  #checking for valid state and outcomes using %in%
  # is.element can also be used
  
  if (!(outcome %in% outcomesa)) {stop("Invalid outcome")}
  
  if(is.numeric(num)) {
    if (num > length(files[,2])) {return (NA)}
  }
  #finding column number of the selected outcome
  
  outcol<- if(outcome == outcomesa[1]) {11}
           else if (outcome == outcomesa[2]) {17}
           else {23} 

##################################################
  #coercing names to character and outcomes to numeric
  
  files[,2] <-as.character(files[,2])
  files[,outcol] <- suppressWarnings(as.numeric(levels(files[, outcol])[files[, outcol]]))
  #turning all contents of this column to numeric including NAs
 
  # creating an empty data frame to store rankings for each state
  rankofall <- vector()
 
  for (i in 1:length(states)) {
   sfiles <- subset(files, State == states[i])
   
   #ordering hospitals for selected subset of state dataset
   orderst <- sfiles[order(sfiles[,outcol],sfiles[,2],na.last = NA),]
   
   #selecting the hospital name based on specified rank
   rankedh <- if(num == "best") {orderst[1,2]}
              else if(num == "worst") {orderst[nrow(orderst),2]}
              else {orderst[num,2]}
   
   rankofall <- append(rankofall,c(rankedh,states[i]))
  }
 
 rankofall <- as.data.frame(matrix(rankofall,length(states),2,byrow=TRUE))
 colnames(rankofall) <- c("hospital","state")
 rownames(rankofall) <- states
 
 rankofall
 
}