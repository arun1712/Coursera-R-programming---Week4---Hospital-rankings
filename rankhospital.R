rankhospital<-function(state,outcome,num="best"){
  #### storing data in data frames 
  
  fi<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  #reading data from CSV
  
  df<-as.data.frame(cbind(fi[,2], fi[,7],fi[,11],fi[,17],fi[,23]),stringsAsFactors = FALSE)
  df<-df[complete.cases(df),]
  #creating a data frame with the columns of interest
  #hospital name, state, heart attak, heart failure, pnuemonia
  #default behavior in R is to coerce strings in data frame to factors. Setting as False.
  
  colnames(df)<-c("name","state","heart attack","heart failure","pneumonia")
  #setting column names for easy access and comparison to input
  
  ############################################
  
  #check for valid state and outcome names
  
  if(!(state %in% df[,"state"])) {stop("Invalid state")}
  
  else if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {stop("Invalid outcome")}
  
  #########
  
  #if numeric value is entered for best
  
  if(is.numeric(num)) {
    
    if (num < length(df[,2])) {
    
      slr<- which(df[,"state"]==state)
     # slr <- slr[complete.cases(slr),]
    
      #extracting row numbers with specified state;;; slr -- state list rows
      #calling which gives row numbers where specified state is
    
      sl<- df[slr,]
      #data frame with specifed state only
    
      #using eval function to match input outcome with the one in data frame
      sl[,eval(outcome)] <- as.numeric(sl[,eval(outcome)])
      #coercing to numeric since colclass was set to character
    
      sl<- sl[order(sl[,eval(outcome)],sl[,"name"]),]
      #sorting hospital names for specified outcome in ascending order
    
      bhos<- sl[,"name"][num]
    }
    else {return(NA)}
  }
  
  if (num == "best") {bhos<-best(state,outcome)}
  
  if (num == "worst") {
    
    slr<- which(df[,"state"]==state)
   # slr <- slr[complete.cases(slr),]
    
    #extracting row numbers with specified state;;; slr -- state list rows
    #calling which gives row numbers where specified state is
    
    sl<- df[slr,]
    #data frame with specifed state only
    
    #using eval function to match input outcome with the one in data frame
    sl[,eval(outcome)] <- as.numeric(sl[,eval(outcome)])
    #coercing to numeric since colclass was set to character
    
    sl<- sl[order(sl[,eval(outcome)],sl[,"name"],decreasing = TRUE),]
    #sorting hospital names for specified outcome in ascending order
    
    bhos<- sl[,"name"][1]
  }
  suppressWarnings(return(bhos))
}