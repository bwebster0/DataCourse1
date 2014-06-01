# best function
best <- function(stateCode, outcomeName)  {
    ## get data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## check for valid inputs
    allStateCodes <- state.abb
    if (is.na(match(stateCode,allStateCodes))) { stop("invalid state") }
    # heart attack = 11, heart failure = 17, pneumonia = 23
    allOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if (is.na(match(outcomeName,allOutcomes))) { stop("invalid outcome") }
    
    if(outcomeName == "heart attack")
        columnN<-11
    else if (outcomeName == "heart failure")
        columnN<-17
    else if (outcomeName == "pneumonia")
        columnN<-23
    
    ## Return Hosp name with lowest 30-day death rate
    ## parse out only the columns we are interested in, and sort by outcomeName and then Hospital name
    hosp<-subset(data[,2],data$State==stateCode)
    st<-subset(data[,7],data$State==stateCode)
    oc<-subset(data[,columnN],data$State==stateCode)
    hlist<-cbind(st,hosp,oc)
    suppressWarnings(hlistSort<-hlist[order(as.numeric(oc),hosp),])
    
    # return hospital name
    retval<-hlistSort[1,2]
    names(retval)<-NULL
    retval
}