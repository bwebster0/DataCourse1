## rankall function
rankall <- function(outcomeName, num = "best")  {
    ## num can be "best", "worst", or ranking number
    ## if num > # hospitals with data in state, return NA
    
    ## get data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## check for valid inputs
    # heart attack = 11, heart failure = 17, pneumonia = 23
    allOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if (is.na(match(outcomeName,allOutcomes))) { stop("invalid outcome") }
    
    if(outcomeName == "heart attack")
        columnN<-11
    else if (outcomeName == "heart failure")
        columnN<-17
    else if (outcomeName == "pneumonia")
        columnN<-23
    
    ## parse out only the columns we are interested in
    hospAllStates<-data[,2]
    stAllStates<-data[,7]
    ocAllStates<-data[,columnN]
    alllist<-cbind(stAllStates,hospAllStates,ocAllStates)
    
    nalllist<-subset(alllist,alllist[,3]!="Not Available")
    nalllist<-as.data.frame(nalllist)
    
    # set up list of state abbreviations to loop thru
    allStateCodes <- sort(state.abb)
    
    # set up collection struct
    retval<-NULL
    
    # loop thru states, find nth rated hostipal and create pair and add to end of return list
    for (astate in allStateCodes)  {    
        tlist<-subset(nalllist,nalllist[,1]==astate)
    
        if(num=="best") { 
            tnum<-1 
        }
        else if(num=="worst") { 
            tnum<-dim(tlist)[1] 
        }
        else  {
            tnum<-num
        }
        
        nlist<-as.matrix(tlist)
        if (tnum > dim(tlist)[1])  {
            val<-c(NA,astate)
        }
        else  {
            suppressWarnings(hlistSort<-nlist[order(as.double(nlist[,3]),nlist[,2]),])
            # set this state row
            thospName<-as.character(hlistSort[tnum,2])
            val<-c(thospName,astate)
        }
        retval<-rbind(retval,val)
    }
    # prepare return list and return it
    retval<-as.data.frame(retval)
    row.names(retval)<-allStateCodes
    names(retval)<-c("hospital","state")
    retval
}
