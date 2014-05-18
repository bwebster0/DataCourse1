complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    n<-length(id)
    nobs<-NULL
    for (i in 1:n)  {
        tid<-id[i]
        if(tid < 100)  {
            if (tid < 10) {
                tid <- paste("00",tid,sep="")
            }
            else  {
                tid <- paste("0",tid,sep="")
            }
        }
        file<-paste(tid,".csv",sep="")
        fname<-paste(directory,file,sep="\\")
        
        data<-read.csv(fname)
        data2<-subset(data,!is.na(sulfate) & !is.na(nitrate),c(sulfate,nitrate))
        
        nobs<-c(nobs,nrow(data2))
    }
    outp<-cbind(id,nobs)
    names(outp)<-(c("id","nobs"))
    outdf<-as.data.frame(outp)
    ##outt<-sprintf("##   ids nobs\n")
    ##for (i in 1:nrow(outp)) {
    ##    outt<-paste(outt,sprintf("## %d %d %d",i,outp[i,1],outp[i,2]),sep="\n")
    ##}
    ##outt
}