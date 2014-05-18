pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    n<-length(id)
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
        specdata<-subset(data,TRUE,pollutant)
        specdataF<-specdata[!is.na(specdata)]
        allspecdata<-c(allspecdata,specdataF)
    }
    mean(allspecdata)
}