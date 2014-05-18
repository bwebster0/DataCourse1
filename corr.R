corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    allfiles<-dir(directory)
    out<-numeric(length = 0)
    for (i in 1:length(allfiles))  {
        cfile<-paste(directory, allfiles[i], sep="\\")
        n<-complete(directory,i)
        if (n[2] > threshold)  {
            data<-read.csv(cfile)
            cdata<-subset(data,TRUE,c(sulfate, nitrate))
            res<-cor(cdata[,1],cdata[,2],use = "pairwise.complete.obs")
            out<-c(out,round(res,5))
        }
    }
    out
}