corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
       
       source("complete.R");
       complete <- complete("specdata"); ##find completed case
       checkid  <-complete$id[which(complete$nobs >= threshold)]
       corr <- vector("numeric",length = length(checkid));
       files_full <- list.files(directory,full.names=TRUE);
   

   for(i in seq_along(checkid)) {
        tmp <- read.csv(files_full[[checkid[i]]]); 
        corr[i] <- cor(na.omit(tmp[,c("sulfate","nitrate")]))[1,2];  
     }

     corr
