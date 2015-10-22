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
  
   j = 1;
   nobs <- vector("numeric",length = length(id));
  
  for(i in id) {
   filename = paste(directory,"/",sprintf("%03d",i),".csv",sep='');
   data <- read.csv(filename,sep=',',header=TRUE);
   nobs[j] <- sum(complete.cases(data));
  
   j <- j + 1;
  
}
   output <- data.frame(id = id, nobs = nobs)
  }
