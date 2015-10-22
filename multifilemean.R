pollutantmean <- function(directory,pollutant,id = 1:332) {

##directory indicating the location of the CSV files
## pollutant name of pollutant for which we calculate the mean


 files_full <- list.files(directory,full.names=TRUE);
 dat <- data.frame()

for(i in id) {
  dat  <- rbind(dat,read.csv(files_full[i], sep = ',', header = TRUE))
}
   
   mean <- mean(dat[[pollutant]],na.rm = TRUE);
}
