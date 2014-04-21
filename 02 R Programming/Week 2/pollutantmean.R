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
  
  values <- numeric(0)  
  
  for (i in id){
    if (i<10){
      csvFile <- paste0("./",directory,"/","00", i,".csv")
    }
    else if (i<100){
      csvFile <- paste0("./",directory,"/","0", i,".csv")
    }
    else{
      csvFile <- paste0("./",directory,"/",i,".csv")
    }
 
    data<-read.csv(csvFile)
    
    if (pollutant=="sulfate"){
      valuesFile <- data$sulf      
    }
    else{
      valuesFile <- data$nitr      
    }
    
    values<-c(values, valuesFile)
    
  }
  
  mean <- mean(values, na.rm = TRUE)
}