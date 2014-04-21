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
  
  values <- data.frame()
   
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
    
    ok <- complete.cases(data)
    
    completes <- sum(ok)
    
    values <- rbind(values, c(i, completes))
  }
  
  names(values) <- c("id", "nobs")
  
  values
}