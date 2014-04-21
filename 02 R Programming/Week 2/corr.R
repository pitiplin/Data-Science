corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  values <- numeric(0)
  
  id<-c(1:332)
  
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
    
    if (sum(ok)>=threshold){      
      values <- c(values, cor(data$sul[ok], data$nit[ok]))      
    }    
        
  }
  
  values
}