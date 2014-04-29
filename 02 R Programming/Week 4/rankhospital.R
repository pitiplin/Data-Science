rankhospital <- function(state, outcome, num = "best") {

				## Read outcome data
				data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
								
				## Check that state and outcome are valid
				suboutcome<-sub(" ", "." ,outcome)
				columnname<-paste0("Hospital.30.Day.Death..Mortality..Rates.from.",suboutcome)
				
				if (sum(tolower(data$State)==tolower(state))<1){
								stop(paste0("Error in best(\"",state,"\", \"", outcome, "\") : invalid state\n"))
				}
				else if (sum(tolower(names(data))==tolower(columnname))<1){
								stop(paste0("Error in best(\"",state,"\", \"", outcome, "\") : invalid outcome\n"))
				}
				## Return hospital name in that state with lowest 30-day death
				## rate
				else{
								data<-subset(data, tolower(data$State)==tolower(state))
								data<-data[order(data["Hospital.Name"]),]
								columindex<-match(TRUE, tolower(names(data))==tolower(columnname))
								data[, columindex] <- as.numeric(data[, columindex])								
								data<-data[order(data[columindex]),]
								data<-subset(data, is.na(data[columindex])==F)
								
								ranking<-numeric()
								
								if (num=="best"){
												ranking<-1
								}
								else if (num=="worst"){
												ranking<-nrow(data)
								}
								else{
												ranking<-as.numeric(num)
								}
								
								if (ranking>nrow(data)){
												NA
								}
								else{
												data[ranking,"Hospital.Name"]
								}								
				}	
}