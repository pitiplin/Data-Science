best <- function(state, outcome) {

				## Read outcome data
				data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
								
				## Check that state and outcome are valid
				suboutcome<-sub(" ", "." ,outcome)
				columnname<-paste0("Hospital.30.Day.Death..Mortality..Rates.from.",suboutcome)
				
				if (sum(tolower(data$State)==tolower(state))<1){
								stop(paste0("Error in best(\"",state,"\", \"", outcome, "\") : invalid state\n"))
								#cat(paste0("Error in best(\"",state,"\", \"", outcome, "\") : invalid state\n"))
				}
				else if (sum(tolower(names(data))==tolower(columnname))<1){
								stop(paste0("Error in best(\"",state,"\", \"", outcome, "\") : invalid outcome\n"))
								#cat(paste0("Error in best(\"",state,"\", \"", outcome, "\") : invalid outcome\n"))
				}
				## Return hospital name in that state with lowest 30-day death
				## rate
				else{
								data<-subset(data, tolower(data$State)==tolower(state))
								columindex<-match(TRUE, tolower(names(data))==tolower(columnname))
								data[, columindex] <- as.numeric(data[, columindex])								
								data<-data[order(data[columindex]),]								
								data<-subset(data, data[columindex]==data[1,columindex])
								data<-data[order(data["Hospital.Name"]),]
								data[1,"Hospital.Name"]
				}	
}