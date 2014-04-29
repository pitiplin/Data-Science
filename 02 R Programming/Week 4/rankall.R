rankall <- function(outcome, num = "best") {

				## Read outcome data
				data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
								
				## Check that state and outcome are valid
				suboutcome<-sub(" ", "." ,outcome)
				columnname<-paste0("Hospital.30.Day.Death..Mortality..Rates.from.",suboutcome)
				
				if (sum(tolower(names(data))==tolower(columnname))<1){
								stop(paste0("Error in rankall(\"", outcome, "\") : invalid outcome\n"))
				}
				
				## Return hospital name in that state with lowest 30-day death
				## rate
				else{
								states<-subset(data, duplicated(data$State)==F)
								states<-states$State
								states<-sort(states)
								
								n <- length(states)
								hospital <- character(n)
								state <- character(n)
								
								for (i in 1:n){
				
									dataAux<-subset(data, data$State==states[i])
									dataAux<-dataAux[order(dataAux["Hospital.Name"]),]
									columindex<-match(TRUE, tolower(names(dataAux))==tolower(columnname))
									dataAux[, columindex] <- as.numeric(dataAux[, columindex])								
									dataAux<-dataAux[order(dataAux[columindex]),]
									dataAux<-subset(dataAux, is.na(dataAux[columindex])==F)
									
									ranking<-numeric()
									
									if (num=="best"){
													ranking<-1
									}
									else if (num=="worst"){
													ranking<-nrow(dataAux)
									}
									else{
													ranking<-as.numeric(num)
									}
									
									state[i] <- states[i]
									
									if (ranking>nrow(dataAux)){
													hospital[i] <- NA
									}
									else{
													hospital[i] <- dataAux[ranking,"Hospital.Name"]
									}
							}
							
							df = data.frame(hospital,state)
				}	
}