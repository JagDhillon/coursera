best <- function(state, outcome) {
	## Read outcome data
	oc<-read.csv("outcome-of-care-measures.csv")

	## Check that state and outcome are valid
	if( outcome=="heart attack"){
		oname<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	} else if( outcome=="heart failure"){
		oname<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	} else if( outcome=="pneumonia"){
		oname<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	} else {
		stop("invalid outcome")
	}

	ocs<-oc[oc$State==state,]
	if( nrow(ocs) <=0) {
		stop("invalid state")
	}

	ocs[oname] <- unlist( lapply(ocs[,oname], as.character) )
	ocs[oname] <- unlist( lapply(ocs[,oname], as.numeric) )
	ocs <- ocs[complete.cases(ocs[,oname]),]
	as.character(ocs[order( ocs[oname], ocs$Hospital.Name),][1,"Hospital.Name"])
}