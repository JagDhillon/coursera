rankall <- function(outcome, rank = "best") {
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

	ocs<-oc


	ocs[oname] <- unlist( lapply(ocs[,oname], as.character) )
	ocs[oname] <- unlist( lapply(ocs[,oname], as.numeric) )
	ocs <- ocs[complete.cases(ocs[,oname]),]
	ocs<-ocs[order(ocs[oname]),]

	if(rank=="best") {
		rank<-1
	}

	g <- split(ocs$Hospital.Name, ocs$State)
	l <- levels(ocs$State)

	r <- data.frame(matrix(nrow=0,ncol=2))
	colnames(r) <- c("hospital", "state")

	for( lev in levels(ocs$State)) {
		tr <- data.frame(hospital=as.character(g[[lev]][rank]), state=lev)
		r<-rbind( r, tr)
	}

	r
}