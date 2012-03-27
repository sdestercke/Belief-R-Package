'pignisticCalc'<-function(BBA){	#BBA is a BBA structure
				#returns pignistic probabilities
				#depends: normalization and fromPtoBBAP
	M=normalization(BBA)
	n=length(M@group[1,])
	Pigni=vector('numeric',n)
	for(i in 1:length(M@bba)){
		c=M@group[i,]
		m=M@bba[i]/sum(c)
		c=c*m
		Pigni=Pigni+c
		}
		return(fromPtoBBAP(Pigni))	#computes the pignistic function
	
}