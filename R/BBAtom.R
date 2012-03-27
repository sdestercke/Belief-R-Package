'BBAtom'<-function(BBA){	#transform BBA structure into ExtendBBA
				#depends on binDec and ExtendBBA
	n=length(BBA@group[1,])
	ind=vector('numeric',2^n)

	for(i in 1:length(BBA@bba)){		#decimal coding of sets and create the bba vector
		dec=binDec(BBA@group[i,])
		ind[dec+1]=BBA@bba[i]
		}
	return(ExtendBBA(BBA=ind))	#transform a bba structure into an extensive vector
}