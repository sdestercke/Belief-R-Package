'belCalc'<-function(BBA,vect){		#computes belief from vect: binary vector
						
	if(length(vect)!=length(BBA@group[1,])){
		print("arguments are not OK")
		}
	else{
		bel=0
		for (i in 1:length(BBA@bba)){
			SUM=as.numeric(vect|BBA@group[i,])
			if(sum(SUM==vect)==length(vect) && sum(BBA@group[i,])!=0){
				bel=bel+BBA@bba[i]
				}
			}
		}
	return(bel)	#returns bel(vect)
	}