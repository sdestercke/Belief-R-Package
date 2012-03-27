'bCalc'<-function(BBA,vect){	#computes b(vect), vect: binary vector
				
	if(length(vect)!=length(BBA@group[1,])){
		print("arguments are not valid")
		}
	else{
		b=0
		for (i in 1:length(BBA@bba)){
			SUM=as.numeric(vect|BBA@group[i,])
			if(sum(SUM==vect)==length(vect)){
				b=b+BBA@bba[i]
				}
			}
		}
	return(b)
	}