'qCalc'<-function(BBA,vect){	#computes q(vect) vect:binary vector
				
	if(length(vect)!=length(BBA@group[1,])){
		print("arguments are not valid")
		}
	else{
		q=0
		for (i in 1:length(BBA@bba)){
			SUM=as.numeric(BBA@group[i,]|vect)
			if(sum(SUM==BBA@group[i,])==length(vect)){
				q=q+BBA@bba[i]
				}
			}
		}
	return(q)	#returns q(vect)
	}