'plausCalc'<-function(BBA,vect){	#computes pl(vect) vect:binary vector
				
	if(length(vect)!=length(BBA@group[1,])){
		print("arguments are not valid")
		}
	else{
		pl=0
		for (i in 1:length(BBA@bba)){
			SUM=as.numeric(vect&BBA@group[i,])
			if(sum(SUM)!=0 ){
				pl=pl+BBA@bba[i]
				}
			}
		}
	return(pl)	#returns pl(vect)
	}