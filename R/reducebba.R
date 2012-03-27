'reducebba'=function(BBA){	#takes a BBA as argument
				#depends: mtoBBA and ExtendBBA
if(length(BBA@bba)==1){
	return(BBA)
	}
else{
	k=length(BBA@bba)
	n=length(BBA@group[1,])
	BBAr=vector('numeric',2^n)
	for(i in 1:k){
		p=binDec(BBA@group[i,])
		BBAr[p+1]=BBAr[p+1]+BBA@bba[i]
		}
	BBAr=mtoBBA(ExtendBBA(BBA=BBAr))	#returns the reduced BBA
	return(BBAr)
	}
}