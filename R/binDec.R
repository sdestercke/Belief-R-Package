'binDec'<-function(vect){	#encode a binary vector into integers
				#vect=binary vector
k=0;
for(i in 1:length(vect)){
	k=k+vect[i]*2^(i-1)
	}
return(k)
}