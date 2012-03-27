'decBin'<-function(nb,n){	#encode an integer into a binary representation
				#nb = integer, n number of bits to use
v=nb;			

if(nb>=2^n){
	print("arguments are not valid")
	}
else{
	bin=vector('numeric',n)
	i=1
	while(v!=0){
		p=v%/%2
		if(2*p==v){
			bin[i]=0
			}
		else{
			bin[i]=1
			}
		v=p
		i=i+1
		}
	return(bin)	#return binary encoding
	}
}