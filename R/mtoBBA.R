'mtoBBA'<-function(M){	#transform a BBA into a reduced structure
m=M			#depends: decBin
s=length(m@bba)		
n=log2(s)
p=round(n)

if(2^p==s){
	j=1
	M=1:n
	groupe=1:n
	for(i in 1:length(m@bba)){
		if(m@bba[i]!=0){	#detect a mass different from 0
			M[j]=m@bba[i]	#add that mass
			j=j+1
			groupe=rbind(groupe,decBin(i-1,n))
			#add corresponding binary coded element
			}}
	M=M[1:j-1]	#select only wanted elements
	groupe=groupe[-1,]
	bba=BBA(Group=groupe,Bba=M)
	return(bba)	#transform complete vector into bba

}
else{
	print("input vector length not valid")
	}
}