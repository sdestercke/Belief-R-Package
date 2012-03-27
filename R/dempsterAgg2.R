'dempsterAgg2'=function(BBA1,BBA2){	#BBA1,BBA2 ate two BBA structures
					#Dempster fusion of two BBA structures
					#returns a BBA
					#uses : binDec and mtoBBA
List1=BBA1@group
List2=BBA2@group
List3=BBA1@bba
List4=BBA2@bba

n=length(List1[1,])
List5=List1 %*% t(List2)
dim=dim(List5)
dec=vector('numeric',2^n)
for(i in 1:dim[1]){
	for(j in 1:dim[2]){
		if(List5[i,j]!=0){
			c=as.numeric(List1[i,]&List2[j,])
			d=binDec(c)
			b=List3[i]*List4[j]
			dec[d+1]=dec[d+1]+b
			}
		}
	}
sum=sum(dec)
dec[1]=1-sum
Ag=mtoBBA(ExtendBBA(BBA=dec))
return(Ag)		#Dempster aggregation of two elements
}