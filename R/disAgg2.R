'disAgg2'=function(BBA1,BBA2){	#BBA1,BBA2 two bba structures
				# to be merged
				#depends: binDec, mtoBBA

List1=BBA1@group
List2=BBA2@group
List3=BBA1@bba
List4=BBA2@bba

n=length(List1[1,])

n1=length(List1[,1])
n2=length(List2[,2])

vect=vector('numeric',2^n)

for(i in 1:n1){
	for(j in 1:n2){
		k=binDec(as.numeric(List1[i,]|List2[j,]))
		vect[k+1]=vect[k+1]+List3[i]*List4[j]
		}
	}
bba=mtoBBA(ExtendBBA(BBA=vect))
return(bba)		# disjunctive aggregation of two elements
}