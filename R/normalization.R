'normalization'<-function(BBA){	#BBA list with bba and group(see definitionbba)
				#depends: BBA
SUM=apply(BBA@group,1,sum)	#search for empty set position (0 if not present)
ind=0
for (i in 1:length(SUM)){
	if(SUM[i]==0){
		ind=i
		}
	}

if(ind!=0){			#normalization
	val=BBA@bba[ind]
	result=BBA@bba/(1-val)
	result=result[-ind]
	return(BBA(Bba=result,Group=matrix(BBA@group[-ind,],nrow=length(BBA@bba)-1)))

	}
else{
	return(BBA)	#normalized bba
	}
}