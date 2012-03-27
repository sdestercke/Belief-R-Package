'discounting'=function(BBA,alpha){	#BBA= BBA structure, alpha in [0,1]
					#Computes BBA weakening

if(alpha<0 || alpha>1){
	print("arguments are not OK")
	}
else{
	boolean=FALSE
	num=0					#omega
	TOT=length(BBA@group[1,])
	for(i in 1:length(BBA@bba)){
		if(sum(BBA@group[i,])==TOT){	#checks if omega is present
			boolean=TRUE
			num=i
			break
			}
		}
	if(boolean==FALSE){			# if omega is not present, add it to BBA object with null mass
		V=sample(1:1,TOT,TRUE)
		BBA@group=rbind(BBA@group,V)
		BBA@bba=c(BBA@bba,0)
		num=length(BBA@bba)
		}
	BBA@bba=BBA@bba*alpha
	BBA@bba[num]=BBA@bba[num]+(1-alpha)	#computes belief masses of focal elements
	return(BBA)
	}
}