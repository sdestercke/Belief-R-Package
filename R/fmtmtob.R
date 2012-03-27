'fmtmtob'=function(m){
if(length(m@bba)==1){
	print("error the argument isn't valid. m was not calculated")
	}
else{
	MAT=m@bba
	lm=length(m@bba)
	natoms=round(log2(lm))
	if(2^natoms==lm){
		if(length(m@b)==1){
			for(step in 1:natoms){
				i124=2^(step-1)
				i842=2^(natoms+1-step)
				i421=2^(natoms-step)
				MAT=matrix(MAT,ncol=i842,nrow=i124)
				MAT[,((1:i421)*2)]=MAT[,((1:i421)*2)]+MAT[,((1:i421)*2-1)]
				}
			x <- deparse(substitute(m))
		  	assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=m@bel,Pl=m@pl,B=as.vector(MAT)), pos=.GlobalEnv)
			return(as.vector(MAT))
			}
		else{
			return(m@b)
			}
		}
	else{
		print("Problem in fmtmtob: length of input vector not valid")
		}
	}
}