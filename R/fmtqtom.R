'fmtqtom'=function(m){
if(length(m@q)==1){
	print("error the argument isn't valid. q was not calculated")
	}
else{
	MAT=m@q
	lm=length(MAT)
	natoms=round(log2(lm))
	if(2^natoms==lm){
		if(length(m@bba)==1){
			for (step in 1:natoms){
				i124=2^(step-1)
				i842=2^(natoms+1-step)
				i421=2^(natoms-step)
				MAT=matrix(MAT,ncol=i842,nrow=i124)
				MAT[,((1:i421)*2-1)]=MAT[,((1:i421)*2-1)]-MAT[,((1:i421)*2)]
				}
			M=as.vector(MAT)
			x <- deparse(substitute(m))
				assign(x,ExtendBBA(BBA=M,Q=m@q,Bel=m@bel,Pl=m@pl,B=m@b), pos=.GlobalEnv)
			return(M)
			}
		else{
			return(m@bba)
			}
		}
	else{
		print("Problem in fmtqtom: length of input vector not valid")
		}
	}
}