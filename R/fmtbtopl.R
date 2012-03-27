'fmtbtopl'=function(m){
if(length(m@b)==1){
	print("error the argument isn't valid. b was not calculated")
	}
else{
	lm=length(m@b)
	natoms=round(log2(lm))
	if(2^natoms==lm){
		if(length(m@pl)==1){
			fmt=m@b[lm:1]
			K=vector('numeric',lm)
			K[]=m@b[lm]
			FMT=K-fmt
			FMT[1]=0
			x <- deparse(substitute(m))
		  	assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=m@bel,Pl=FMT,B=m@b), pos=.GlobalEnv)
			return(FMT)
			}
		else{
			return(m@pl)
			}
		}
	else{
		print("problem in fmtbtopl: length of input vector not valid")
		}
	}
}