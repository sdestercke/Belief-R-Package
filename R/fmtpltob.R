'fmtpltob'=function(m){
if(length(m@pl)==1){
	print("error the argument isn't valid. pl was not calculated")
	}
else{
	PL=m
	pl=PL@pl
	lm=length(pl)
	natoms=round(log2(lm))
	if(2^natoms==lm){
		if(length(m@b)==1){
			fmt=pl[lm:1]
			K=vector('numeric',lm)
			K[]=1
			FMT=K-fmt
			b=FMT
			x <- deparse(substitute(m))
			assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=m@bel,Pl=m@pl,B=b), pos=.GlobalEnv)
			return(b)
			}
		else{
			return(m@b)
			}
		}
	else{
		print("Problem in fmtpltob: length of input vector not valid")
		}
	}
}