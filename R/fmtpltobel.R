'fmtpltobel'=function(m){
if(length(m@pl)==1){
	print("error the argument isn't valid. pl was not calculated")
	}
else{
	pl=m@pl
	lm=length(pl)
	natoms=round(log2(lm))
	if(2^natoms==lm){
		if(length(m@bel)==1){
			fmt=fmtpltob(m)
			if(fmt[1]<1){
				fmt=fmt/(1-fmt[1])
				}
			fmt[1]=0
			bel=fmt
			x <- deparse(substitute(m))
			assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=bel,Pl=m@pl,B=m@b), pos=.GlobalEnv)
			return(bel)
			}
		else{
			return(m@bel)
			}
		}
		else{
		print("Problem in fmtpltobel: length of input vector not valid")	
		}
	}
}