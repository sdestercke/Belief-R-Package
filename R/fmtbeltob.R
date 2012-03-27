'fmtbeltob'=function(m){
if(length(m@bel)==1){
	print("error the argument isn't valid. bel was not calculated")
	}
else{
	BEL=m
	bel=BEL@bel
	lm=length(bel)
	natoms=round(log2(lm))
	if(2^natoms==lm){
		if(length(m@b)==1){
			b=BEL@bel
			x <- deparse(substitute(m))
			assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=m@bel,Pl=m@pl,B=b), pos=.GlobalEnv)
			return(b)
			}
		else{
			return(m@b)
			}
		}
	else{
		print("Problem in fmtbeltob: length of input vector not valid")	
		}
	}
}