'fmtpltoq'=function(m){
if(length(m@pl)==1){
	print("error the argument isn't valid. pl was not calculated")
	}
else{
	PL=m
	PL@b=PL@pl
	PL@bba=0
	fmt=fmtbtom(PL)
	fmt=abs(fmt)
	fmt[1]=1
	x <- deparse(substitute(m))
	assign(x,ExtendBBA(BBA=m@bba,Q=fmt,Bel=m@bel,Pl=m@pl,B=m@b), pos=.GlobalEnv)
	return(fmt)
	}
}