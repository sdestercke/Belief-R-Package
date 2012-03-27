'fmtqtobel'=function(m){
if(length(m@q)==1){
	print("error the argument isn't valid. q was not calculated")
	}
else{
	Q=m
	Q@b=fmtqtob(Q)
	bel=fmtbtobel(Q)
	x <- deparse(substitute(m))
			assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=bel,Pl=m@pl,B=m@b), pos=.GlobalEnv)
			return(bel)
	}
}