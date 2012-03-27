'fmtqtopl'=function(m){
if(length(m@q)==1){
	print("error the argument isn't valid. q was not calculated")
	}
else{
	Q=m
	Q@q[1]=0
	Q@b=Q@q
	Q@bba=0
	pl=abs(fmtbtom(Q))
	x <- deparse(substitute(m))
		assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=m@bel,Pl=pl,B=m@b), pos=.GlobalEnv)
		return(pl)
	}
}