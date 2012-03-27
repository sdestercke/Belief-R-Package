'fmtmtopl'=function(m){
if(length(m@bba)==1){
	print("error the argument isn't valid. m was not calculated")
	}
else{
	M=m
	M@b=fmtmtob(m)
	pl=fmtbtopl(M)
	x <- deparse(substitute(m))

	assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=m@bel,Pl=pl,B=m@b), pos=.GlobalEnv)

	return(pl)
	}
}