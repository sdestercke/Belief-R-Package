'fmtbtoq'=function(m){
if(length(m@b)==1){
	print("error the argument isn't valid. b was not calculated")
	}
else{
	M=m
	M@pl=fmtbtopl(M)
	q=fmtpltoq(M)

	x <- deparse(substitute(m))

	assign(x,ExtendBBA(BBA=m@bba,Q=q,Bel=m@bel,Pl=m@pl,B=m@b), pos=.GlobalEnv)

	return(q)
	}
}