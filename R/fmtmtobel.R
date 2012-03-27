'fmtmtobel'=function(m){
if(length(m@bbba)==1){
	print("error the argument isn't valid. m was not calculated")
	}
else{
	bel=fmtmtob(fmtmtonm(m))

	x <- deparse(substitute(m))

	assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=bel,Pl=m@pl,B=m@b), pos=.GlobalEnv)

	return(bel)
	}
}