'fmtqtob'=function(m){

if(length(m@q)==1){
	print("error the argument isn't valid. q was not calculated")
	}
else{
	Q=m
	Q@pl=fmtqtopl(Q)
	b=fmtpltob(Q)
	x <- deparse(substitute(m))
			assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=m@bel,Pl=m@pl,B=b), pos=.GlobalEnv)
			return(b)
	}
}