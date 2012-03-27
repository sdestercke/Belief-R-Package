'fmtpltom'=function(m){
if(length(m@pl)==1){
	print("error the argument isn't valid. pl was not calculated")
	}
else{
	PL=m
	PL@b=fmtpltob(PL)
	M=fmtbtom(PL)

	x <- deparse(substitute(m))

	assign(x,ExtendBBA(BBA=M,Q=m@q,Bel=m@bel,Pl=m@pl,B=m@b), pos=.GlobalEnv)

	return(M)
	}
}