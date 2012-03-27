'fmtbeltom'=function(m){
if(length(m@bel)==1){
	print("error the argument isn't valid. bel was not calculated")
	}
else{
	BEL=m
	BEL@b=BEL@bel
	M=fmtbtom(BEL)
	x<-deparse(substitute(m))
	assign(x,ExtendBBA(BBA=M,Q=m@q,Bel=m@bel,Pl=m@pl,B=m@b), pos=.GlobalEnv)
	return(M)
	}
}