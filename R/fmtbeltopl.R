'fmtbeltopl'=function(m){
if(length(m@bel)==1){
	print("error the argument isn't valid. bel was not calculated")
	}
else{
	M=m
	M@b=M@bel
	pl=fmtbtopl(M)
	x<-deparse(substitute(m))
	assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=m@bel,Pl=pl,B=m@b), pos=.GlobalEnv)
	return(pl)
	}
}#