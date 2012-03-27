'fmtbeltoq'=function(m){
if(length(m@bel)==1){
	print("error the argument isn't valid. bel was not calculated")
	}
else{
	B=m
	B@b=B@bel
	q=fmtbtoq(B)
	x<-deparse(substitute(m))
	assign(x,ExtendBBA(BBA=m@bba,Q=q,Bel=m@bel,Pl=m@pl,B=m@b), pos=.GlobalEnv)
	return(q)
	}
}##