'fmtbtobel'=function(m){
if(length(m@b)==1){
	print("error the argument isn't valid. b was not calculated")
	}
else{
	b=m@b
	if(b[1]==1){
		print("Problem in fmtbtobel, m( )=1")
		}
	else{
		lm=length(b)
		natoms=round(log2(lm))
		if(2^natoms==lm){
			if(length(m@bel)==1){
				k=1-b[1]
				b=(b-b[1])/k
				b[1]=0
				bel=b
				x <- deparse(substitute(m))
		  		assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=bel,Pl=m@pl,B=m@b), pos=.GlobalEnv)
				return(bel)
				}
			else{
				return(m@bel)
				}
			}
		else{
			print("Accident in fmtbtobel : length of input vector not valid")
			}
		}
	}
}