'fmtmtonm'=function(m){
if(length(m@bba)==1){
	print("error the argument isn't valid. m was not calculated")
	}
else{
	m=m@bba
	fmt=m/(1-m[1])
	fmt[1]=0
	return(ExtendBBA(BBA=fmt))
	}
}