'intersectionint'=function(interval,c){	

#internal - used by SMCgen
#

inf=2*c-1
sup=2*c

bound_inf=max(interval[inf])
bound_sup=min(interval[sup])

if(bound_inf>bound_sup){
	bound_inf=0
	bound_sup=0
	}

return(c(bound_inf,bound_sup))
}