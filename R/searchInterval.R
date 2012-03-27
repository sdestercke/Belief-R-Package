'searchInterval'=function(bbalist,c){	#bbalist=set of bbas, c=selection vector for each bba
l=c()					#fonction used for SMC
L=length(bbalist)				#searches for intersecting intervals 
for(i in 1:L){
	k=bbalist[[i]]@group[c[i],]
	p=length(bbalist[[i]]@group[c[i],])
	p=1:p
	p=p*k
	p=p[p!=0]
	if(sum(p)!=0){
		l=c(l,min(p),max(p))
		}
	else{
		l=c(l)
		}
	}
return(l)
}