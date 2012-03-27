'add1vec'=function(a,b){	#add 1 to a, according to b argument
				# used by SMCagg
k=length(a)
kl=length(b)
a[k]=a[k]+1

if(a[k]>b[kl] && kl>0){
	a=add1vec(a[1:(k-1)],b[-kl])
	a=c(a,1)	
	}
return(a)
}