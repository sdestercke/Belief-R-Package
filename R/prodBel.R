'prodBel'=function(list,c){	#computes the product of belief masses for a given SMC
				#list=set of bbas
				#c=intersection asked for

E=length(list)
P=1
while(E!=0){
	a=list[[1]]@bba[c[1]]	#get belief mass associated to set to be used for SMC
	P=P*a
	list=list[-1]
	c=c[-1]
	E=E-1
	}
#returns the  product of masses
return(P)
}