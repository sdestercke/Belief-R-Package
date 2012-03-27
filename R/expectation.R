'expectation'<-function(BBA,c){	#c=function f(theta_i)
				#returns bounds of BBA expectation
				#depends on: normalization
	if(length(c)!=length(BBA@group[1,])){	#check arguments
		print("error in the length of c")
		}
	else{
		B=normalization(BBA)
		Einf=0
		Esup=0
		for(i in 1:length(B@bba)){
			k=B@group[i,]
			F=k*c
			g=k==1
			F=F[g]
			Einf=Einf+B@bba[i]*min(F)
			Esup=Esup+B@bba[i]*max(F)
			}
		return(list(Expectation_inf=Einf,Expectation_sup=Esup))	#returns inf and sup expectation as a list
		}
}