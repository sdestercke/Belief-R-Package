'plotpl'=function(BBA){		#input:  BBA object
				#display plausibility on singletons

k=length(BBA@group[1,])
MAT=diag(k)
res=vector('numeric',k)
for(i in 1:k){
	t=plausCalc(BBA,MAT[i,])
	res=res+MAT[i,]*t	
	}
plot(res,main='Plausibility on singletons',xlab=expression(theta))
}