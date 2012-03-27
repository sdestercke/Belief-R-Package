'plotbel'=function(BBA){	#input: BBA object
				#display belief on singletons
k=length(BBA@group[1,])
res=vector('numeric',k)
MAT=diag(k)
for(i in 1:k){
	t=belCalc(BBA,MAT[i,])
	res=res+MAT[i,]*t	
	}
plot(res,main='belief function on singletons',xlab=expression(theta))
}