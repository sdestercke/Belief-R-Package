library(graphics)
library(methods)
setClass("Fuzzyset",
	representation(FuzzySet="vector"),
	prototype(FuzzySet=c(0)),
	)
setMethod("plot",signature(x="Fuzzyset"),
	function(x,y="missing"){

matplot(x@FuzzySet,xlab=expression(theta),ylab=expression(alpha),main="Fuzzy set representation",pch="o")
abline(1,0)

})
setMethod("show",signature("Fuzzyset"),
	function(object){
	taille=length(object@FuzzySet)
	theta=c()
	mu=c()
	for(i in 1:taille){
		theta=c(theta,paste("theta",i,sep=""))
		mu=c(mu,object@FuzzySet[i])
		}
	DATA=data.frame(theta=theta, "mu(theta)"= mu, stringsAsFactors = FALSE)
	print(DATA)
})
setMethod("summary",signature("Fuzzyset"),
	function(object){

	taille=length(object@FuzzySet)
	theta=c()
	mu=c()
	support=c()
	kernel=c()
	for(i in 1:taille){
		theta=c(theta,paste("theta",i,sep=""))
		mu=c(mu,object@FuzzySet[i])
		if(object@FuzzySet[i]==1){
			kernel=c(kernel,paste("theta",i,sep=""))
			}
		if(object@FuzzySet[i]>0){
			support=c(support,paste("theta",i,sep=""))
			}
		}
	DATA=data.frame(theta=theta, "mu(theta)"= mu, stringsAsFactors = FALSE)
	print(DATA)
	DATA2=data.frame(support=support)
	DATA3=data.frame(kernel=kernel)
	print(DATA2)
	print(DATA3)
})