library(graphics)
library(methods)
setClass("Pignistic",
	representation(bba="vector",group="matrix"),
	prototype(bba=c(0),group=matrix(0)),
	)
setMethod("plot",signature(x="Pignistic"),
	function(x,y="missing"){

plot(x@bba,xlab=expression(theta),ylab='Proba',main="Pignistic Probability")
abline(1,0)

})
setMethod("show",signature("Pignistic"),
	function(object){
	cat("Pignistic probability of a subset with ",length(object@bba)," elements \n")
	cat(object@bba,"\n")
})
setMethod("summary",signature("Pignistic"),
	function(object){

cat("Pignistique d'un ensemble a ",length(object@bba), "modalites\n")
print(object@bba)

})

