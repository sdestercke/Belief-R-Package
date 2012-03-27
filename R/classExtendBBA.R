library(graphics)
library(methods)
setClass("ExtendBBA",
	representation(bba="vector",bel="vector",pl="vector",q="vector",b="vector"),
	prototype(bba=c(0),bel=c(0),pl=c(0),q=c(0),b=c(0)),
	)
setMethod("plot",signature(x="ExtendBBA"),
	function(x,y="missing"){

matplot(x@bba,xlab=expression(theta),ylab='m',main="BBA")
abline(1,0)

})
setMethod("show",signature("ExtendBBA"),
	function(object){

	if(length(object@bba)!=1){
		print(object@bba)
		}
	else{
		print("The mass vector must be computed")
		}
})
setMethod("summary",signature("ExtendBBA"),
	function(object){

	if(length(object@bba)!=1){
		pl=fmtmtopl(object)#@pl
		bel=fmtmtobel(object)#@bel
		DATA=data.frame(m=object@bba,pl=pl,bel=bel, stringsAsFactors = FALSE)
		print(DATA)
		}
	else{
		print("The mass vector must be computed")
		}
})