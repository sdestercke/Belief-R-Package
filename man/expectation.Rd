\name{expectation}
\alias{expectation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Expectation computation}
\description{
Computes, for a given BBA, the lower and upper expectation reached by a function \code{c} on finite spaces, using the mass assignment and the Choquet integral.

Note that classical expectations are retrieved when the BBA is equivalent to a probability distribution (masses on singletons only). 
}
\usage{
expectation(BBA, c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{Object of class BBA}
  \item{c}{Vector containing the values of the function whose expectation has to be estimated.}
}
\details{
Expectations (lower and upper) are computed once the BBA have been normalized (if it is not already), otherwise this would make their interpretation as expectations dubious. 
}
\value{Returna a list with the lower and the upper expectations (coinciding when BBA is a probability).}
\references{
A.P. Dempster, (2008): \emph{Upper and Lower Probabilities Induced by a Multivalued Mapping.} Classic Works of the Dempster-Shafer Theory of Belief Functions \bold{57-72}
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
bba=BBA(M,V)
expectation(bba,c(1,2))


## The function is currently defined as
function(BBA,c){	#c=function f(theta_i)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
