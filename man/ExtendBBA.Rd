\name{ExtendBBA}
\alias{ExtendBBA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Construct extend bba}
\description{
Instanciates an object of the class ExtendBBA.
}
\usage{
ExtendBBA(BBA = 0, Bel = 0, Pl = 0, Q = 0, B = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{
\code{vector} containing the mass assignment information} 
  \item{Bel}{
\code{vector} containing the belief measure information}
  \item{Pl}{
\code{vector} containing the plausibility measure information
}
  \item{Q}{
\code{vector} containing the commonality measure information
}
  \item{B}{
\code{vector} containing the implicability measure information}
}
\details{At least one field must be given to instanciate the object. Vectors must be a power of 2.}
\value{Returns an object of class ExtendBBA.}
\references{
P. Smets (2002) \emph{The application of the matrix calculus to belief functions}. Int. J. Approx. Reasoning \bold{31(1-2): 1-30}
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ExtendBBAClass}}
}
\examples{
m=c(0,0,0.5,0,0.3,0,0,0.2)
ExtendBBA(m)

## The function is currently defined as
function(BBA=0,Bel=0,Pl=0,Q=0,B=0){

s=length(BBA)
n=log2(s)
p=round(n)
if(2^p==s){ #&& abs(sum(BBA)-1)<0.0000001
	return(new("ExtendBBA",bba=BBA,bel=Bel,pl=Pl,q=Q,b=B))
	}
else{
	print("error in input vector")
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
