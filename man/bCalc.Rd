\name{bCalc}
\alias{bCalc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Implicability Estimation}
\description{Compute the implicability function of a given event/subset of the frame of discernment.

Given a BBA \eqn{m:2^\Theta \to [0,1]}, the implicability \eqn{b(A)} of \eqn{A} is \eqn{b(A)=\sum_{E \subseteq A, E \neq \emptyset} m(E) + m(\emptyset)} }
\usage{
bCalc(BBA, vect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{Object of class BBA}
  \item{vect}{Subset (Event) encoded in binary values}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns the b value of \code{vect}}
\references{
P. Smets (1998) \emph{The Transferable Belief Model for Quantified Belief Representation.} Handbook of Defeasible Reasoning and Uncertainty Management Systems. Gabbay D. and Smets Ph. (Series Eds). Ph. Smets (Vol. eds.), \bold{Vol. 1 : Quantified Representation of Uncertainty & Imprecision, Kluwer, Doordrecht 267-301.}
}
\author{N. Maillet, B. Charnomordic, S. Destercke }
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{BBA}},
\code{\link{belCalc}},
\code{\link{plausCalc}},
\code{\link{qCalc}}.

}
\examples{
M=matrix(c(0,1,1,1),nrow=2)
V=c(0.4,0.6)
bba1=BBA(M,V)
bCalc(bba1,c(0,1))

## The function is currently defined as
function(BBA,vect){	#computes b(vect), vect: binary vector
				
	if(length(vect)!=length(BBA@group[1,])){
		print("arguments are not valid")
		}
	else{
		b=0
		for (i in 1:length(BBA@bba)){
			SUM=as.numeric(vect|BBA@group[i,])
			if(sum(SUM==vect)==length(vect)){
				b=b+BBA@bba[i]
				}
			}
		}
	return(b)
	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
