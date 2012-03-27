\name{qCalc}
\alias{qCalc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Commonality Estimation}
\description{Computes the belief function of a given event/subset of the frame of discernment.

Given a BBA \eqn{m:2^\Theta \to [0,1]}, the commonality \eqn{q(A)} of \eqn{A} is \eqn{q(A)=\sum_{E \supseteq A, E \neq \emptyset} m(E) }}
\usage{
qCalc(BBA, vect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{Object of class BBA}
  \item{vect}{Subset (Event) encoded in binary values}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns the q value of \code{vect}}
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
\code{\link{plausCalc}},
\code{\link{bCalc}},
\code{\link{belCalc}}.}
\examples{
M=matrix(c(0,1,1,1),nrow=2)
V=c(0.4,0.6)
bba1=BBA(M,V)
qCalc(bba1,c(0,1))

## The function is currently defined as
function(BBA,vect){	#computes q(vect) vect:binary vector
				
	if(length(vect)!=length(BBA@group[1,])){
		print("arguments are not valid")
		}
	else{
		q=0
		for (i in 1:length(BBA@bba)){
			SUM=as.numeric(BBA@group[i,]|vect)
			if(sum(SUM==BBA@group[i,])==length(vect)){
				q=q+BBA@bba[i]
				}
			}
		}
	return(q)	#returns q(vect)
	}
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
