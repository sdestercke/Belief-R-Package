\name{pignisticCalc}
\alias{pignisticCalc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pignistic probability computation}
\description{Computes the pignistic probability of a given
BBA. Pignistic probability consists of equi-reparting masses of focal
elements among all elements of the focal set, after the BBA has been normalized.

Pignistic probability corresponds to the centroid of the probability set described by the BBA. }
\usage{
pignisticCalc(BBA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{An object of class BBA}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns an object of class BBA}
\references{
P. Smets (2005) \emph{Decision making in the TBM: the necessity of the pignistic transformation.} Int. J. Approx. Reasoning \bold{38(2): 133-147}
}
\author{N. Maillet, B. Charnomordic, S. Destercke }
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{normalization}}, \code{\link{pignisticExp}}
}
\examples{
M=matrix(c(0,1,0,1,1,0,1,0,0),nrow=3)
V=c(0.6,0.2,0.2)
Pig=BBA(M,V)
pignisticCalc(Pig)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(BBA){	#BBA is a BBA structure
				#returns pignistic probabilities
				#depends: normalization and fromPtoBBAP
	M=normalization(BBA)
	n=length(M@group[1,])
	Pigni=vector('numeric',n)
	for(i in 1:length(M@bba)){
		c=M@group[i,]
		m=M@bba[i]/sum(c)
		c=c*m
		Pigni=Pigni+c
		}
		return(fromPtoBBAP(Pigni))	#computes the pignistic function
	
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
