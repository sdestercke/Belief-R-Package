\name{Pignistic}
\alias{Pignistic}
\alias{pignistic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pignistic}
\description{Instanciates an object of Pignistic class}
\usage{
Pignistic(pig, Group)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pig}{Probability masses on singletons}
  \item{Group}{Matrix of elements to which are associated masses (each line contains one 1 and all 0}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns an object of Pignistic class}
\references{
P. Smets (2005) \emph{Decision making in the TBM: the necessity of the pignistic transformation.} Int. J. Approx. Reasoning \bold{38(2): 133-147}}
\author{ N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Pignisticclass}}
}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
Pignistic(M,V)

## The function is currently defined as
function(pig,Group){

if(abs(sum(pig)-1)<0.000001 && length(Group[,1]==length(pig))){
	return(new("Pignistic",Pig=pig,group=Group))
	}
else{
	print("error in input arguments")
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
