\name{BBA}
\alias{BBA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{construct BBA}
\description{
Creates a BBA objet (mass assignment) in an economic way, encoding only the focal sets (sets receiving positive mass).

First elements are in the first column, last elements in the last column.
}
\usage{
BBA(Group, Bba)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Group}{Matrix where each line describes a set in binary fashion (number of columns=number of elements of the discernment frame).}
  \item{Bba}{Vector containing the weights of each focal element.}
}
\details{Matrix \emph{Group} line number must coincide with vector \code{Bba} length. Vector \emph{Bba} elements must sum up to 1. Matrix \code{Group} must contain 0 and 1}
\value{Returns a BBA object}
\references{
P. Smets, R. Kennes, (1994) \emph{The Transferable Belief Model}. Artif. Intell. \bold{66(2): 191-234}

\url{http://iridia.ulb.ac.be/~psmets/}
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ExtendBBA}},
\code{\link{BBAClass}},
\code{\link{ExtendBBAClass}},
\code{\link{BBAtom}},
\code{\link{mtoBBA}}.

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
BBA(M,V)

## The function is currently defined as
function(Group,Bba){

if(abs(sum(Bba)-1)<0.000001 && length(Group[,1]==length(Bba))){
	return(new("BBA",group=Group,bba=Bba))
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
