\name{BBAtom}
\alias{BBAtom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transforms bba to m}
\description{
Transforms a BBA object, encoded in an economical way, into an extendedBBA (object of class \code{ExtendBBA}), where all elements of the power set are encoded. 

Produces a linear vector with \eqn{2^n} elements, where \eqn{n} is the cardinality of the discernment frame. 
}
\usage{
BBAtom(BBA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{Object of class BBA}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns an object of class ExtendBBA}
\references{
P. Smets (2002) \emph{The application of the matrix calculus to belief functions}. Int. J. Approx. Reasoning \bold{31(1-2): 1-30}
}
\author{ N. Maillet, B. Charnomordic, S. Destercke }
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ExtendBBA}},
\code{\link{ExtendBBAClass}},
\code{\link{mtoBBA}}.
}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
BBAtom(BBA(M,V))

## The function is currently defined as
function(BBA){	#transform BBA structure into ExtendBBA
				#depends on binDec and ExtendBBA
	n=length(BBA@group[1,])
	ind=vector('numeric',2^n)

	for(i in 1:length(BBA@bba)){		#decimal coding of sets and create the bba vector
		dec=binDec(BBA@group[i,])
		ind[dec+1]=BBA@bba[i]
		}
	return(ExtendBBA(BBA=ind))	#transform a bba structure into an extensive vector
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
