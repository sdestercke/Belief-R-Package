\name{Fuzzyset}
\alias{Fuzzyset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fuzzy set}
\description{Instanciates an object of class FuzzySet}
\usage{
Fuzzyset(fset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fset}{\code{vector} containing the fuzzy set membership values of elements.}
}
\details{Values of the membership functions must be in the unit interval.
}
\value{Returns an object of class FuzzySet}
\references{
D. Dubois, H. Prade (1988) \emph{Fuzzy Sets and Systems.} Academic Press, New York. }
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Fuzzysetclass}}, 
}
\examples{
fuzzy=c(0.5,0.9,1)
Fuzzyset(fuzzy)

## The function is currently defined as
function(fset){

boolean=TRUE
for(i in 1:length(fset)){
	if(fset[i]>1||fset[i]<0){	#check that each element is between  0 and 1
		boolean=FALSE
		}
	}
if(boolean==TRUE){
	return(new("fuzzyset",FuzzySet=fset))
	}
else{
	print("error in input argument")
	}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
