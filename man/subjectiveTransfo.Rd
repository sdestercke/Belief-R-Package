\name{subjectiveTransfo}
\alias{subjectiveTransfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Subjective Transformation}
\description{Performs a transformation from a probability to a fuzzy set, using the subjective transformation (considering that the probability is the pignistic of a fuzzy set), resulting in a more imprecise fuzzy set (compared to objective transformation)}
\usage{
subjectiveTransfo(vect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vect}{\code{vector} containing the probability masses of each elements.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns an object of the class Fuzzyset}
\references{D. Dubois, H. Prade, P. Smets (2008) \emph{A definition of subjective possibility}. Int. J. Approx. Reasoning (IJAR) \bold{48(2):352-364}
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{objectiveTransfo}}}
\examples{
p=c(0.2,0.6,0.2)
subjectiveTransfo(p)

## The function is currently defined as
function(vect){	#vect: vector of probabilities
					#depends: fuzzyset
					#passes from probabilities to
					#fuzzy set using the subjective transformation
vect2=vect
mu=vect
for(i in 1:length(vect)){
	mu[i]=sum(pmin(vect[i],vect))
	}
return(Fuzzyset(mu))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
