\name{objectiveTransfo}
\alias{objectiveTransfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Objective transformation of probability to fuzzy set}
\description{Transforms a probability vector into a corresponding
object, using the objective transformation, thus obtaining a precise fuzzy set (compared to the one obtained by subjective transformation). }
\usage{
objectiveTransfo(vect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vect}{\code{vector} containing the probability masses of the probability to transform}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns an object of the class FuzzySet}
\references{
D. Dubois, L. Foulloy, G. Mauris, H. Prade (2004) \emph{Probability-Possibility Transformations, Triangular Fuzzy Sets, and Probabilistic Inequalities.} Reliable Computing \bold{10(4): 273-297}
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{subjectiveTransfo}}}
\examples{
p=c(0.2,0.6,0.2)
objectiveTransfo(p)

## The function is currently defined as
function(vect){	#vect:  vector of probabilities
					#depends: fuzzyset
					#transforms a probability into
					#a fuzzy set using the
					#objective transformation 
taille=length(vect)
ordre=order(vect)
proba=vect[ordre]
mu=cumsum(proba)
mu=mu[ordre]
return(Fuzzyset(mu))

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
