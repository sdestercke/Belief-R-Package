\name{fromPtoBBAP}
\alias{fromPtoBBAP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{from probability to corresponding BBA}
\description{
Transforms a probability given as a vector of probability masses over singletons into the corresponding object of class BBA
}
\usage{
fromPtoBBAP(pig)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pig}{
\code{vector} containing the probability masses to transform}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns the object of class BBA corresponding to Pig.}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
N. Maillet, B. Charnomordic, S. Destercke
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{Pignistic}}, 
}
\examples{
p=c(0.3,0.2,0.5)
fromPtoBBAP(p)

## The function is currently defined as
function(pig){	#argument=output of Pignistic function
					#depends: verifPig and verifbba
	P=length(pig)
	Part1=matrix(0,nrow=P,ncol=P)
	for(i in 1:P){
		Part1[i,i]=1
		}
	Pig2=BBA(Group=Part1,Bba=pig)
		return(Pig2)	#returns pignistic as a BBA
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
