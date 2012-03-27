\name{binDec}
\alias{binDec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Binary to Decimal (Internal use)}
\description{
Transforms a Binary encoded vector into its decimal evaluation.
}
\usage{
binDec(vect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vect}{
\code{vect} is the binary vector to transform
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns the corresponding decimal value.
}
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
\code{\link{decBin}}, \code{\link{BBAtom}}
}
\examples{
binDec(c(1,0,0))

## The function is currently defined as
function(vect){	#encode a binary vector into integers
				#vect=binary vector
k=0;
for(i in 1:length(vect)){
	k=k+vect[i]*2^(i-1)
	}
return(k)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
