\name{decBin}
\alias{decBin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Decimal to Binary (Internal use)}
\description{
Transforms a decimal number into a binary representation
}
\usage{
decBin(nb, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nb}{Number to transform
}
  \item{n}{
Number of desired binary digits (in the current package, the discernment frame element number)
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns the corresponding vector
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
N. Maillet, B. Charnomordic, S. Destercke
}
%\note{
%
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{binDec}}, \code{\link{mtoBBA}}
}
\examples{
decBin(10,5)

## The function is currently defined as
function(nb,n){	#encode an integer into a binary representation
				#nb = integer, n number of bits to use
v=nb;			

if(nb>=2^n){
	print("arguments are not valid")
	}
else{
	bin=vector('numeric',n)
	i=1
	while(v!=0){
		p=v%/%2
		if(2*p==v){
			bin[i]=0
			}
		else{
			bin[i]=1
			}
		v=p
		i=i+1
		}
	return(bin)	#return binary encoding
	}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
