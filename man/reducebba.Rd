\name{reducebba}
\alias{reducebba}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reduce BBA}
\description{Reduces a BBA if the same focal element is present
multiple times in a function result (for instance, following a merging procedure). }
\usage{
reducebba(BBA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{an object of class BBA}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Return an object of class BBA}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
M=matrix(c(0,1,1,1,0,0,1,0),nrow=4)
V=c(0.4,0.1,0.3,0.2)
reducebba(BBA(M,V))

## The function is currently defined as
function(BBA){	#takes a BBA as argument
				#depends: mtoBBA and ExtendBBA
if(length(BBA@bba)==1){
	return(BBA)
	}
else{
	k=length(BBA@bba)
	n=length(BBA@group[1,])
	BBAr=vector('numeric',2^n)
	for(i in 1:k){
		p=binDec(BBA@group[i,])
		BBAr[p+1]=BBAr[p+1]+BBA@bba[i]
		}
	BBAr=mtoBBA(ExtendBBA(BBA=BBAr))	#returns the reduced BBA
	return(BBAr)
	}
}
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
