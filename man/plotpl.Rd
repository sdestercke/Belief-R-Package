\name{plotpl}
\alias{plotpl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{plot pl}
\description{Plots the plausibility values over singletons}
\usage{
plotpl(BBA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{Object of class BBA}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Return a plot of the plausibilities on singletons.}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{plotbel}}}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
plotpl(BBA(M,V))

## The function is currently defined as
function(BBA){		#input:  BBA object
				#display plausibility on singletons

k=length(BBA@group[1,])
MAT=diag(k)
res=vector('numeric',k)
for(i in 1:k){
	t=plausCalc(BBA,MAT[i,])
	res=res+MAT[i,]*t	
	}
plot(res,main='Plausibility on singletons',xlab=expression(theta))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
