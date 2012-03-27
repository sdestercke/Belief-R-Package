\name{plotbel}
\alias{plotbel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot of beliefs}
\description{Plots the belief values over singletons}
\usage{
plotbel(BBA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{Object of class BBA}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Return a plot of the belief function on singletons.}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{N. Maillet, B. Charnomordic, S. Destercke
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{plotpl}}}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
plotbel(BBA(M,V))


## The function is currently defined as
function(BBA){	#input: BBA object
				#display belief on singletons
k=length(BBA@group[1,])
res=vector('numeric',k)
MAT=diag(k)
for(i in 1:k){
	t=belCalc(BBA,MAT[i,])
	res=res+MAT[i,]*t	
	}
plot(res,main='belief function on singletons',xlab=expression(theta))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{plot}

