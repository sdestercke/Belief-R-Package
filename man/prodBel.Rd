\name{prodBel}
\alias{prodBel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{mass product computation for SMC (internal use)}
\description{
Mass product computation for SMC
}
\usage{
prodBel(list, c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{
Set of BBAs involved in SMC merging
}
  \item{c}{
Elements whose product mass must be computed
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns product (real-valued) result.
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(list,c){	#computes the product of belief masses for a given SMC
				#list=set of bbas
				#c=intersection asked for

E=length(list)
P=1
while(E!=0){
	a=list[[1]]@bba[c[1]]	#get belief mass associated to set to be used for SMC
	P=P*a
	list=list[-1]
	c=c[-1]
	E=E-1
	}
#returns the  product of masses
return(P)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
