\name{intersectionint}
\alias{intersectionint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interval intersection (internal use of SMCagg)}
\description{
compute the intersection of a set of intervals provided over an ordered frame. 
}
\usage{
intersectionint(interval, c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{interval}{
Set of intervals}
  \item{c}{
Index of intervals whose intersection is to be computed
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
return the intersection of all intervals.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
N. Maillet, B. Charnomordic, S. Destercke}
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
function(interval,c){	

#internal - used by SMCgen
#

inf=2*c-1
sup=2*c

bound_inf=max(interval[inf])
bound_sup=min(interval[sup])

if(bound_inf>bound_sup){
	bound_inf=0
	bound_sup=0
	}

return(c(bound_inf,bound_sup))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
