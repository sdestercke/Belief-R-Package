\name{searchInterval}
\alias{searchInterval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{search intervalle (Internal use SMC)}
\description{
Search for intervals with non-empty intersection.
}
\usage{
searchInterval(bbalist, c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bbalist}{
List of BBAS
}
  \item{c}{
Vector containing the indices of selected elements}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{N. Maillet, B. Charnomordic, S. Destercke

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
function(bbalist,c){	#bbalist=set of bbas, c=selection vector for each bba
l=c()					#fonction used for SMC
L=length(bbalist)				#searches for intersecting intervals 
for(i in 1:L){
	k=bbalist[[i]]@group[c[i],]
	p=length(bbalist[[i]]@group[c[i],])
	p=1:p
	p=p*k
	p=p[p!=0]
	if(sum(p)!=0){
		l=c(l,min(p),max(p))
		}
	else{
		l=c(l)
		}
	}
return(l)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
