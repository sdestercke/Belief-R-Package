\name{dempsterAgg}
\alias{dempsterAgg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dempster Agregation}
\description{Performs Dempster's unnormalized conjunctive rule (also
called TBM conjunctive rule) on a set of BBA objects}
\usage{
dempsterAgg(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Two or more objects of class BBA}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns an object of class BBA resulting from the merging.}
\references{
P. Smets, (1990) \emph{The Combination of Evidence in the Transferable Belief Model}. IEEE Trans. Pattern Anal. Mach. Intell. \bold{12(5): 447-458}
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dempsterAgg2}}, \code{\link{disAgg}}, \code{\link{SMCagg}}
}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
bba1=BBA(M,V)
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
bba2=BBA(M,V)
dempsterAgg(bba1,bba2)

## The function is currently defined as
function(...){	#Dempster aggregation of n elements
					#depends: DempsterAgg2
					#recursive use of Dempster fusion
x=list(...)
Ag=x[[1]]
n=length(x)
x=x[-1]
if(n==1){
	return(x[[1]])
	}
else{
	n=n-1
	while(n!=0){
			
			Ag=dempsterAgg2(Ag,x[[1]])	#aggregate 2
			#elements, remove element from list and
			#decrement counter
			x=x[-1]
			n=n-1
		}
	return(Ag)		#returns Dempster aggregation
	}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
