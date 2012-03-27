\name{disAgg}
\alias{disAgg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Disjunctive merging rule}
\description{Performs a classical disjunctive merging on a list of BBA's}
\usage{
disAgg(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{two or more objects of class BBA}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns an object of class BBA}
\references{
P. Smets, (2008) \emph{Belief Functions: The Disjunctive Rule of Combination and the Generalized Bayesian Theorem.} Classic Works of the Dempster-Shafer Theory of Belief Functions \bold{633-664}.
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dempsterAgg}},\code{\link{disAgg2}},\code{\link{SMCagg}}
}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
bba1=BBA(M,V)
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
bba2=BBA(M,V)
disAgg(bba1,bba2)

## The function is currently defined as
function(...){	#disjunctive aggregation of n elements
				#depends: DisAg2
				#recursive use of disjunctive fusion
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
			Ag=disAgg2(Ag,x[[1]])	#aggregate 2 elements,
			x=x[-1]
			n=n-1
			#remove first element from x 
			#and decrement counter by 1
		}
	return(Ag)	#disjunctive aggregation 
	}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
