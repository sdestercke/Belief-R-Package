\name{disAgg2}
\alias{disAgg2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Disjunctive merging rule for 2 BBA's (internal use)}
\description{
Performs a classical disjunctive merging on two BBA's
}
\usage{
disAgg2(BBA1, BBA2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA1}{object of class BBA}
  \item{BBA2}{object of class BBA}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns an objct of class BBA}
\references{
P. Smets, (2008) \emph{Belief Functions: The Disjunctive Rule of Combination and the Generalized Bayesian Theorem.} Classic Works of the Dempster-Shafer Theory of Belief Functions \bold{633-664}.
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{disAgg}}
}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
bba1=BBA(M,V)
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
bba2=BBA(M,V)
disAgg2(bba1,bba2)


## The function is currently defined as
function(BBA1,BBA2)
{	#BBA1,BBA2 two bba structures
				# to be merged
				#depends: binDec, mtoBBA

List1=BBA1@group
List2=BBA2@group
List3=BBA1@bba
List4=BBA2@bba

n=length(List1[1,])

n1=length(List1[,1])
n2=length(List2[,2])

vect=vector('numeric',2^n)

for(i in 1:n1){
	for(j in 1:n2){
		k=binDec(as.numeric(List1[i,]|List2[j,]))
		vect[k+1]=vect[k+1]+List3[i]*List4[j]
		}
	}
bba=mtoBBA(ExtendBBA(BBA=vect))
return(bba)		# disjunctive aggregation of two elements
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
