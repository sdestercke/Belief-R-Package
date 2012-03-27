\name{SMCgen}
\alias{SMCgen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{interval SMC}
\description{This function searches, within a set of intervals, the maximal coherent subsets of these intervals (i.e., biggest subsets of intervals with non-empty intersection), perform the intersection within these subsets and send back the resulting set of intervals.
}
\usage{
SMCgen(interval)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{interval}{
List of intervals within which SMC must be searched
}
}
\details{
works on ordered spaces where intervals are sequences of elements. Function  requires the bounds of the intervals (works on real-valued intervals too).
}
\value{
return set of intervals.}
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

\seealso{
\code{\link{SMCagg}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(interval){	#interval=vector with 2n elements
				#each pair represents an interval, mass=vector of associated masses
				#depends: intersectionint
				#does SMC merging of several intervals
Listg=list()
j=1
K=c()

l=length(interval)/2
c=1:l
source=sort(c(c,c))

q=sort(interval)
p=order(interval)
source=source[p]
type=p%%2	#1 for inf bound, 0 for sup bound

k=1
while(k!=0){
	k=0
	for(i in 1:(length(p)-1)){
		if(q[i]==q[i+1] && type[i]<type[i+1]){
			type[i]=1
			type[i+1]=0
			h=source[i]
			source[i]=source[i+1]
			source[i+1]=h
			k=1
			}
		}
	}

for(i in (1:(length(p)-1))){
	if(type[i]==1){
		K=c(K,source[i])
		if(type[i+1]==0){
			Listg[[j]]=K
			j=j+1
			}
		}
	else{
		ind=1:length(K)
		ind2=as.numeric(K==source[i])
		ind3=ind%*%ind2
		K=K[-ind3]
		}
	}
k=length(Listg)
intersection=list()
for(i in 1:k){
	c=intersectionint(interval,Listg[[i]])
	intersection[[i]]=c
	}
return(list(origin=Listg,intersection=intersection))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
