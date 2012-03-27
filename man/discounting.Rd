\name{discounting}
\alias{discounting}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Discounting operation}
\description{
Performs the (classical) discounting operation on a BBA.

The discounting operation use a number \eqn{\alpha \in [0,1]} to make the BBA more imprecise. \eqn{\alpha} is often interpreted as the reliability of the source (1 if the source is totally reliable, 0 if it is totally unreliable).

Discounting adds \eqn{\alpha} to the whole space focal element, and multiply by \eqn{\alpha} the existing focal elements
}
\usage{
discounting(BBA, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{An object of class BBA}
  \item{alpha}{Reliability degree of the information}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns a discounted object of class BBA}
\references{
D. Mercier, B. Quost, T. Denoeux. (2008) \emph{Refined modeling of sensor reliability in the belief function framework using contextual discounting.} Information Fusion \bold{9(2): 246-258}
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
bba=BBA(M,V)
discounting(bba,0.5)


## The function is currently defined as
function(BBA,alpha){	#BBA= BBA structure, alpha in [0,1]
					#Computes BBA weakening

if(alpha<0 || alpha>1){
	print("arguments are not OK")
	}
else{
	boolean=FALSE
	num=0					#omega
	TOT=length(BBA@group[1,])
	for(i in 1:length(BBA@bba)){
		if(sum(BBA@group[i,])==TOT){	#checks if omega is present
			boolean=TRUE
			num=i
			break
			}
		}
	if(boolean==FALSE){			# if omega is not present, add it to BBA object with null mass
		V=sample(1:1,TOT,TRUE)
		BBA@group=rbind(BBA@group,V)
		BBA@bba=c(BBA@bba,0)
		num=length(BBA@bba)
		}
	BBA@bba=BBA@bba*alpha
	BBA@bba[num]=BBA@bba[num]+(1-alpha)	#computes belief masses of focal elements
	return(BBA)
	}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
