\name{pignisticExp}
\alias{pignisticExp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pignistic expectation computation}
\description{
Computea the Pignistic expectation of a given BBA and a given (utility) function. 
}
\usage{
pignisticExp(BBA, c)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{An object of class BBA}
  \item{c}{\code{vector} containing the function value for each element}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns the (real-valued) pignistic expectation}
\references{
 P. Smets (2005) \emph{Decision making in the TBM: the necessity of the pignistic transformation.} Int. J. Approx. Reasoning \bold{38(2): 133-147}
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pignisticCalc}},
}
\examples{
M=matrix(c(0,1,0,1,1,0,1,0,0),nrow=3)
V=c(0.6,0.2,0.2)
bba=BBA(M,V)
pignisticExp(bba,c(1,2,3))


## The function is currently defined as
function(BBA,c){	#BBA structure, c: vector f(theta_i)
					#Computes pignistic Expectation
					#depends: pignistic and expectation
Pig=pignisticCalc(BBA)
#P=length(Pig@bba)
#Part1=matrix(0,nrow=P,ncol=P)
#for(i in 1:P){
#	Part1[i,i]=1
#	}
#Pigni=BBA(Group=Part1,Bba=Pig@bba)
#Epig=expectation(Pigni,c)
Epig=expectation(Pig,c)
if(abs(Epig$Expectation_inf-Epig$Expectation_inf)<(min(c)*10^-6)){	#check that inf Expectation equals sup Expectation
	Epig=Epig$Expectation_inf
	}
else{
	Epig=("Error in 'pignisticExp")
	}
return(Epig)	#returns Expectation of Pignistic

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
