\name{dempsterAgg2}
\alias{dempsterAgg2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dempster Agregation for two BBA (internal)}
\description{
Perform Dempster's unnormalized merging (or TBM conjunctive rule) on two BBA's}
\usage{
dempsterAgg2(BBA1, BBA2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA1}{Object of class BBA}
  \item{BBA2}{Object of class BBA}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns an object of class BBA}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{N. Maillet, B. Charnomordic, S. Destercke
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dempsterAgg}}, \code{\link{disAgg2}}
}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
bba1=BBA(M,V)
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
bba2=BBA(M,V)
dempsterAgg2(bba1,bba2)


## The function is currently defined as
function(BBA1,BBA2){	#BBA1,BBA2 ate two BBA structures
					#Dempster fusion of two BBA structures
					#returns a BBA
					#uses : binDec and mtoBBA
List1=BBA1@group
List2=BBA2@group
List3=BBA1@bba
List4=BBA2@bba

n=length(List1[1,])
List5=List1 %*% t(List2)
dim=dim(List5)
dec=vector('numeric',2^n)
for(i in 1:dim[1]){
	for(j in 1:dim[2]){
		if(List5[i,j]!=0){
			c=as.numeric(List1[i,]&List2[j,])
			d=binDec(c)
			b=List3[i]*List4[j]
			dec[d+1]=dec[d+1]+b
			}
		}
	}
sum=sum(dec)
dec[1]=1-sum
Ag=mtoBBA(ExtendBBA(BBA=dec))
return(Ag)		#Dempster aggregation of two elements
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
