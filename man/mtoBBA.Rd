\name{mtoBBA}
\alias{mtoBBA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extend bba to bba}
\description{Maps a BBA coded in extensive forms (vectors of \eqn{2^n} elements) to a BBA coded in economical form (only focal sets)}
\usage{
mtoBBA(M)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{Object of class ExtendBBA to map}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{return an object of class BBA}
%\references{
%
%}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
m=c(0,0,0.5,0,0.3,0,0,0.2)
m=ExtendBBA(m)
mtoBBA(m)

## The function is currently defined as
function(M){	#transform a BBA into a reduced structure
m=M			#depends: decBin
s=length(m@bba)		
n=log2(s)
p=round(n)

if(2^p==s){
	j=1
	M=1:n
	groupe=1:n
	for(i in 1:length(m@bba)){
		if(m@bba[i]!=0){	#detect a mass different from 0
			M[j]=m@bba[i]	#add that mass
			j=j+1
			groupe=rbind(groupe,decBin(i-1,n))
			#add corresponding binary coded element
			}
		}
	M=M[1:j-1]	#select only wanted elements
	groupe=groupe[-1,]
	bba=BBA(Group=groupe,Bba=M)
	return(bba)	#transform complete vector into bba
}	
else{
	print("input vector length not valid")
	}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
