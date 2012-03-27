\name{normalization}
\alias{normalization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{normalization}
\description{Normalizes an unnormalized BBA through a classical normalization tool, i.e., by equi-distributing the mass associated to the empty set towards all existing focal elements.}
\usage{
normalization(BBA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BBA}{an object of class BBA to normalise}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns an object of class BBA}
\references{
P. Smets (2007) \emph{Analyzing the combination of conflicting belief functions}. Information Fusion \bold{8(4): 387-412}
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
M=matrix(c(0,0,1,0,1,0),nrow=3)
V=c(0.4,0.4,0.2)
bba=BBA(M,V)
normalization(bba)


## The function is currently defined as
function(BBA){	#BBA list with bba and group(see definitionbba)
				#depends: BBA
SUM=apply(BBA@group,1,sum)	#search for empty set position (0 if not present)
ind=0
for (i in 1:length(SUM)){
	if(SUM[i]==0){
		ind=i
		}
	}

if(ind!=0){			#normalization
	val=BBA@bba[ind]
	result=BBA@bba/(1-val)
	result=result[-ind]
	return(BBA(Bba=result,Group=as.matrix(BBA@group[-ind,],nrow=length(BBA@bba-1))))

	}
else{
	return(BBA)	#normalized bba
	}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
