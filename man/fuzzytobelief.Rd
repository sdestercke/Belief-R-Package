\name{fuzzytobelief}
\alias{fuzzytobelief}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fuzzy set to BBA}
\description{Transforms a fuzzy set into its equivalent BBA (where focal elements are nested, or included into each other). }
\usage{
fuzzytobelief(fuzzy)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fuzzy}{An object of the FuzzySet class.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns the transformed object of class BBA}
\references{
D. Dubois, H. Prade (1990) \emph{Consonant approximations of belief functions}. Int. J. Approx. Reasoning \bold{4(5-6): 419-449}
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{objectiveTransfo}}, \code{\link{subjectiveTransfo}}
}
\examples{
fuzzy=c(0.5,0.9,1)
fuzzy=Fuzzyset(fuzzy)
fuzzytobelief(fuzzy)

## The function is currently defined as
function(fuzzy)
{	#fuzzy=vector including alpha for each set theta1...thetaN
					#depends: BBA

#fuzzyset objects are tested at object instanciation
	d=sort(fuzzy@FuzzySet)
	nb=length(d)
	e=vector('numeric',nb)
#associate sets to the values
	for(i in 1:nb){
		g=as.numeric(fuzzy@FuzzySet==d[i])
		e[i]=max((1:nb)*g)
		fuzzy@FuzzySet[e[i]]=-1
		}
	E=matrix(0,ncol=nb,nrow=nb)
	j=0
	m=vector('numeric',nb)
	#construct belief function with mass and corresponding set
	for (i in 1:nb){
		if(d[i]==0){
			j=j+1
			}
		else{		
			if(i!=1){
				m[i]=d[i]-d[i-1]
				T=e[i:length(d)]			#revoir le rappel des ensembles????
				H=vector('numeric',length(d))
				H[T]=1
				E[i,]=H
				}
			else{
				m[i]=d[i]
				k2=e[i:length(d)]
				k2[]=1
				E[i,]=k2
				}
			}
		}
	#subnormalization case
	k=sum(m[(j+1):nb])
	nb2=nb
	if(k<1){	#add empty set
		p=1-k
		m=c(m,p)
		nb=nb+1
		d=vector('numeric',length(d))
		E=rbind(E,d)
		nb2=nb-1
		}
	result=BBA(Group=E[m>0,,drop=FALSE],Bba=m[m>0])
		return(result)	#transform fuzzy set into belief function
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
