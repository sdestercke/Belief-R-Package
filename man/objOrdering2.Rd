\name{objOrdering2}
\alias{objOrdering2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Object ordering (Descending)}
\description{Order a set of uncertain variables described by BBA'S according to the lower and upper expectations of an estimation function (ordering use a partial order where object O1 dominates object O2 if lower/upper expe of O1 are higher than lower/upper expe of O2, respectively). 

The order induce a pre-order between objects. It start from the equivalence class of best objects and put it in it objects that are not dominated by any other object. It then remove them and iteratively repeat the procedure until all objects are classified, going from the first to the last equivalence class.}
\usage{
objOrdering2(listbba, fc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{listbba}{List of objects of class BBA (describing uncertain knowledge about objects to order).}
  \item{fc}{Estimation function (common to all objects) used to compute expectation}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{return a list with ordered objects (BBA's)}
\references{
To come... }
\author{N. Maillet, B. Charnomordic, S. Destercke}
\note{
This function should be:

1. merged with objOrdering (adding an option for ascending/descending ordering).

2. comparison function should be parametrized (passed as argument) so as to accomodate with different binary comparison operators.

3. complemented with a function giving one BBA and different utility function (for different actions).

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{OrderingRelia}
\examples{
M=matrix(c(0,1,1,1),nrow=2)
V=c(0.4,0.6)
bba1=BBA(M,V)
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
bba2=BBA(M,V)
objOrdering2(list(bba1,bba2),c(1,2))

## The function is currently defined as
function(listbba,fc){	#...=list of bbas, fc=function used to compute esperance

List=listbba
n=length(List)
for(i in 1:n){
	List[[i]]=expectation(List[[i]],fc)	#Compute expectation for each bba
	}
E=list()
j=1
E[[j]]=c(0)
Num=1:length(List)
Num2=Num
while(length(List) != 0){			#continue until no more elements in the list
E[[j]]=c(0)
d=0
	for(i in 1:length(List)){		#for each element of the list
		dominated=FALSE
		for(k in 1:length(List)){	#see if it is dominated
			if((List[[i]]$Expectation_inf<List[[k]]$Expectation_inf && List[[i]]$Expectation_sup<=List[[k]]$Expectation_sup && i!=k	)||(List[[i]]$Expectation_inf<=List[[k]]$Expectation_inf && List[[i]]$Expectation_sup<List[[k]]$Expectation_sup && i!=k)){
				dominated=TRUE
				break
				}
			}
		if(dominated==FALSE){		#if not, class it in current group
			E[[j]]=c(E[[j]],Num[i])
			}
		}
	
	E[[j]]=E[[j]][-1]			#remove element after it has been classified
	List=List[-E[[j]]]
	

	E[[j]]=Num2[E[[j]]]			#update
	nbt=length(E[[j]])
	for(i in 1:nbt){
		Num2=Num2[Num2!=E[[j]][i]]
		}
	j=j+1
	d=length(List[E[[j-1]]])
	}
return(E)		#return list of ordered bbas
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
