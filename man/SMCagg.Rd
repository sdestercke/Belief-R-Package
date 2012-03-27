\name{SMCagg}
\alias{SMCagg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SMC agregation}
\description{Merges several BBA'S using a maximal coherent subset (conjunctive) merging rule.}
\usage{
SMCagg(List1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{List1}{List of objects of class BBA to merge}
}
\details{
The merging (mainly the algorithm used for identification of maximal coherent subset) is based on the assumption that the space is (pre-)ordered and that focal elements are only constituted of sequences of elements (i.e., intervals)
}
\value{Returns a list with the merged object of class BBA and the BBA's that formed maximal coherent subsets}
\references{
S. Destercke, D. Dubois, E. Chojnacki (2009) \emph{Possibilistic Information Fusion Using Maximal Coherent Subsets.} IEEE T. Fuzzy Systems \bold{17(1): 79-92}

J.M. Vannobel, I.J. Madiomanana, O. Colot (2008) \emph{About the combination of belief functions on real numbers and the fusion based on maximal coherent sets}. Proc of Information Fusion \bold{1-8}
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{SMCgen}}
}
\examples{
M=matrix(c(1,1,1,0,1,1,0,0,1),nrow=3)
V=c(0.4,0.3,0.3)
bba1=BBA(M,V)
M=matrix(c(1,0,1,1,0,1),nrow=2)
V=c(0.4,0.6)
bba2=BBA(M,V)
M=matrix(c(1,0,1,0,1,1),nrow=2)
V=c(0.5,0.5)
bba3=BBA(M,V)
SMCagg(list(bba1,bba2,bba3))

## The function is currently defined as
function(List1){	#List1=list of bbas
				#use only for ordered fuzzy sets
				#depends: searchInterval, prodBel, SMCgen, reduceBBA, decBin
ENS=List1
ENS2=ENS
k=length(ENS)
HELP=matrix(vector('numeric',k),nrow=1)
SUM=c(0)
SUM2=vector('numeric',2^k)
NB=vector('numeric',2^k)
P=length(List1)
if(k==1){
	return(ENS[[1]])
	}
else{
	SMC=list(group=matrix(0,ncol=length(ENS[[1]]@group[1,]),nrow=1),bba=0)
	Nbline=c()
	while(length(ENS2)!=0){				#gets number of rows for each BBAc
		a=length(ENS2[[1]]@group[,2])
		Nbline=c(Nbline,a)
		ENS2=ENS2[-1]
		}
	c=vector('numeric',k)
	c[]=1
	c[k]=0
	HelpSrc=c(1:k)
	for(i in 1:prod(Nbline)){			#browse all possible combinations
		c[k]=c[k]+1				#search for next sets to merge
		if(c[k]>Nbline[length(c)] && i!=prod(Nbline)){
			c=add1vec(c[1:k-1],Nbline[-k])
			c=c(c,1)
			}
		if(c[k]>Nbline[length(c)] && i==prod(Nbline)){	#check that found set is correct
			c=Nbline
			}
		inter=searchInterval(ENS,c)		#search from intervals issued from theta_i
		if(length(inter)==0){				#if resulting set is limited to a singleton
			ensemble=vector('numeric',length(ENS[[1]]@group[1,]))
			bba=prodBel(ENS,c)
			HelpSrc=rbind(HelpSrc,c)
			SMC$bba=c(SMC$bba,bba)
			SMC$group=rbind(SMC$group,ensemble)
			}
		else{
		AP=SMCgen(inter)			#if resulting set is not limited to a singleton
		intersect=AP$intersection		# SMC aggregation of current sets
		nbinter=length(intersect)		#nbinter=nb of intervals resulting from merging
		j=c()
		for(h in 1:nbinter){
			j=c(j,intersect[[h]][1]:intersect[[h]][2])	
			}
		intersect2=vector('numeric',length(ENS[[1]]@group[1,]))
		intersect2[j]=j				#create set resulting from merging
		intersect2=as.numeric(intersect2>0)
		HelpSrc=rbind(HelpSrc,c)			#complete help field to remember source
		SMC$group=rbind(SMC$group,intersect2)	#row number in bba to intersect
		bba=prodBel(ENS,c)			#complete structure of future BBA object 
		SMC$bba=c(SMC$bba,bba)			#mass of corresponding set
		for(pl in 1:length(AP$origin)){
				u1=vector('numeric',k)
				u1[AP$origin[[pl]]]=1
				HELP=rbind(HELP,u1)
				SUM=c(SUM,bba)
				u2=binDec(u1)
				SUM2[u2+1]=SUM2[u2+1]+bba
				NB[u2+1]=NB[u2+1]+1
				}
			}
		}
	SMC$group=SMC$group[-1,]			#structure is complete
	SMC$bba=SMC$bba[-1]
	###rajouter si SMC=une ligne
	if(is.vector(SMC$group)){
		SMC$group=matrix(SMC$group,nrow=1)
		}##fin de l'ajout
	SMC2=reducebba(BBA(Group=SMC$group,Bba=SMC$bba))#reduce bba object
	HelpSrc=HelpSrc[-1,]
	nb=length(SMC2@group[1,])
	HELP2=matrix(0,ncol=k,nrow=1)
	for(pl in 1:length(SUM2)){
		bm=decBin(pl-1,k)
		HELP2=rbind(HELP2,bm)
		}
	ORIGIN=list(CRITERE=HELP2[-1,],MASSE=SUM2,TOTAL=NB)
		return(list(SMC=SMC2,SOURCE=ORIGIN))	#return result
	}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
