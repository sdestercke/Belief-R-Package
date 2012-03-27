\name{writefile}
\alias{writefile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{function use by the script test
}
\description{
Internal use for testing
}
\usage{
writefile(group_fuzzysets, SMCagr, group_bba, group_SMCag, del, Nom, num = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{group_fuzzysets}{
First arg.
}
  \item{SMCagr}{
Second arg.
}
  \item{group_bba}{
Third arg.
}
  \item{group_SMCag}{
Fourth arg.
}
  \item{del}{
Fifth arg.
}
  \item{Nom}{
sixth arg.
}
  \item{num}{
seventh arg.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
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

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(group_fuzzysets,SMCagr,group_bba,group_SMCag,del,Nom,num=1){
#does not assign elements
#writes results in files
if(num==1){
write.table(paste("Fuzzy sets are read by row"),paste("RES",del,"Fuzzy_set ",Nom,sep=""),row.names=FALSE,col.names=FALSE,qmethod="double",append=FALSE)
for(J in 1:length(group_fuzzysets[[i]])){
	F=group_fuzzysets[[i]][[J]]
	write.table(paste("fuzzyset ",NAMES[J],sep=""),paste("RES",del,"Fuzzy_set ",Nom,sep=""),row.names=FALSE,col.names=FALSE,qmethod="double",append=TRUE)
	write.table(t(F@FuzzySet),paste("RES",del,"Fuzzy_set ",Nom,sep=""),col.names=FALSE,row.names=FALSE,append=TRUE)
	}


write.table("Belief mass is on the left; sets are reprensented in binary coding.",paste("RES",del,"SMCag ",Nom,sep=""),row.names=FALSE,col.names=FALSE,append=FALSE)
write.table(paste("SMC of ",Nom,sep=""),paste("RES",del,"SMCag ",Nom,sep=""),row.names=FALSE,col.names=FALSE,qmethod="double",append=TRUE)

write.table(SMCagr[[i]]@group,paste("RES",del,"SMCag ",Nom,sep=""),row.names=FALSE,col.names=FALSE,append=TRUE)
write.table(SMCagr[[i]]@bba,paste("RES",del,"SMCag ",Nom,sep=""),row.names=FALSE,col.names=FALSE,append=TRUE)



write.table("To each criterion corresponds a bba. The last column represents belief masses; sets are written in binary coding ",paste("RES",del,"bba of ",Nom,sep=""),row.names=FALSE,col.names=FALSE,qmethod="double",append=FALSE)
for(J in 1:length(group_bba[[i]])){
	write.table(paste("bba ",NAMES[J],sep=""),paste("RES",del,"bba of ",Nom,sep=""),row.names=FALSE,col.names=FALSE,qmethod="double",append=TRUE)
	write.table(group_bba[[i]][[J]]@group,paste("RES",del,"bba of ",Nom,sep=""),row.names=FALSE,col.names=FALSE,qmethod="double",append=TRUE)
	write.table(group_bba[[i]][[J]]@bba,paste("RES",del,"bba of ",Nom,sep=""),row.names=FALSE,col.names=FALSE,qmethod="double",append=TRUE)
	}

title=c()
for(m in 1:length(group_SMCag[[i]]$SOURCE$CRITERE[1,])){
	title=c(title,paste("critere",m,sep=""))
	}
title=c(title,"mass","total")

res=data.frame(rbind(title,cbind(group_SMCag[[i]]$SOURCE$CRITERE,group_SMCag[[i]]$SOURCE$MASSE,group_SMCag[[i]]$SOURCE$TOTAL)),row.names=NULL,stringsAsFactors = TRUE)
print(res)

write.table("Origin of SMC. The criteria columns show the agreements between criteria, the MASS column shows the total mass for each rowand the TOTAL column shows the total number of times where the criteria agree with each other.",paste("RES",del,"SMC_origin_of ",Nom,sep=""),row.names=FALSE,col.names=TRUE,qmethod="double",append=FALSE)
write.table(res,paste("RES",del,"SMC_origin_of",Nom,sep=""),row.names=FALSE,col.names=FALSE,qmethod="double",append=TRUE)
print(group_SMCag[[i]]$SOURCE)
	}
if(num==2){
	write.table(paste("Ordering by group with esperances"),paste("RES",del,"SMCOrdering",sep=""),row.names=FALSE,col.names=FALSE,qmethod="double",append=FALSE)
K=objOrdering(SMCagr,fc=c(1,2,3,4,5))	#ordering sources
taille=length(K)
print("sources ordered by reliability")
Expectation1=list()
for(i in 1:length(SMCagr)){
	Expectation1[[i]]=expectation(SMCagr[[i]],c(1,2,3,4,5))	#compute esperances
}
for(i in 1:taille){
	write.table(paste("groupe",i),paste("RES",del,"SMCOrdering",sep=""),row.names=FALSE,col.names=FALSE,qmethod="double",append=TRUE)
	var=paste("set:",i,sep="")
	print(var)
	write.table(paste(source[K[[i]]],Expectation1[K[[i]]],sep=" "),paste("RES",del,"SMCOrdering",sep=""),row.names=FALSE,col.names=FALSE,qmethod="double",append=TRUE)
	print(source[K[[i]]])
	print(K[[i]])
}
print(Expectation1)
	}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
