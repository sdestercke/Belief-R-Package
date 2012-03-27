library(graphics)
library(methods)
setClass("BBA",
	representation(group="matrix",bba="vector"),
	prototype(group=matrix(0),bba=c(0)),
	)
setMethod("plot",signature(x="BBA"),
	function(x,y="missing"){

BBA=x
mass_emptyset=0
m=mtoBBA(BBAtom(BBA))
if(sum(m@group[1,])==0){
	mass_emptyset=m@bba[1]
	if(length(m@bba==2)){
		m@bba=m@bba[-1]
		m@group=matrix(m@group[-1,],nrow=1)
		}
	else{
		m@group=m@group[-1,]
		m@bba=m@bba[-1]
	}
	}
t=length(m@group[1,])
k=length(m@bba)

print(mass_emptyset)

COLOR=c("blue","red","green")

matplot(c(-0.5,5.5),c(0,1.5),type='n',xlab=expression(theta),ylab='m',main='BBA representation')#+0.1*k
abline(1,0,col='red')
H=0
abline(1,0)
for(j in 1:length(m@bba)){
	Nb=c()
	c=c(m@group[j,],0)
	Vect=(1:(t+1))*c
	for(i in 1:t){
		if(c[i]==0){
			Nb=c()
			}
		else{
			Nb=c(Nb,Vect[i])
			if(i!=7 && c[i+1]==0){
				a=min(Nb)-0.5
				b=max(Nb)+0.5
				rect(a,H,b,H+m@bba[j],col=COLOR[(j%%3)+1])
				
				}
			}	
		}
	H=H+m@bba[j]
	}
# use plotmath symbols to display symbols
legend(-0.5,1.25,c(expression(paste("mass(",symbol("\306"),")=")),mass_emptyset))



})
setMethod("show",signature("BBA"),
	function(object){

if(length(object@bba)>=1){
	taille=length(object@group[1,])
	theta=c()
	y=1:taille
	for(i in 1:taille){
		theta=c(theta,paste("theta ",i,sep=""))
		}
	taille=length(object@bba)
	for(i in 1:taille){
		ENS=theta[y*object@group[i,]]
		mass=object@bba[i]
		m=paste("Subset",paste("{",ENS,"}",collapse=""),"has a mass equal to", mass , collapse="")
		print(m)
		}
	}
else{
print("m was not calculated")
	}
})
setMethod("summary",signature("BBA"),
	function(object){

if(length(object@bba)!=1){
	taille=length(object@group[1,])
	theta=c()
	y=1:taille
	for(i in 1:taille){
		theta=c(theta,paste("theta ",i,sep=""))
		}
	taille=length(object@bba)
	for(i in 1:taille){
		ENS=theta[y*object@group[i,]]
		mass=object@bba[i]
		bel=belCalc(object,object@group[i,])
		pl=plausCalc(object,object@group[i,])
		m=paste("Subset",paste("{",ENS,"}",collapse=""),"has a mass equal to", mass ," pl=",pl," bel=",bel, collapse="")
		print(m)
		}
	}
else{
print("m was not calculated")
	}
})

