'fromPtoBBAP'<-function(pig){	#argument=output of Pignistic function
					#depends: verifPig and verifbba
	P=length(pig)
	Part1=matrix(0,nrow=P,ncol=P)
	for(i in 1:P){
		Part1[i,i]=1
		}
	Pig2=BBA(Group=Part1,Bba=pig)
		return(Pig2)	#returns pignistic as a BBA
}