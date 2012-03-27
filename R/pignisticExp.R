'pignisticExp'<-function(BBA,c){	#BBA structure, c: vector f(theta_i)
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