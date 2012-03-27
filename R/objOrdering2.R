'objOrdering2'<-function(listbba,fc){	#...=list of bbas, fc=function used to compute esperance

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