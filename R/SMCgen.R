'SMCgen'=function(interval){	#interval=vector with 2n elements
				#each pair represents an interval, mass=vector of associated masses
				#depends: intersectionint
				#does SMC merging of several intervals
Listg=list()
j=1
K=c()

l=length(interval)/2
c=1:l
source=sort(c(c,c))

q=sort(interval)
p=order(interval)
source=source[p]
type=p%%2	#1 for inf bound, 0 for sup bound

k=1
while(k!=0){
	k=0
	for(i in 1:(length(p)-1)){
		if(q[i]==q[i+1] && type[i]<type[i+1]){
			type[i]=1
			type[i+1]=0
			h=source[i]
			source[i]=source[i+1]
			source[i+1]=h
			k=1
			}
		}
	}

for(i in (1:(length(p)-1))){
	if(type[i]==1){
		K=c(K,source[i])
		if(type[i+1]==0){
			Listg[[j]]=K
			j=j+1
			}
		}
	else{
		ind=1:length(K)
		ind2=as.numeric(K==source[i])
		ind3=ind%*%ind2
		K=K[-ind3]
		}
	}
k=length(Listg)
intersection=list()
for(i in 1:k){
	c=intersectionint(interval,Listg[[i]])
	intersection[[i]]=c
	}
return(list(origin=Listg,intersection=intersection))
}