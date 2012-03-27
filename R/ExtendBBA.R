ExtendBBA<-function(BBA=0,Bel=0,Pl=0,Q=0,B=0){

s=length(BBA)
n=log2(s)
p=round(n)
if(missing(BBA) && missing(Bel) && missing(Pl) && missing(Q) && missing(B)){
	print("argument not use")
	}
else{
	if(2^p==s){ #&& abs(sum(BBA)-1)<0.0000001
		return(new("ExtendBBA",bba=BBA,bel=Bel,pl=Pl,q=Q,b=B))
		}
	else{
		print("error in input vector")
		}
	}
}