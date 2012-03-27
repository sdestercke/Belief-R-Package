BBA<-function(Group,Bba){

if(abs(sum(Bba)-1)<0.000001 && (length(Group[,1])==length(Bba))){
	return(new("BBA",group=Group,bba=Bba))
	}
else{
	print("error in input arguments")
	}
}