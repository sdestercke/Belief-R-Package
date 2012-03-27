Pignistic<-function(pig,Group){

if(abs(sum(pig)-1)<0.000001 && length(Group[,1]==length(pig))){
	return(new("Pignistic",bba=pig,group=Group))
	}
else{
	print("error in input arguments")
	}
}