Fuzzyset<-function(fset){

boolean=TRUE
for(i in 1:length(fset)){
	if(fset[i]>(1+1e-8) || fset[i]<(0-1e-8)){	#check that each element is between  0 and 1
		boolean=FALSE
		}
	}
if(boolean==TRUE){
	return(new("Fuzzyset",FuzzySet=fset))
	}
else{
	print("error in input argument, membreship function val outside [0,1]")
	}
}