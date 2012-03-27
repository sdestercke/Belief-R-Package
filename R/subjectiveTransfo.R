'subjectiveTransfo'<-function(vect){	#vect: vector of probabilities
					#depends: fuzzyset
					#passes from probabilities to
					#fuzzy set using the subjective transformation
vect2=vect
mu=vect
for(i in 1:length(vect)){
	mu[i]=sum(pmin(vect[i],vect))
	}
return(Fuzzyset(mu))
}

