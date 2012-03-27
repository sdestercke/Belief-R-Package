'objectiveTransfo'<-function(vect){	#vect:  vector of probabilities
					#depends: fuzzyset
					#transforms a probability into
					#a fuzzy set using the
					#objective transformation 
taille=length(vect)
ordre=order(vect)
proba=vect[ordre]
mu=cumsum(proba)
mu=mu[ordre]
return(Fuzzyset(mu))

}
