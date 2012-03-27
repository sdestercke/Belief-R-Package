"critVal"=function(vect){

if(length(vect)==2){
data(CitationnbANDageAge)
u="NA"
if(is.na(vect[1])|| is.na(vect[2])){#is.na(Nb)|| is.na(Age)){
	u="unknown"}
else{
	Nbcitation=row.names(CitationnbANDageAge)
	citclasses=strsplit(Nbcitation,"citations")
	# last element is special (+40)
	last=strsplit(citclasses[[length(citclasses)]][1],split="+",fixed=TRUE)[[1]][2]

	# check if Nb is > last, and return last element position if
	# true, otherwise NA
	nbclass=ifelse(vect[1]>last,length(citclasses),NA)

	# check other classes
	scl=sapply(citclasses,FUN=function(te){match(vect[1],eval(parse(text=te)))})
	scl=scl[-length(citclasses)]
	indcl=1:(length(citclasses)-1)
	indcl=indcl[!is.na(scl)]
	if (length(indcl)>0) nbclass=indcl[1]	
	print(nbclass)

	age=names(CitationnbANDageAge)
	ageclasses=strsplit(age,"years")
	last=strsplit(ageclasses[[length(ageclasses)]][1],split="+",fixed=TRUE)[[1]][2]
	ageclass=ifelse(vect[2]>last,length(ageclasses),NA)
	scl=sapply(ageclasses,FUN=function(te){match(vect[2],eval(parse(text=te)))})
	scl=scl[-length(ageclasses)]
	indcl=1:(length(ageclasses)-1)
	indcl=indcl[!is.na(scl)]
	if (length(indcl)>0) ageclass=indcl[1]
	print(ageclass)
	#age classes	
	u=CitationnbANDageAge[nbclass,ageclass]
	}
return(u)
}
else{
if(is.na(vect)){
	v="unknown"
	}
else{
	data(Repetition)
	data(TypeOfSource)
	v=NA
	if(sum(as.numeric(names(Repetition)==vect))==1){
	v=Repetition[1,vect]
	}
	if(sum(as.numeric(names(TypeOfSource)==vect))==1){
	v=TypeOfSource[1,vect]
	}
	}
return(v)
}
}