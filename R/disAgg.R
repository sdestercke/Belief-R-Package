'disAgg'<-function(...){	#disjunctive aggregation of n elements
				#depends: DisAg2
				#recursive use of disjunctive fusion
x=list(...)
Ag=x[[1]]
n=length(x)
x=x[-1]
if(n==1){
	return(x[[1]])
	}
else{
	n=n-1
	while(n!=0){
			Ag=disAgg2(Ag,x[[1]])	#aggregate 2 elements,
			x=x[-1]
			n=n-1
			#remove first element from x 
			#and decrement counter by 1
		}
	return(Ag)	#disjunctive aggregation 
	}
}