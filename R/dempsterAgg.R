'dempsterAgg'<-function(...){	#Dempster aggregation of n elements
					#depends: DempsterAgg2
					#recursive use of Dempster fusion
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
			
			Ag=dempsterAgg2(Ag,x[[1]])	#aggregate 2
			#elements, remove element from list and
			#decrement counter
			x=x[-1]
			n=n-1
		}
	return(Ag)		#returns Dempster aggregation
	}
}