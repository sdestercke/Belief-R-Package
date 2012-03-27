# list of called functions: critVal, fuzzyset, fuzzytobelief, SMCagg, objOrdering, expectation
# list of used data files: 
#CitationnbANDage_FS_Modalities.txt  Table.txt
#ReliabilityCharacterization.txt    TypeOfSource_FS_Modalities.txt
#RepetitionANDModalities.txt

'SCRIPTTEST' <- function(del="/"){

writeFlag <- FALSE			#boolean: TRUE write files, FALSE only print results

nc <- 3	#number of criteria
#read the file that associates criteria and their characterization as fuzzy sets
#Characterization <- data(Characterization)
data(Characterization, package="belief")


#paths for correspondence between criteria and reliability

#read data file
data(Table, package="belief")

#number of arguments for each criterion
nbParCrit=c(2,1,1)

group_fuzzy_set=list()	#init objects
group_bba=list()
group_SMCag=list()
SMCagr=list()
name=list()
source=Table[,1]

NAMES=c("citation_number_and_age","repetitions","source_type") #Criteria names

k=nrow(Table)

for(i in 1:k){
	print(Table[i,])
	Nom=Table[i,"source_name"]
	typ=Table[i,"source_type"]
	Nb=Table[i,"citation_number"]
	Age=Table[i,"age"]
	rep=Table[i,"repetitions"]

	CritVal=c(Nb,Age,rep,typ)	#vector holding criterion values
	Nbna=0

	for(p in 1:nc){					#for each criterion compute fuzzy set and reliability
		crit=CritVal[1:nbParCrit[p]]
		CritVal=CritVal[-(1:nbParCrit[p])]
		if(sum(as.numeric(is.na(crit)))!=0){
			Nbna=Nbna+1}
		u=critVal(crit)	#for each criterion compute belief mass
		uu=strsplit(u,"_or_")
		if(length(uu[[1]])==2){		#particular case: two possible correspondences for reliability because of or string
			reliab1=uu[[1]][[1]]
			reliab2=uu[[1]][[2]]
			Fuzzy1=pmax(Characterization[[reliab1]],Characterization[[reliab2]])	#case where or is used as in reliable or very reliable
		} else{
			Fuzzy1=Characterization[[u]]
		}
		assign(paste("Fuz",p,sep=""),Fuzzyset(Fuzzy1))
		assign(paste("bba",p,sep=""),fuzzytobelief(get(paste("Fuz",p,sep=""))))
		}

	name1=names(Table)
	name[[i]]=name1

	Fuzzy_Set=list()
	ENS_bba=list()
	for(nbCrit in 1:nc){						#completion of objects
		Fuzzy_Set=c(Fuzzy_Set,get(paste('Fuz',nbCrit,sep="")))
		ENS_bba=c(ENS_bba,get(paste('bba',nbCrit,sep="")))
		}
	group_fuzzy_set[[i]]=Fuzzy_Set
	group_bba[[i]]=ENS_bba

	group_SMCag[[i]]=SMCagg(ENS_bba)		#compute SMC for criteria
	SMCagr[[i]]=discounting(group_SMCag[[i]]$SMC,(1-Nbna/nc))#group_SMCag[[i]]$SMC#


	if(writeFlag==TRUE){
		writefile(group_fuzzy_set,SMCagr,group_bba,group_SMCag,del,Nom)
	}
}# end of loop on data rows
if(writeFlag==TRUE){
		writefile(group_fuzzy_set,SMCagr,group_bba,group_SMCag,del,Nom,num=2)
		}


K=objOrdering(SMCagr,fc=c(1,2,3,4,5))	#ordering sources

size=length(K)
print("sources sorted by decreasing reliability")
Exp1=list()
for(i in 1:length(SMCagr)){
	Exp1[[i]]=expectation(SMCagr[[i]],c(1,2,3,4,5))	#compute expectations
}
for(i in 1:size){
	var=paste("set: ",i,sep="")
	print(var)
	print(source[K[[i]]])
	print(K[[i]])
}
}

#see plot for fuzzy set num2 from source num1: plot(group_fuzzy_set[[num1]][[num2]])
#see plot for bba num2 from source num1 : plot(group_bba[[num1]][[num2]])
#see plot for SMC aggregation  num : plot(group_SMCag[[num1]]$SMC)
#print SMC result: print(group_SMCag[[num]]$SMC)
#print SMC detailed results: DetailSMC(group_SMCag[[num1]],group_bba[[num1]],num2)