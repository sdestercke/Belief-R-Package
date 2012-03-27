\name{critVal}
\alias{critVal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{function for script test (internal use)}
\description{
Function used in the scripted testing scenario
}
\usage{
critVal(vect)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vect}{
Firs argument
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
N. Maillet, B. Charnomordic, S. Destercke
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 See Also as \code{\link{SCRIPTTEST}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (vect) 
{
    if (length(vect) == 2) {
        nbdecitationEtAge = read.table(paste(".", del, "DATA", 
            del, "CitationnbANDage_FS_Modalities.txt", sep = ""), 
            header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
        u = "NA"
        if (is.na(vect[1]) || is.na(vect[2])) {
            u = "unknown"
        }
        else {
            Nbcitation = row.names(nbdecitationEtAge)
            citclasses = strsplit(Nbcitation, "citations")
            last = strsplit(citclasses[[length(citclasses)]][1], 
                split = "+", fixed = TRUE)[[1]][2]
            nbclass = ifelse(vect[1] > last, length(citclasses), 
                NA)
            scl = sapply(citclasses, FUN = function(te) {
                match(vect[1], eval(parse(text = te)))
            })
            scl = scl[-length(citclasses)]
            indcl = 1:(length(citclasses) - 1)
            indcl = indcl[!is.na(scl)]
            if (length(indcl) > 0) 
                nbclass = indcl[1]
            print(nbclass)
            age = names(nbdecitationEtAge)
            ageclasses = strsplit(age, "years")
            last = strsplit(ageclasses[[length(ageclasses)]][1], 
                split = "+", fixed = TRUE)[[1]][2]
            ageclass = ifelse(vect[2] > last, length(ageclasses), 
                NA)
            scl = sapply(ageclasses, FUN = function(te) {
                match(vect[2], eval(parse(text = te)))
            })
            scl = scl[-length(ageclasses)]
            indcl = 1:(length(ageclasses) - 1)
            indcl = indcl[!is.na(scl)]
            if (length(indcl) > 0) 
                ageclass = indcl[1]
            print(ageclass)
            u = nbdecitationEtAge[nbclass, ageclass]
        }
        return(u)
    }
    else {
        if (is.na(vect)) {
            v = "unknown"
        }
        else {
            repetition = read.table(paste(".", del, "DATA", del, 
                "RepetitionANDModalities.txt", sep = ""), header = TRUE, 
                stringsAsFactors = FALSE)
            typeDeSource = read.table(paste(".", del, "DATA", 
                del, "TypeOfSource_FS_Modalities.txt", sep = ""), 
                header = TRUE, stringsAsFactors = FALSE)
            v = NA
            if (sum(as.numeric(names(repetition) == vect)) == 
                1) {
                v = repetition[1, vect]
            }
            if (sum(as.numeric(names(typeDeSource) == vect)) == 
                1) {
                v = typeDeSource[1, vect]
            }
        }
        return(v)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
