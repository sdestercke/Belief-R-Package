\name{fuzzyset-class}
\Rdversion{1.1}
\docType{class}
\alias{Fuzzyset-class}
\alias{Fuzzysetclass}
\alias{plot,Fuzzyset-method}
\alias{show,Fuzzyset-method}
\alias{summary,Fuzzyset-method}

\title{Class "fuzzyset"}
\description{	Class describing fuzzy sets by their membership function values}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("fuzzyset", ...)}.
	 
}
\section{Slots}{
  \describe{
    \item{\code{FuzzySet}:}{Object of class \code{"vector"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "fuzzyset")}: ... }
    \item{show}{\code{signature(object = "fuzzyset")}: ... }
    \item{summary}{\code{signature(object = "fuzzyset")}: ... }
	 }
}
\references{ D. Dubois, H. Prade (1988) \emph{Fuzzy Sets and Systems.} Academic Press, New York. }
\author{ N. Maillet, B. Charnomordic, S. Destercke }
%\note{  }

\section{Warning }{....}

\seealso{
\code{\link{Fuzzyset}}
}
\examples{
showClass("Fuzzyset")
}
\keyword{classes}
