\name{BBA-class}
\Rdversion{1.1}
\docType{class}
\alias{BBA-class}
\alias{BBAClass}
\alias{plot,BBA-method}
\alias{show,BBA-method}
\alias{summary,BBA-method}

\title{Class "BBA"}
\description{ The class contains objects that are Basic Belief Assignments, encoded in an economic way (only focal elements are stored)}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BBA", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{group}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{bba}:}{Object of class \code{"vector"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "BBA")}: ... }
    \item{show}{\code{signature(object = "BBA")}: ... }
    \item{summary}{\code{signature(object = "BBA")}: ... }
	 }
}
\references{ 

P. Smets, R. Kennes, (1994) \emph{The Transferable Belief Model}. Artif. Intell. \bold{66(2): 191-234}

\url{http://iridia.ulb.ac.be/~psmets/}
 }
\author{ N. Maillet, B. Charnomordic, S. Destercke }
%\note{ }

\section{Warning }{....}

\seealso{
	\code{\link{ExtendBBAClass}}}
\examples{
showClass("BBA")
}
\keyword{classes}
