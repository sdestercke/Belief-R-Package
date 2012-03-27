\name{Pignistic-class}
\Rdversion{1.1}
\docType{class}
\alias{Pignistic-class}
\alias{Pignisticclass}
\alias{plot,Pignistic-method}
\alias{show,Pignistic-method}
\alias{summary,Pignistic-method}

\title{Class "Pignistic"}
\description{	 Class Pignistic, corresponding to probabilistic BBAs}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Pignistic", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{bba}:}{Object of class \code{"vector"}, containing the pignistic probability masses}
    \item{\code{group}:}{Object of class \code{"matrix"}, containing elements to which are given masses.}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "Pignistic")}: ... }
    \item{show}{\code{signature(object = "Pignistic")}: ... }
    \item{summary}{\code{signature(object = "Pignistic")}: ... }
	 }
}
\references{ P. Smets (2005) \emph{Decision making in the TBM: the necessity of the pignistic transformation.} Int. J. Approx. Reasoning \bold{38(2): 133-147}}
\author{ N. Maillet, B. Charnomordic, S. Destercke  }
%\note{ ~~further notes~~ }

%\section{Warning }{....}

\seealso{
\code{\link{pignistic}}
}
\examples{
showClass("Pignistic")
}
\keyword{classes}
