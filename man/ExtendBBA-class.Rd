\name{ExtendBBA-class}
\Rdversion{1.1}
\docType{class}
\alias{ExtendBBA-class}
\alias{ExtendBBAClass}
\alias{plot,ExtendBBA-method}
\alias{show,ExtendBBA-method}
\alias{summary,ExtendBBA-method}

\title{Class "ExtendBBA"}
\description{ExtendBBA is a class whose objects are BBA coded in an extended way. That is, it encodes a BBA by a vector counting all \eqn{2^n} elements of the power set of a space counting \eqn{n} elements. 

An ExtendBBA can contain the information in terms of mass assignment and belief/ plausibility/ commonality/ implicability functions value.

The \eqn{n^{th}} element of each vector will contain the value corresponding to the subset whose binary encoding correspond to decimal number \eqn{n} (i.e., (0,0,0,0,\dots) for the first element, (1,0,0,0,\dots) for the second element, (0,1,0,0,\dots) for the third, (1,1,0,0,\dots) for the fourth, and so on\dots). } 
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ExtendBBA", ...)}. 
}
\section{Slots}{
  \describe{
    \item{\code{bba}:}{Object of class \code{"vector"}, containing the mass assignment information.}
    \item{\code{bel}:}{Object of class \code{"vector"}, containing the belief measure information. }
    \item{\code{pl}:}{Object of class \code{"vector"}, containing the plausibility measure information.}
    \item{\code{q}:}{Object of class \code{"vector"}, containing the commonality measure information. }
    \item{\code{b}:}{Object of class \code{"vector"}, containing the implicability measure information. }
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "ExtendBBA")}: ... }
    \item{show}{\code{signature(object = "ExtendBBA")}: ... }
    \item{summary}{\code{signature(object = "ExtendBBA")}: ... }
	 }
}
\references{ P. Smets (2002) \emph{The application of the matrix calculus to belief functions}. Int. J. Approx. Reasoning \bold{31(1-2): 1-30} }
\author{N. Maillet, B. Charnomordic, S. Destercke}
\note{Class can (should?) be extended to contain both the v and w transformations introduced by Denoeux (IA, 2008).}

\section{Warning }{....}

\seealso{
\code{\link{ExtendBBA}}
}
\examples{
showClass("ExtendBBA")
}
\keyword{classes}
