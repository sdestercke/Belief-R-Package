\name{belief-package}
\alias{beliefpackage}
\alias{belief}
\alias{packbelief}
\docType{package}
\title{belief function manipulation}
\description{This package contains basic functions to manipulate belief functions and associated mass assignments (currently on finite spaces only). 

It is aimed at providing a basic skeleton for belief function applications using R.
}
\details{
\tabular{ll}{
Package: \tab belief\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-08-25\cr
License: \tab What license is it under?\cr
LazyLoad: \tab yes\cr
}
Currently, the package contains the following features:
\describe{\item{Transformations}{Transformations of any encoding to another using the Fast Mobius Transform} 
\item{Basic merging rules}{Basic merging rules, including TBM conjunctive and disjunctive rule, and SMC rule}
\item{Basic tools}{Discounting, Pignistic transform, Expectation (lower/upper/pignistic) computations.}
\item{FuzzySet related tools}{Transformation of fuzzy sets to bba and vice versa, transformation of probabilities to fuzzy sets}
}

The following classes are implemented:
\describe{\item{BBA}{Basic Belief Assignment with economical encoding}
\item{ExtendBBA}{Basic Belief Assignment encoding all elements of the power set}
\item{Fuzzyset}{Fuzzy sets described by membership functions}
\item{Pignistic}{Pignistic probability}
}
}
\author{
N. Maillet, B. Charnomordic, S. Destercke 

Maintainer: S. Destercke <sebastien.destercke@cirad.fr>, B. Charnomordic <bch@supagro.inra.fr>
}
\references{
P. Smets (1998) \emph{The Transferable Belief Model for Quantified Belief Representation.} Handbook of Defeasible Reasoning and Uncertainty Management Systems. Gabbay D. and Smets Ph. (Series Eds). Ph. Smets (Vol. eds.), \bold{Vol. 1 : Quantified Representation of Uncertainty & Imprecision, Kluwer, Doordrecht 267-301.}
}
\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{
M=matrix(c(0,1,1,0),nrow=2)
V=c(0.4,0.6)
BBA(M,V)
}
