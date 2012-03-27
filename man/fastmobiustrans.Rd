\name{fastmobiustrans}
\alias{fmtqtopl}
\alias{fmtqtob}
\alias{fmtqtobel}
\alias{fmtqtom}
\alias{fmtmtoq}
\alias{fmtmtob}
\alias{fmtmtonm}
\alias{fmtmtobel}
\alias{fmtmtopl}
\alias{fmtpltoq}
\alias{fmtpltom}
\alias{fmtpltob}
\alias{fmtpltobel}
\alias{fmtbeltopl}
\alias{fmtbeltoq}
\alias{fmtbeltob}
\alias{fmtbeltom}
\alias{fmtbtopl}
\alias{fmtbtom}
\alias{fmtbtobel}
\alias{fmtbtoq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fast Mobius Transform}
\description{Uses the Fast Mobius Transformation to go from one
representation (measure or BBA) to another one (measure or BBA)}
\usage{
fmtqtopl(m)
fmtqtob(m)
fmtqtobel(m)
fmtqtom(m)
fmtmtoq(m)
fmtmtob(m)
fmtmtobel(m)
fmtmtopl(m)
fmtmtonm(m)
fmtpltoq(m)
fmtpltom(m)
fmtpltob(m)
fmtpltobel(m)
fmtbeltopl(m)
fmtbeltoq(m)
fmtbeltob(m)
fmtbeltom(m)
fmtbtopl(m)
fmtbtobel(m)
fmtbtoq(m)
fmtbtom(m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{object of the class extendBBA, where the initial representation is provided.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{Returns the object with the goal transformation completed. 
}
\references{
P. Smets (2002) \emph{The application of the matrix calculus to belief functions}. Int. J. Approx. Reasoning \bold{31(1-2): 1-30}

Wilson, N. (2000), \emph{Algorithms for Dempster-Shafer Theory}, in Kohlas, J., andMoral, S., (eds.), Volume 5: Algorithms for Uncertainty and DefeasibleReasoning, Handbook of Defeasible Reasoning and UncertaintyManagement Systems, (Series eds.: D. Gabbay, P. Smets), Kluwer AcademicPublishers.

Kennes R.(1992) \emph{Computational Aspects of the Mobius Transformation of Graphs.}
IEEE Trans. SMC. \bold{22: 201-223}.
}
\author{N. Maillet, B. Charnomordic, S. Destercke}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ExtendBBA}}
}
\examples{
m=c(0,0,0.5,0,0.3,0,0,0.2)
m=ExtendBBA(m)
m@q
fmtmtoq(m)
m@q
m@pl
fmtqtopl(m)
m@pl


## The function is currently defined as
function(m){

Q=m
Q@q[1]=0
Q@b=Q@q
Q@bba=0
pl=abs(fmtbtom(Q))
x <- deparse(substitute(m))
		assign(x,ExtendBBA(BBA=m@bba,Q=m@q,Bel=m@bel,Pl=pl,B=m@b), pos=.GlobalEnv)
		return(pl)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
