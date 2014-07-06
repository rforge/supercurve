% $Id$

\name{CobsFitClass-class}
\alias{CobsFitClass-class}
\alias{fitted,CobsFitClass-method}
\docType{class}
\title{Class "CobsFitClass"}
\description{
  The CobsFitClass class represents models that were fit with the
  nonparametric model.
}
\usage{
\S4method{fitted}{CobsFitClass}(object, conc, \dots)
}
\arguments{
  \item{object}{object of class \code{CobsFitClass}}
  \item{conc}{numeric vector containing estimates of the log concentration
    for each dilution series}
  \item{\dots}{extra arguments for generic routines}
}
\section{Methods}{
  \describe{
    \item{fitted(object, conc, \dots)}{Extracts fitted values of the model} 
  }
}
\value{
  The \code{coef} and \code{coefficients} methods return \code{NULL}.

  The \code{fitted} method returns a numeric vector.
}
\section{Objects from the Class}{
  Objects are created internally by calls to the methods
  \code{\link[=FitClass-class]{fitSlide}} or \code{\link{RPPAFit}}.
}
\section{Slots}{
  \describe{
    \item{\code{model}:}{object of class \code{cobs} summarizing nonparametric
      fit}
    \item{\code{lambda}:}{numeric}
  }
}
\section{Extends}{
  Class \code{\linkS4class{FitClass}}, directly.
}
\references{
  Hu J, He X, Baggerly KA, Coombes KR, Hennessy BT, Mills GB.\cr
  "Non-parametric Quantification of Protein Lysate Arrays"\cr
  Bioinformatics (2007) 23(15): 1986-1994.\cr
  \url{http://bioinformatics.oxfordjournals.org/content/23/15/1986}
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\linkS4class{FitClass}}
}
\keyword{classes}
\keyword{models}

