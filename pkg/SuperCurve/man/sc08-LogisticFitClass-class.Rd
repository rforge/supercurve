% $Id$

\name{LogisticFitClass-class}
\alias{LogisticFitClass-class}
\alias{coef,LogisticFitClass-method}
\alias{coefficients,LogisticFitClass-method}
\alias{fitted,LogisticFitClass-method}
\docType{class}
\title{Class "LogisticFitClass"}
\description{
  The LogisticFitClass class represents models that were fit with the
  logistic model.
}
\usage{
\S4method{coef}{LogisticFitClass}(object, \dots)
\S4method{coefficients}{LogisticFitClass}(object, \dots)
\S4method{fitted}{LogisticFitClass}(object, conc, \dots)
} 
\arguments{
  \item{object}{object of class \code{LogisticFitClass}}
  \item{conc}{numeric vector containing estimates of the log concentration
    for each dilution series}
  \item{\dots}{extra arguments for generic routines}
}
\section{Methods}{
  \describe{
    \item{coef(object, \dots)}{Extracts model coefficients.} 
    \item{coefficients(object, \dots)}{An alias for \code{coef}.}
    \item{fitted(object, conc, \dots)}{Extracts fitted values of the model.} 
  }
}
\value{
  The \code{coef} and \code{coefficients} methods return a named vector
  of length three with logistic curve coefficients.

  The \code{fitted} method returns a numeric vector.
}
\section{Objects from the Class}{
  Objects are created internally by calls to the methods
  \code{\link[=FitClass-class]{fitSlide}} or \code{\link{RPPAFit}}.
} 
\section{Slots}{
  \describe{
    \item{\code{coefficients}:}{numeric vector of length 3, representing
      alpha, beta, and gamma respectively.}
  }
}
\section{Extends}{
  Class \code{\linkS4class{FitClass}}, directly.
}
\references{KRC}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\linkS4class{FitClass}}
}   
\keyword{classes}

