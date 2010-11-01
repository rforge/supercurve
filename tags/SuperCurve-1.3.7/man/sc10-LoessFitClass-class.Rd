\name{LoessFitClass-class}
\alias{LoessFitClass-class}
\alias{fitted,LoessFitClass-method}
\docType{class}
\title{Class "LoessFitClass"}
\description{
  The LoessFitClass class represents models that were fit with the
  nonparametric model.
}
\usage{
\S4method{fitted}{LoessFitClass}(object, conc, \dots)
} 
\arguments{
  \item{object}{object of class \code{LoessFitClass}}
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
    \item{\code{model}:}{object of class \code{loess} summarizing loess fit}
  }
}
\section{Extends}{
  Class \code{\linkS4class{FitClass}}, directly.
}
\references{KRC}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\seealso{
  \code{\linkS4class{FitClass}}
}   
\keyword{classes}
\keyword{models}

