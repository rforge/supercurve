\name{CobsFitClass-class}
\alias{CobsFitClass-class}
\alias{fitted,CobsFitClass-method}
\docType{class}
\title{Class "CobsFitClass"}
\description{
  The CobsFitClass class represents models that were fit with the nonparametric model.
}
\usage{
\S4method{fitted}{CobsFitClass}(object, conc, \dots)
}
\arguments{
  \item{object}{object of class \code{CobsFitClass}}
  \item{conc}{numeric vector containing estimates of the log concentration for each dilution series}
  \item{\dots}{extra arguments for generic routines}
}
%\section{Methods}{
%  \describe{
%    \item{fitted(object, conc, \dots)}{TBD}
%  }
%}
\section{Objects from the Class}{
  Objects are created internally by calls to the methods
  \code{\link[=FitClass-class]{fitSlide}} or \code{\link{RPPAFit}}.
}
\section{Slots}{
  \describe{
    \item{\code{model}:}{Object of class \code{cobs} summarizing nonparametric fit }
    \item{\code{lambda}:}{numeric}
  }
}
\section{Extends}{
  Class \code{\linkS4class{FitClass}}, directly.
}
\references{KRC}
\author{P. Roebuck \email{plroebuck@mdanderson.org}}
\seealso{
  \code{\linkS4class{FitClass}}
}
\keyword{classes}
\keyword{models}

