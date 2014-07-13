% $Id$

\name{LoessFitClass-class}
\alias{LoessFitClass-class}
\alias{fitSeries,LoessFitClass-method}
\alias{fitSlide,LoessFitClass-method}
\alias{fitted,LoessFitClass-method}
\alias{trimConc,LoessFitClass-method}
\docType{class}
\title{Class "LoessFitClass"}
\description{
  The \code{LoessFitClass} class represents models that were fit with the
  nonparametric model.
}
\usage{
\S4method{fitSeries}{LoessFitClass}(object,
          diln,
          intensity,
          est.conc,
          method="nls",
          silent=TRUE,
          trace=FALSE,
          \dots)
\S4method{fitSlide}{LoessFitClass}(object,
         conc,
         intensity,
         \dots)
\S4method{fitted}{LoessFitClass}(object,
       conc,
       \dots)
\S4method{trimConc}{LoessFitClass}(object,
         conc,
         intensity,
         design,
         trimLevel,
         \dots)
} 
\arguments{
  \item{object}{object of class \code{LoessFitClass}}
  \item{diln}{numeric vector of dilutions for series to be fit}
  \item{intensity}{numeric vector of observed intensities for series to be fit}
  \item{est.conc}{numeric estimated concentration for dilution = 0}
  \item{method}{character string specifying regression method to use to fit the
    series}
  \item{silent}{logical scalar. If \code{TRUE}, report of error messages will
    be suppressed in \code{try(nlsmeth(...))}}
  \item{trace}{logical scalar. Used in \code{nls} method.}
  \item{conc}{numeric vector containing estimates of the log concentration
    for each dilution series}
  \item{design}{object of class \code{RPPADesign} describing the layout
    of the array}
  \item{trimLevel}{numeric scalar multiplied to \acronym{MAD}}
  \item{\dots}{extra arguments for generic routines}
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
\section{Methods}{
  \describe{
    \item{fitSeries}{
      \code{signature(object = "LoessFitClass")}:\cr
      Finds the concentration for an individual dilution series given the
      curve fit for the slide.}
    \item{fitSlide}{
      \code{signature(object = "LoessFitClass")}:\cr
      Uses the concentration and intensity series for an entire slide to
      fit a curve for the slide of intensity = f(conc).}
    \item{fitted}{
      \code{signature(object = "LoessFitClass")}:\cr
      Extracts fitted values of the model.} 
    \item{trimConc}{
      \code{signature(object = "LoessFitClass")}:\cr
      Returns concentration and intensity cutoffs for the model.}
  }
}
\value{
  The \code{fitted} method returns a numeric vector.
}
\references{KRC}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\linkS4class{FitClass}}
}   
\keyword{classes}
\keyword{models}

