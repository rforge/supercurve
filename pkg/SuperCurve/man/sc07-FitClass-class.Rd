% $Id$

\name{FitClass-class}
\alias{FitClass-class}
\alias{coef,FitClass-method}
\alias{coefficients,FitClass-method}
\alias{fitSeries,FitClass-method}
\alias{fitSlide,FitClass-method}
\alias{fitted,FitClass-method}
\alias{trimConc,FitClass-method}
\alias{is.FitClass}
\docType{class}
\title{Class "FitClass"}
\description{
  The \code{FitClass} class is a virtual class representing the model that
  was fit in the \code{RPPAFit} routine. Functions for use with \code{FitClass}
  are only to be used internally.  
}
\usage{
is.FitClass(x)
\S4method{coef}{FitClass}(object, \dots)
\S4method{coefficients}{FitClass}(object, \dots)
\S4method{fitSeries}{FitClass}(object,
          diln,
          intensity,
          est.conc,
          method="nls",
          silent=TRUE,
          trace=FALSE,
          \dots)
\S4method{fitSlide}{FitClass}(object,
         conc,
         intensity,
         \dots)
\S4method{fitted}{FitClass}(object,
       conc,
       \dots)
\S4method{trimConc}{FitClass}(object,
         conc,
         intensity,
         design,
         trimLevel,
         \dots)
}
\arguments{
  \item{x}{object of (sub)class \code{FitClass}}
  \item{object}{object of (sub)class \code{FitClass}}
  \item{diln}{numeric vector of dilutions for series to be fit}
  \item{intensity}{numeric vector of observed intensities for series to be fit}
  \item{est.conc}{numeric estimated concentration for dilution = 0}
  \item{method}{character string specifying regression method to use to fit the
    series}
  \item{silent}{logical scalar. If \code{TRUE}, report of error messages will
    be suppressed in \code{try(nlsmeth(...))}}
  \item{trace}{logical scalar. Used in \code{nls} method.}
  \item{conc}{numeric vector containing current estimates of concentration for
    each series}
  \item{design}{object of class \code{RPPADesign} describing the layout
    of the array}
  \item{trimLevel}{numeric scalar multiplied to \acronym{MAD}}
  \item{\dots}{extra arguments for generic routines}
}
\section{Objects from the Class}{
  This class should not be instantiated directly; extend this class instead.
}
\section{Methods}{
  \describe{
    \item{coef}{
      \code{signature(object = "FitClass")}:\cr
      Placeholder method which should be implemented by subclass if appropriate
      for the particular model.}
    \item{coefficients}{
      \code{signature(object = "FitClass")}:\cr
      An alias for \code{coef}.}
    \item{fitSeries}{
      \code{signature(object = "FitClass")}:\cr
      Placeholder method which must be implemented by subclass.}
    \item{fitSlide}{
      \code{signature(object = "FitClass")}:\cr
      Placeholder method which must be implemented by subclass.}
    \item{fitted}{
      \code{signature(object = "FitClass")}:\cr
      Placeholder method which must be implemented by subclass.}
    \item{trimConc}{
      \code{signature(object = "FitClass")}:\cr
      Placeholder method which must be implemented by subclass.}
  }
}
\value{
  The \code{is.FitClass} method returns \code{TRUE} if its
  argument is an object of subclass of class \code{FitClass}.

  The \code{coef} and \code{coefficients} methods return \code{NULL}.
}
\references{KRC}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\keyword{classes}
\keyword{models}
\keyword{nonlinear}
\keyword{regression}
\keyword{robust}

