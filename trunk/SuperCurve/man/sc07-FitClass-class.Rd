\name{FitClass-class}
\alias{FitClass-class}
\alias{fitSeries,FitClass-method}
\alias{fitSlide,FitClass-method}
\alias{trimConc,FitClass-method}
\docType{class}
\title{Class "FitClass"}
\description{
  The FitClass class represents the model that was fit in the \code{RPPAFit}
  routine. Functions for use with the FitClass are only to be used internally.  
}
\usage{
\S4method{fitSeries}{FitClass}(object, diln, intensity, est.conc, method="nls",
                               silent=TRUE, trace=FALSE, \dots)
\S4method{fitSlide}{FitClass}(object, conc, intensity, \dots)
\S4method{trimConc}{FitClass}(object, conc, intensity, design, \dots)
}
\arguments{
  \item{object}{object of (sub)class \code{FitClass}}
  \item{diln}{numeric vector of dilutions for series to be fit}
  \item{intensity}{numeric vector of observed intensities for series to be fit}
  \item{est.conc}{numeric estimated concentration for dilution = 0}
  \item{method}{character string specifying regression method to use fit the
    series}
  \item{silent}{logical scalar. If \code{TRUE}, report of error messages will
    not be suppressed in try(nlsmeth(...).)
  \item{trace}{logical scalar. Used in \code{nls} method.}
  \item{conc}{numeric vector containing current estimates of concentration for
    each series}
  \item{design}{object of class \code{RPPADesign} describing the layout
    of the array}
  \item{\dots}{extra arguments for generic routines}
}
\section{Methods}{
  \describe{
    \item{fitSeries(object, diln, intensity, est.conc, method="nls",
      silent=TRUE, trace=FALSE, \dots)}{Finds the concentration for an
      individual dilution series given the curve fit for the slide.}
    \item{fitSlide(object, conc, intensity, \dots)}{Uses the concentration and
      intensity series for an entire slide to fit a curve for the slide of
      intensity = f(conc).}
    \item{trimConc(object, conc, intensity, design, \dots)}{Returns
      concentration and intensity cutoffs for the model.}
  }
}
\references{KRC}
\author{P. Roebuck \email{plroebuck@mdanderson.org}}
\keyword{models}
\keyword{regression}
\keyword{nonlinear}
\keyword{robust}

