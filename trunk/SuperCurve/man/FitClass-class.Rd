\name{FitClass-class}
\alias{FitClass-class}
\alias{fitSeries,FitClass-method}
\alias{fitSlide,FitClass-method}
\alias{trimConc,FitClass-method}
\docType{class}
\title{Class "FitClass"}
\description{
  The FitClass class represents TBD.
}
\usage{
\S4method{fitSeries}{FitClass}(object, diln, intensity, est.conc, method="nls",
                               silent=TRUE, trace=FALSE, \dots)
\S4method{fitSlide}{FitClass}(object, conc, intensity, \dots)
\S4method{trimConc}{FitClass}(object, conc, intensity, design, \dots)
}
\arguments{
  \item{object}{object of (sub)class \code{FitClass}}
  \item{diln}{numeric TBD}
  \item{intensity}{numeric TBD}
  \item{est.conc}{numeric TBD estimated concentration for dilution = 0}
  \item{method}{character string specifying regression method to use TBD}
  \item{silent}{logical scalar. If \code{TRUE}, TBD}
  \item{trace}{logical scalar. If \code{TRUE}, TBD}
  \item{conc}{numeric TBD}
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
\author{P. Roebuck, \email{plroebuck@mdanderson.org}}
\keyword{models}
\keyword{regression}
\keyword{nonlinear}
\keyword{robust}

