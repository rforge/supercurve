\name{FitClass-class}
\alias{FitClass-class}
\alias{CobsFitClass-class}
\alias{LoessFitClass-class}
\alias{LogisticFitClass-class}
\alias{fitSeries,FitClass-method}
\alias{fitSlide,FitClass-method}
\alias{trimConc,FitClass-method}
\alias{fitted,CobsFitClass-method}
\alias{fitted,LoessFitClass-method}
\alias{fitted,LogisticFitClass-method}
\docType{class}
\title{The FitClass Class}
\description{
  The FitClass class represents TBD.
}
\usage{
\S4method{fitSeries}{FitClass}(object, diln, intensity, est.conc, method="nls",
                               silent=TRUE, trace=FALSE, \dots)
\S4method{fitSlide}{FitClass}(object, conc, intensity, \dots)
\S4method{trimConc}{FitClass}(object, conc, intensity, design, \dots)
\S4method{fitted}{CobsFitClass}(object, conc, \dots)
\S4method{fitted}{LoessFitClass}(object, conc, \dots)
\S4method{fitted}{LogisticFitClass}(object, conc, \dots)
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
    \item{fitted(object, conc, \dots)}{TBD}
  }
}
\references{KRC}
\author{P. Roebuck, \email{plroebuck@mdanderson.org}}
\keyword{models}
\keyword{regression}
\keyword{nonlinear}
\keyword{robust}

