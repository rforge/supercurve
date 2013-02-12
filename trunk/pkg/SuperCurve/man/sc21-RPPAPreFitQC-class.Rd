\name{RPPAPreFitQC-class}
\alias{RPPAPreFitQC-class}
\alias{RPPAPreFitQC}
\alias{is.RPPAPreFitQC}
\alias{qcprob,RPPAPreFitQC-method}
\alias{summary,RPPAPreFitQC-method}
\docType{class}
\title{Class "RPPAPreFitQC"}
\description{
  The RPPAPreFitQC class represents the inputs necessary to determine the
  quality control rating of a reverse-phase protein array slide.
}
\usage{
RPPAPreFitQC(rppa, design, useAdjusted=TRUE)
is.RPPAPreFitQC(x)
\S4method{qcprob}{RPPAPreFitQC}(object, \dots)
\S4method{summary}{RPPAPreFitQC}(object, \dots)
}
\arguments{
  \item{rppa}{object of class \code{\link{RPPA}} containing the raw data
    to be assessed}
  \item{design}{object of class \code{\link{RPPADesign}} describing the layout
    of the array}
  \item{useAdjusted}{logical scalar. If \code{TRUE}, spatially adjusted
    measures are used instead of \code{Mean.Net} and \code{Mean.Total}.}
  \item{object}{object of (sub)class \code{RPPAPreFitQC}}
  \item{x}{object of (sub)class \code{RPPAPreFitQC}}
  \item{\dots}{extra arguments for generic routines}
}
\section{Objects from the Class}{
  Objects are created by calls to the \code{RPPAPreFitQC} factory method.
}
\section{Methods}{
  \describe{
    \item{qcprob(object, \dots)}{Calculates the probability of good slide.}
    \item{summary(object, \dots)}{Prints a summary of the underlying
      data frame.}
  }
}
\value{
  The \code{RPPAPreFitQC} generator returns an object of subclass of class
  \code{RPPAPreFitQC}.

  The \code{is.RPPAPreFitQC} method returns \code{TRUE} if its
  argument is an object of subclass of class \code{RPPAPreFitQC}.

  The \code{summary} method returns a summary of the underlying data frame.
}
\section{Warning}{
  The current implementation is only capable of handling designs with 5
  dilution series. Anything else will fail.
}
\references{KRC}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\examples{
extdatadir <- system.file("extdata", package="SuperCurve")

txtdir <- file.path(extdatadir, "rppaSingleSubgridData")
rppa <- RPPA("Foo.txt", path=txtdir, software="singlesubgrid")
designfile <- file.path(txtdir, "slidedesign.tsv")
design <- RPPADesign(rppa, designfile=designfile)
rppa <- spatialAdjustment(rppa, design)
fitqc <- RPPAPreFitQC(rppa, design)
summary(fitqc)
qcprob(fitqc)
}
\keyword{classes}

