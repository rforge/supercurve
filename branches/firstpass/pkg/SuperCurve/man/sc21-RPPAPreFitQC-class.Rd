% $Id$

\name{RPPAPreFitQC-class}
\alias{RPPAPreFitQC-class}
\alias{RPPAPreFitQC}
\alias{is.RPPAPreFitQC}
\alias{qcprob,RPPAPreFitQC-method}
\alias{summary,RPPAPreFitQC-method}
\docType{class}
\title{Class \dQuote{RPPAPreFitQC}}
\description{
  The RPPAPreFitQC class represents the inputs necessary to determine the
  quality control rating of a reverse-phase protein array slide.
}
\usage{
RPPAPreFitQC(rppa, design, useAdjusted=FALSE)
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
    \item{qcprob}{ 
      \code{signature(object = "RPPAPreFitQC")}:\cr 
      Placeholder method which must be implemented by subclass.}
    \item{summary}{
      \code{signature(object = "RPPAPreFitQC")}:\cr
      Placeholder method which must be implemented by subclass.}
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
  \strong{The current implementation only handles designs with 5 dilution
  series.\cr Anything else will fail.}
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\examples{
extdata.dir <- system.file("extdata", package="SuperCurveSampleData")

txtdir <- file.path(extdata.dir, "rppaSingleSubgridData")
rppa <- RPPA("Foo.txt",
             path=txtdir,
             software="microvigene",
             alt.layout="superslide")
designfile <- file.path(txtdir, "slidedesign.tsv")
design <- RPPADesign(rppa, designfile=designfile)
fitqc <- RPPAPreFitQC(rppa, design)
summary(fitqc)
qcprob(fitqc)
}
\keyword{classes}

