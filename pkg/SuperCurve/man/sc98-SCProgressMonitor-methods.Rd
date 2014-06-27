% $Id$

\name{SCProgressMonitor-methods}
\alias{progressMarquee}
\alias{progressMarquee-method}
\alias{progressMarquee,ANY-method}
\alias{progressMarquee<-}
\alias{progressMarquee<--method}
\alias{progressMarquee<-,ANY-method}
\alias{progressStage}
\alias{progressStage-method}
\alias{progressStage,ANY-method}
\alias{progressStage<-}
\alias{progressStage<--method}
\alias{progressStage<-,ANY-method}
\docType{methods}
\title{Methods for Manipulating SuperCurve Progess Models}
\description{
  These are generic functions used as accessors and mutators for
  SuperCurve-specific additions for objects of Progress-related subclasses.

  \describe{
    \item{\code{progressMarquee}:}{determines marquee "applied" to the task.}
    \item{\code{progressStage}:}{determines current stage of the task.}
  }

  The method invokes particular \code{\link{methods}}
  which depend on the \code{\link{class}} of the first argument.
}
\usage{
\S4method{progressMarquee}{ANY}(object)
\S4method{progressMarquee}{ANY}(object, \dots) <- value
\S4method{progressStage}{ANY}(object)
\S4method{progressStage}{ANY}(object, \dots) <- value
}
\arguments{
  \item{object}{object of (sub)class \code{SCProgressMonitor}}
  \item{value}{new value to apply}
  \item{\dots}{additional arguments affecting the updated values}
}
\details{
  All functions are generic: you must write methods to handle specific classes
  of objects.
}
\value{
  The form of the value returned by these methods depends on the
  class of its argument. See the documentation of the particular methods
  for details of what is produced by that method.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\seealso{
  \code{\linkS4class{ProgressMonitor}},
  \code{\linkS4class{DefaultProgressMonitor}}
}
\keyword{methods}

