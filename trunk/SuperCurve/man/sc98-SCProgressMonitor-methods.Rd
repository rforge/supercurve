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
  These methods provide accessors and mutators for SuperCurve-specific
  additions of Progress-related subclasses.
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
  \item{\dots}{extra arguments for generic routines}
}
\details{
  All functions are generic: you must write methods to handle specific classes
  of objects.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\seealso{
  \code{\linkS4class{ProgressMonitor}},
  \code{\linkS4class{DefaultProgressMonitor}}
}
\keyword{methods}

