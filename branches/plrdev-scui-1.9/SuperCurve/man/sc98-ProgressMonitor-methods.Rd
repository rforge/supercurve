\name{ProgressMonitor-methods}
\alias{progressDone}
\alias{progressDone-method}
\alias{progressDone,ANY-method}
\alias{progressDone<-}
\alias{progressDone<--method}
\alias{progressDone<-,ANY-method}
\alias{progressError}
\alias{progressError-method}
\alias{progressError,ANY-method}
\alias{progressError<-}
\alias{progressError<--method}
\alias{progressError<-,ANY-method}
\alias{progressLabel}
\alias{progressLabel-method}
\alias{progressLabel,ANY-method}
\alias{progressLabel<-}
\alias{progressLabel<--method}
\alias{progressLabel<-,ANY-method}
\alias{progressMaximum}
\alias{progressMaximum-method}
\alias{progressMaximum,ANY-method}
\alias{progressMaximum<-}
\alias{progressMaximum<--method}
\alias{progressMaximum<-,ANY-method}
\alias{progressMinimum}
\alias{progressMinimum-method}
\alias{progressMinimum,ANY-method}
\alias{progressMinimum<-}
\alias{progressMinimum<--method}
\alias{progressMinimum<-,ANY-method}
\alias{progressValue}
\alias{progressValue-method}
\alias{progressValue,ANY-method}
\alias{progressValue<-}
\alias{progressValue<--method}
\alias{progressValue<-,ANY-method}
\docType{methods}
\title{Methods for Manipulating Progess Models}
\description{
  These methods provide accessors and mutators for Progress-related classes.
}
\usage{
\S4method{progressDone}{ANY}(object)
\S4method{progressDone}{ANY}(object, \dots) <- value
\S4method{progressError}{ANY}(object)
\S4method{progressError}{ANY}(object, \dots) <- value
\S4method{progressLabel}{ANY}(object)
\S4method{progressLabel}{ANY}(object, \dots) <- value
\S4method{progressMaximum}{ANY}(object)
\S4method{progressMaximum}{ANY}(object, \dots) <- value
\S4method{progressMinimum}{ANY}(object)
\S4method{progressMinimum}{ANY}(object, \dots) <- value
\S4method{progressValue}{ANY}(object)
\S4method{progressValue}{ANY}(object, \dots) <- value
}
\arguments{
  \item{object}{object of (sub)class \code{ProgressMonitor}}
  \item{value}{new value to apply}
  \item{\dots}{extra arguments for generic routines}
}
\details{
  All functions are generic: you must write methods to handle specific classes  of objects.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\seealso{
  \code{\linkS4class{ProgressMonitor}},
  \code{\linkS4class{DefaultProgressMonitor}}
}
\keyword{methods}

