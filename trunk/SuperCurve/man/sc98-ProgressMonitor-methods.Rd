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
  These are generic functions used as accessors and mutators for objects of
  Progress-related classes.

  \describe{
    \item{\code{progressDone}:}{determines whether the task has been
      completed.}
    \item{\code{progressError}:}{determines whether an error occurred
      attempting to complete the task.}
    \item{\code{progressLabel}:}{determines label "applied" to the task.}
    \item{\code{progressMinimum}:}{determines value associated with
      initiating the task.}
    \item{\code{progressMaximum}:}{determines value associated with
      completing the task.}
    \item{\code{progressMaximum}:}{determines value associated with
      percentage of the task completed.}
  }

  The method invokes particular \code{\link{methods}}
  which depend on the \code{\link{class}} of the first argument.
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

