\name{ProgressMonitor-class}
\alias{ProgressMonitor-class}
\alias{is.ProgressMonitor}
\alias{progressDone,ProgressMonitor-method}
\alias{progressDone<-,ProgressMonitor,ANY-method}
\alias{progressError,ProgressMonitor-method}
\alias{progressError<-,ProgressMonitor,ANY-method}
\alias{progressLabel,ProgressMonitor-method}
\alias{progressLabel<-,ProgressMonitor,ANY-method}
\alias{progressMaximum,ProgressMonitor-method}
\alias{progressMaximum<-,ProgressMonitor,ANY-method}
\alias{progressMinimum,ProgressMonitor-method}
\alias{progressMinimum<-,ProgressMonitor,ANY-method}
\alias{progressValue,ProgressMonitor-method}
\alias{progressValue<-,ProgressMonitor,ANY-method}
\docType{class}
\title{Class "ProgressMonitor"}
\description{
  The \code{ProgressMonitor} class is a virtual class for abstract reporting
of progress of a task. All the generic methods here are placeholders that must
be extended to be used.
}
\usage{
is.ProgressMonitor(x)
%%\S4method{progressDone}{ProgressMonitor}(object, \dots)
%%\S4method{progressDone}{ProgressMonitor}(object, \dots) <- value
%%\S4method{progressError}{ProgressMonitor}(object, \dots)
%%\S4method{progressError}{ProgressMonitor}(object, \dots) <- value
%%\S4method{progressLabel}{ProgressMonitor}(object, \dots)
%%\S4method{progressLabel}{ProgressMonitor}(object, \dots) <- value
%%\S4method{progressMaximum}{ProgressMonitor}(object, \dots)
%%\S4method{progressMaximum}{ProgressMonitor}(object, \dots) <- value
%%\S4method{progressMinimum}{ProgressMonitor}(object, \dots)
%%\S4method{progressMinimum}{ProgressMonitor}(object, \dots) <- value
%%\S4method{progressValue}{ProgressMonitor}(object, \dots)
%%\S4method{progressValue}{ProgressMonitor}(object, \dots) <- value
}
\arguments{
%%\item{object}{object of (sub)class \code{ProgressMonitor}}
  \item{x}{object of (sub)class \code{ProgressMonitor}}
  \item{\dots}{extra arguments for generic routines.}
}
\section{Methods}{
  \describe{
    \item{progressDone(object)}{Placeholder method.}
    \item{progressError(object)}{Placeholder method.}
    \item{progressLabel(object)}{Placeholder method.}
    \item{progressMaximum(object)}{Placeholder method.}
    \item{progressMinimum(object)}{Placeholder method.}
    \item{progressValue(object)}{Placeholder method.}
  }
}
\value{
  The \code{is.ProgressMonitor} method returns \code{TRUE} if its
  argument is an object of class \code{ProgressMonitor}.
}
\section{Warning}{
  This class should not be instantiated directly; extend this class instead.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\keyword{classes}
\keyword{methods}

