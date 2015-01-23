% $Id$

\name{DefaultProgressMonitor-class}
\alias{DefaultProgressMonitor-class}
\alias{DefaultProgressMonitor}
\alias{is.DefaultProgressMonitor}
\alias{elapsed,DefaultProgressMonitor-method}
\alias{progressDone,DefaultProgressMonitor-method}
\alias{progressDone<-,DefaultProgressMonitor,logical-method}
\alias{progressError,DefaultProgressMonitor-method}
\alias{progressError<-,DefaultProgressMonitor,logical-method}
\alias{progressLabel,DefaultProgressMonitor-method}
\alias{progressLabel<-,DefaultProgressMonitor,character-method}
\alias{progressMaximum,DefaultProgressMonitor-method}
\alias{progressMaximum<-,DefaultProgressMonitor,numeric-method}
\alias{progressMinimum,DefaultProgressMonitor-method}
\alias{progressMinimum<-,DefaultProgressMonitor,numeric-method}
\alias{progressValue,DefaultProgressMonitor-method}
\alias{progressValue<-,DefaultProgressMonitor,numeric-method}
\docType{class}
\title{Class "DefaultProgressMonitor"}
\description{
  The \code{DefaultProgressMonitor} class represents an attempt to abstract
reporting of progress of a task. This class assumes that progress is reported
via a progressbar and provides means to get/set values for such a widget.
}
\usage{
DefaultProgressMonitor(label, value, minimum=0, maximum=100)
\S4method{elapsed}{DefaultProgressMonitor}(object)
\S4method{progressDone}{DefaultProgressMonitor}(object)
\S4method{progressDone}{DefaultProgressMonitor,logical}(object) <- value
\S4method{progressError}{DefaultProgressMonitor}(object)
\S4method{progressError}{DefaultProgressMonitor,logical}(object) <- value
\S4method{progressLabel}{DefaultProgressMonitor}(object)
\S4method{progressLabel}{DefaultProgressMonitor,character}(object) <- value
\S4method{progressMaximum}{DefaultProgressMonitor}(object)
\S4method{progressMaximum}{DefaultProgressMonitor,numeric}(object) <- value
\S4method{progressMinimum}{DefaultProgressMonitor}(object)
\S4method{progressMinimum}{DefaultProgressMonitor,numeric}(object) <- value
\S4method{progressValue}{DefaultProgressMonitor}(object)
\S4method{progressValue}{DefaultProgressMonitor,numeric}(object) <- value
}
\arguments{
  \item{label}{string specifying label for progressbar widget}
  \item{value}{integer value representing current progress towards task
    completion}
  \item{minimum}{integer value representing minimum range of progress}
  \item{maximum}{integer value representing minimum range of progress}
  \item{object}{object of (sub)class \code{DefaultProgressMonitor}}
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{DefaultProgressMonitor} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{done}:}{logical scalar specifying if task completed.
      Default is \code{FALSE}.}
    \item{\code{err}:}{logical scalar specifying if an error has occurred.
      Default is \code{FALSE}.}
    \item{\code{label}:}{string specifying label for abstract progressbar}
    \item{\code{range}:}{object of class \code{BoundedRange}}
    \item{\code{etime}:}{object of class \code{ElapsedTime}}
    \item{\code{elapsed}:}{object of class \code{difftime} specifying seconds
      since last update}
  }
}
\section{Extends}{
  Class \code{\linkS4class{ProgressMonitor}}, directly.
}
\section{Methods}{
  \describe{
    \item{elapsed}{
      \code{signature(object = "DefaultProgressMonitor")}:\cr
      Returns elapsed time since creation of abstract progressbar.}
    \item{progressDone}{
      \code{signature(object = "DefaultProgressMonitor")}:\cr
      Returns \code{TRUE} if task is complete; otherwise, \code{FALSE}.}
    \item{progressDone<-}{
      \code{signature(object = "DefaultProgressMonitor", value = "logical")}:\cr
      Sets value of the \code{done} slot.}
    \item{progressError}{
      \code{signature(object = "DefaultProgressMonitor")}:\cr
      Returns \code{TRUE} if an error occurred during processing; otherwise,
      \code{FALSE}.}
    \item{progressError<-}{
      \code{signature(object = "DefaultProgressMonitor", value = "logical")}:\cr
      Sets value of the \code{err} slot.}
    \item{progressLabel}{
      \code{signature(object = "DefaultProgressMonitor")}:\cr
      Returns string representing label for abstract progressbar.}
    \item{progressLabel<-}{
      \code{signature(object = "DefaultProgressMonitor", value = "character")}:\cr
      Sets value of the \code{label} slot.}
    \item{progressMaximum}{
      \code{signature(object = "DefaultProgressMonitor")}:\cr
      Returns integer representing maximum value for abstract progressbar.}
    \item{progressMaximum<-}{
      \code{signature(object = "DefaultProgressMonitor", value = "numeric")}:\cr
      Sets the maximum value of the \code{range} slot.}
    \item{progressMinimum}{
      \code{signature(object = "DefaultProgressMonitor")}:\cr
      Returns integer representing minimum value for abstract progressbar.}
    \item{progressMinimum<-}{
      \code{signature(object = "DefaultProgressMonitor", value = "numeric")}:\cr
      Sets the minimum value of the \code{range} slot.}
    \item{progressValue}{
      \code{signature(object = "DefaultProgressMonitor")}:\cr
      Returns integer representing current value for abstract progressbar.}
    \item{progressValue<-}{
      \code{signature(object = "DefaultProgressMonitor", value = "numeric")}:\cr
      Sets the current value of the \code{range} slot.}
  }
}
\value{
  The \code{DefaultProgressMonitor} generator returns an object of class
  \code{DefaultProgressMonitor}.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\link{difftime}},
  \code{\linkS4class{BoundedRange}},
  \code{\linkS4class{ElapsedTime}},
  \code{\linkS4class{ProgressMonitor}}
}
\examples{
showClass("DefaultProgressMonitor")
niters <- 10
dpm <- DefaultProgressMonitor("my task", value=0, maximum=niters)
for (i in seq.int(niters)) {
    ## Perform portion of task
    progressValue(dpm) <- i # Modify current value
}
}
\keyword{classes}
\keyword{methods}

