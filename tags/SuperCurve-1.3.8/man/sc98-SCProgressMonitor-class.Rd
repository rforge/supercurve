\name{SCProgressMonitor-class}
\alias{SCProgressMonitor-class}
\alias{SCProgressMonitor}
\alias{getStages}
\alias{is.SCProgressMonitor}
\alias{progressMarquee,SCProgressMonitor-method}
\alias{progressMarquee<-,SCProgressMonitor,character-method}
\alias{progressStage,SCProgressMonitor-method}
\alias{progressStage<-,SCProgressMonitor,character-method}
\docType{class}
\title{Class "SCProgressMonitor"}
\description{
  The \code{SCProgressMonitor} class represents an attempt to abstract
reporting of progress of a task. This class assumes that progress is reported
via a progressbar and provides means to get/set values for such a widget.
}
\usage{
SCProgressMonitor(stage="")
is.SCProgressMonitor(x)
getStages()
%%\S4method{progressMarquee}{SCProgressMonitor}(object)
%%\S4method{progressMarquee}{SCProgressMonitor}(object, \dots) <- value
%%\S4method{progressStage}{SCProgressMonitor}(object)
%%\S4method{progressStage}{SCProgressMonitor}(object, \dots) <- value
}
\arguments{
  \item{stage}{string specifying current stage of task}
  \item{x}{object of (sub)class \code{SCProgressMonitor}}
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{SCProgressMonitor} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{stage}:}{string specifying current stage of task}
    \item{\code{marquee}:}{string specifying marquee for current task}
    \item{\code{range}:}{object of class \code{BoundedRange}}
    \item{\code{label}:}{string specifying detail label for current task}
    \item{\code{done}:}{logical scalar specifying if task completed.
      Default is \code{FALSE}.}
    \item{\code{err}:}{logical scalar specifying if an error has occurred.
      Default is \code{FALSE}.}
  }
}
\section{Extends}{
  Class \code{\linkS4class{DefaultProgressMonitor}}, directly.
  Class \code{\linkS4class{ProgressMonitor}},
    by class "DefaultProgressMonitor", distance 2.
}
\section{Methods}{
  \describe{
    \item{progressStage}{\code{signature(object = "SCProgressMonitor")}:
      Returns string representing current stage of task.}
    \item{progressStage<-}{\code{signature(object = "SCProgressMonitor",
      value = "character")}: Sets value of the \code{stage} slot.}
    \item{progressMarquee}{\code{signature(object = "SCProgressMonitor")}:
      Returns string representing marquee for current stage.}
    \item{progressMarquee<-}{\code{signature(object = "SCProgressMonitor",
      value = "character")}: Sets value of the \code{marquee} slot.}
  }
}
\value{
  The \code{SCProgressMonitor} generator returns an object of class
  \code{SCProgressMonitor}.

  The \code{getStages} method returns a named character vector specifying
  the stages processed by this package.

  The \code{is.SCProgressMonitor} method returns \code{TRUE} if its
  argument is an object of class \code{SCProgressMonitor}.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\seealso{
  \code{\linkS4class{BoundedRange}},
  \code{\linkS4class{ElapsedTime}},
  \code{\linkS4class{ProgressMonitor}},
  \code{\linkS4class{DefaultProgressMonitor}}
}
\examples{
showClass("SCProgressMonitor")
niters <- 10
scpm <- SuperCurve:::SCProgressMonitor("input")
progressMarquee(scpm) <- "Read input files"
for (i in seq.int(niters)) {
    ## Perform portion of task
    progressValue(scpm) <- i # Modify current value
}
}
\keyword{classes}
\keyword{methods}

