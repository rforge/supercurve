\name{ElapsedTime-class}
\alias{ElapsedTime-class}
\alias{ElapsedTime}
\alias{is.ElapsedTime}
\alias{elapsed,ElapsedTime-method}
\docType{class}
\title{Class "ElapsedTime"}
\description{
  The \code{ElapsedTime} class represents an attempt to abstract
reporting of progress of a task. This class assumes that progress is reported
via a progressbar and provides means to get/set values for such a widget.
}
\usage{
ElapsedTime()
is.ElapsedTime(x)
%%\S4method{elapsed}{ElapsedTime}(object, units="auto")
}
\arguments{
%%\item{object}{object of class \code{ElapsedTime}}
  \item{x}{object of class \code{ElapsedTime}}
%%\item{units}{string specifying desired unit of time}
%%\item{\dots}{extra arguments for generic routines.}
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{ElapsedTime} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{start}:}{numeric scalar specifying the elapsed time
      for the currently running R process (taken from \code{proc.time}).}
  }
}
\section{Methods}{
  \describe{
    \item{elapsed}{\code{signature(object = "ElapsedTime")}:
      Returns object of class \code{difftime} representing elapsed time
      difference between current time and the \code{start} slot value.

      Units may be specified if desired; otherwise, the largest possible
      in which difference is greater than one will be chosen.}
  }
}
\value{
  The \code{ElapsedTime} generator returns an object of class
  \code{ElapsedTime}.

  The \code{is.ElapsedTime} method returns \code{TRUE} if its
  argument is an object of class \code{ElapsedTime}.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\examples{
showClass("ElapsedTime")
et <- ElapsedTime()
elapsed(et)
elapsed(et, units="secs")     # Same units as above
elapsed(et, units="mins")
elapsed(et, units="hours")
elapsed(et, units="days")
}
\keyword{classes}
\keyword{methods}

