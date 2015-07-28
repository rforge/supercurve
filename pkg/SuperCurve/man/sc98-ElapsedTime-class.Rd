% $Id$

\name{ElapsedTime-class}
\alias{ElapsedTime-class}
\alias{ElapsedTime}
\alias{is.ElapsedTime}
\alias{elapsed,ElapsedTime-method}
\docType{class}
\title{Class \dQuote{ElapsedTime}}
\description{
  The \code{ElapsedTime} class represents a means of reporting elapsed time.
}
\usage{
ElapsedTime()
is.ElapsedTime(x)
\S4method{elapsed}{ElapsedTime}(object,
        units=c("auto", "secs", "mins", "hours", "days"))
}
\arguments{
  \item{x}{object of class \code{ElapsedTime}}
  \item{object}{object of class \code{ElapsedTime}}
  \item{units}{string specifying desired unit of time}
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{ElapsedTime} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{start}:}{numeric scalar specifying the elapsed time
      for the currently running R process (taken from
      \code{\link{proc.time}} when created)}
  }
}
\section{Methods}{
  \describe{
    \item{elapsed}{
      \code{signature(object = "ElapsedTime")}:\cr
      Returns object of class \code{difftime} representing elapsed time
      difference between current time and the \code{start} slot value.

      Units may be specified if desired; otherwise, the largest possible
      unit in which difference is greater than one will be chosen.}
  }
}
\value{
  The \code{ElapsedTime} generator returns an object of class
  \code{ElapsedTime}.

  The \code{is.ElapsedTime} method returns \code{TRUE} if its
  argument is an object of class \code{ElapsedTime}.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\link{difftime}}
}
\examples{
showClass("ElapsedTime")
et <- ElapsedTime()
elapsed(et)                  # 'auto' reports 'secs' as less than minute later
elapsed(et, units="secs")
elapsed(et, units="mins")
elapsed(et, units="hours")
elapsed(et, units="days")
}
\keyword{classes}
\keyword{methods}

