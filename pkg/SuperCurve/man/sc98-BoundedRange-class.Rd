% $Id$

\name{BoundedRange-class}
\alias{BoundedRange-class}
\alias{BoundedRange}
\alias{is.BoundedRange}
\alias{progressMaximum,BoundedRange-method}
\alias{progressMaximum<-,BoundedRange,numeric-method}
\alias{progressMinimum,BoundedRange-method}
\alias{progressMinimum<-,BoundedRange,numeric-method}
\alias{progressValue,BoundedRange-method}
\alias{progressValue<-,BoundedRange,numeric-method}
\docType{class}
\title{Class "BoundedRange"}
\description{
  The \code{BoundedRange} class represents an attempt to abstract
reporting of progress of a task. This class assumes that progress is reported
via a progressbar and provides means to get/set values for such a widget.
}
\usage{
BoundedRange(value, minimum=1, maximum=100)
is.BoundedRange(x)
\S4method{progressMaximum}{BoundedRange}(object)
\S4method{progressMaximum}{BoundedRange,numeric}(object) <- value
\S4method{progressMinimum}{BoundedRange}(object)
\S4method{progressMinimum}{BoundedRange,numeric}(object) <- value
\S4method{progressValue}{BoundedRange}(object)
\S4method{progressValue}{BoundedRange,numeric}(object) <- value
}
\arguments{
  \item{minimum}{integer specifying desired minimum value of closed interval}
  \item{maximum}{integer specifying desired maximum value of closed interval}
  \item{value}{integer specifying desired current value}
  \item{object}{object of class \code{BoundedRange}}
  \item{x}{object of class \code{BoundedRange}}
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{BoundedRange} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{minimum}:}{integer specifying the minimum value of the closed
      interval. Default is \code{1}.}
    \item{\code{maximum}:}{integer specifying the minimum value of the closed
      interval. Default is \code{100}.}
    \item{\code{value}:}{integer specifying current value. Must be in the
      closed interval \code{minimum}..\code{maximum}}
  }
}
\section{Methods}{
  \describe{
    \item{progressMaximum}{
      \code{signature(object = "BoundedRange")}:\cr
      Returns integer representing maximum value of interval.}
    \item{progressMaximum<-}{
      \code{signature(object = "BoundedRange", value = "numeric")}:\cr
      Sets the value of the \code{maximum} slot.}
    \item{progressMinimum}{
      \code{signature(object = "BoundedRange")}:\cr
      Returns integer representing minimum value of interval.}
    \item{progressMinimum<-}{
      \code{signature(object = "BoundedRange", value = "numeric")}:\cr
      Sets the value of the \code{minimum} slot.}
    \item{progressValue}{
      \code{signature(object = "BoundedRange")}:\cr
      Returns integer representing current value.}
    \item{progressValue<-}{
      \code{signature(object = "BoundedRange", value = "numeric")}:\cr
      Sets the value of the \code{value} slot.}
  }
}
\value{
  The \code{BoundedRange} generator returns an object of class
  \code{BoundedRange}.

  The \code{is.BoundedRange} method returns \code{TRUE} if its
  argument is an object of class \code{BoundedRange}.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\examples{
showClass("BoundedRange")
br <- BoundedRange(3, maximum=10)
progressValue(br) <- 5 # Modify current value
}
\keyword{classes}
\keyword{methods}

