% $Id$

\name{elapsed-method}
\alias{elapsed}
\alias{elapsed-method}
\alias{elapsed,ANY-method}
\docType{methods}
\title{Method \dQuote{elapsed}}
\description{
  \code{elapsed} is a generic function used to return the elapsed time since
  some operation (related to the object) began. The method invokes particular
  \code{\link{methods}} which depend on the \code{\link{class}} of the
  first argument.
}
\usage{
\S4method{elapsed}{ANY}(object, \dots)
}
\arguments{
  \item{object}{an object for which an elapsed time is desired}
  \item{\dots}{additional arguments affecting the elapsed time produced}
}
\value{
  The form of the value returned by \code{elapsed} depends on the
  class of its argument. See the documentation of the particular methods
  for details of what is produced by that method.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\keyword{methods}

