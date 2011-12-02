\name{elapsed-method}
\alias{elapsed}
\alias{elapsed-method}
\alias{elapsed,ANY-method}
\docType{methods}
\title{Method "elapsed"}
\description{
  \code{elapsed} is a generic function used to return the elapsed time since
  some operation (related to the object) began. The method invokes particular
  \code{\link{methods}} which depend on the \code{\link{class}} of the
  first argument.
}
\usage{
elapsed(object, \dots)
}
\arguments{
  \item{object}{an object for which an elapsed time is desired}
  \item{\dots}{additional arguments affecting the elapsed time produced}
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\keyword{methods}

