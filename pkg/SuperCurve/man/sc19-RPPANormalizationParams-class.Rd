% $Id$

\name{RPPANormalizationParams-class}
\alias{RPPANormalizationParams-class}
\alias{RPPANormalizationParams}
\alias{is.RPPANormalizationParams}
\alias{paramString,RPPANormalizationParams-method}
\docType{class}
\title{Class "RPPANormalizationParams"}
\description{
  The RPPANormalizationParams class is used to bundle the parameter set together
  that control how to perform spatial adjustment into a reuseable object.
}
\usage{
RPPANormalizationParams(method,
                        arglist=NULL)
is.RPPANormalizationParams(x)
\S4method{paramString}{RPPANormalizationParams}(object, slots, \dots)
}
\arguments{
  \item{method}{character string specifying normalization method to use}
  \item{arglist}{list of named key/value pairs representing argument list to
    be passed upon invocation of \code{normalize} method}
  \item{object}{object of class \code{RPPANormalizationParams}}
  \item{x}{object of class \code{RPPANormalizationParams}}
  \item{slots}{strings specifying \code{RPPANormalizationParams} slotnames
    to display (for debuggging)}
  \item{\dots}{extra arguments for generic routines}
}
\details{
  The \code{method} argument is combined with the \code{arglist} argument
  prior to invocation of \code{normalize} method.
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{RPPANormalizationParams} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{character string; see arguments above}
    \item{\code{method}:}{character string; see arguments above}
    \item{\code{arglist}:}{list of named key/value pairs; see arguments above}
  }
}
\section{Methods}{
  \describe{
    \item{paramString(object)}{Returns string representation of object.}
  }
}
\value{
  The \code{RPPANormalizationParams} generator returns an object of class
  \code{RPPANormalizationParams}.

  The \code{is.RPPANormalizationParams} method returns \code{TRUE} if its
  argument is an object of class \code{RPPANormalizationParams}.

  The \code{paramString} method returns a character vector, possibly
  empty but never \code{NULL}.
}
\section{Warning}{
  The \code{paramString} method should not be called by user except for
  informational purposes. The content and format of the returned string
  may vary between different versions of this package.
}
\references{KRC}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\link{normalize}}
}
\examples{
showClass("RPPANormalizationParams")
normparams <- RPPANormalizationParams(method="medpolish",
                                      arglist=list(calc.medians=FALSE))
paramString(normparams)
}
\keyword{classes}

