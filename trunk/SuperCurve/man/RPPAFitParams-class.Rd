\name{RPPAFitParams-class}
\docType{class}
\alias{RPPAFitParams-class}
\alias{RPPAFitParams}

\title{The RPPAFitParams Class}
\description{
  The RPPAFitParams class represents TBD
}
\usage{
RPPAFitParams(measure,
              xform=function(x) x,
              method=c("nls", "nlrob", "nlrq"),
              ci=FALSE,
              ignoreNegative=TRUE,
              trace=FALSE,
              verbose=FALSE,
              veryVerbose=FALSE,
              warnLevel=0,
              trim=TRUE,
              model=c("logistic", "loess", "cobs"))
}
\arguments{
  \item{measure}{character TBD}
  \item{xform}{function TBD}
  \item{method}{character TBD}
  \item{ci}{logical TBD}
  \item{ignoreNegative}{logical TBD}
  \item{trace}{logical TBD}
  \item{verbose}{logical TBD}
  \item{veryVerbose}{logical TBD}
  \item{warnLevel}{numeric TBD}
  \item{trim}{logical TBD}
  \item{model}{character TBD}
}
\details{
  TBD
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link{new}, the preferred method is to use the \code{RPPAFitParams}
  function.
}
\section{Slots}{
  \describe{
    \item{\code{measure}:}{character TBD}
    \item{\code{xform}:}{function TBD}
    \item{\code{method}:}{character TBD}
    \item{\code{ci}:}{logical TBD}
    \item{\code{ignoreNegative}:}{logical TBD}
    \item{\code{trace}:}{logical TBD}
    \item{\code{verbose}:}{logical TBD}
    \item{\code{veryVerbose}:}{logical TBD}
    \item{\code{warnLevel}:}{numeric TBD}
    \item{\code{trim}:}{logical TBD}
    \item{\code{model}:}{character TBD}
  }
}
\value{
  The \code{RPPAFitParams} constructor returns an object of the
  \code{RPPAFitParams} class.
}
\references{KRC}
\author{Kevin R. Coombes <kcoombes@mdanderson.org>}
\seealso{\code{\link{RPPAFit}}}
\examples{
##path <- system.file("rppaTumorData", package="SuperCurve")
##cat("path=", path, "\n")
##erk2 <- RPPA("ERK2.txt", path=path)
}
\keyword{models}

