\name{RPPADesignParams-class}
\docType{class}
\alias{RPPADesignParams-class}
\alias{RPPADesignParams}

\title{The RPPADesignParams Class}
\description{
  The RPPADesignParams class represents TBD
}
\usage{
RPPADesignParams(steps=rep(0,1),
                 series=factor(rep(0,1)),
                 grouping=c("byRow", "byCol", "bySample", "blockSample"),
                 ordering=c("decreasing", "increasing"),
                 alias=list(),
                 center=FALSE,
                 controls=list())
}
\arguments{
  \item{steps}{An optional numeric vector listing the dilution step
    associated with each spot, on a logarithmic scale.}
  \item{series}{factor TBD}
  \item{grouping}{Describes the way dilution series are oriented on the
    array.}
  \item{ordering}{Are dilution series arranged in order of
    \code{increasing} or \code{decreasing} concentrations. Default is
    \code{decreasing}.}
  \item{alias}{list TBD}
  \item{center}{logical TBD}
  \item{controls}{list TBD}
}
\details{
  TBD
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link{new}, the preferred method is to use the \code{RPPADesignParams}
  function.
}
\section{Slots}{
  \describe{
    \item{\code{alias}:}{TBD}
    \item{\code{center}:}{TBD}
    \item{\code{controls}:}{TBD}
    \item{\code{grouping}:}{TBD}
    \item{\code{ordering}:}{TBD}
    \item{\code{series}:}{TBD}
    \item{\code{steps}:}{TBD}
  }
}
\value{
  The \code{RPPADesignParams} constructor returns an object of the
  \code{RPPADesignParams} class.
}
\references{KRC}
\author{Kevin R. Coombes <kcoombes@mdanderson.org>}
\seealso{
  \code{\link{RPPAFit}},
  \code{\link{RPPADesign}}
}
\examples{
##path <- system.file("rppaTumorData", package="SuperCurve")
##cat("path=", path, "\n")
##erk2 <- RPPA("ERK2.txt", path=path)
}
\keyword{models}

