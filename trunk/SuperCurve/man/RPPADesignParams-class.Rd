\name{RPPADesignParams-class}
\alias{RPPADesignParams-class}
\alias{RPPADesignParams}
\docType{class}
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
  \item{steps}{numeric vector listing the dilution step associated with each
    spot, on a logarithmic scale.}
  \item{series}{character vector or factor identifying the dilution series
    to which each spot corresponds.}
  \item{grouping}{character string specifying the orientation of the
    dilution series on the array. Valid values are:
    \tabular{ll}{
      \code{"byRow"}       \tab each row of a subgrid is its own dilution
                                series \cr
      \code{"byColumn"}    \tab each column of a subgrid is its own dilution
                                series \cr
      \code{"bySample"}    \tab each unique sample id is its own dilution
                                series \cr
      \code{"blockSample"} \tab all occurrences of sample id within subgrid
                                refer to same dilution series \cr
    }}
  \item{ordering}{character string specifying arrangement of dilution series.
    Valid values are:
    \tabular{ll}{
      \code{"decreasing"} \tab arranged in order of decreasing
                               concentrations \cr
      \code{"increasing"} \tab arranged in order of increasing
                               concentrations \cr
    }}
  \item{alias}{list or data frame with the following required
    named components:
    \tabular{ll}{ 
      \code{Alias}  \tab TBD description \cr 
      \code{Sample} \tab TBD description \cr
    }}
  \item{center}{logical scalar. If \code{TRUE}, then dilution steps are
    centered around 0.}
  \item{controls}{list containing the character strings that identify
    control spots on the array.}
}
\details{
  TBD
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link{new}, the preferred method is to use the \code{RPPADesignParams}
  generator function.
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
  The \code{RPPADesignParams} generator returns an object of class
  \code{RPPADesignParams}.
}
\references{KRC}
\author{P. Roebuck, \email{plroebuck@mdanderson.org}}
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

