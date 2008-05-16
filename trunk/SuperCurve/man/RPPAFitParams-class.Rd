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
  \item{measure}{character string identifying the column of the raw RPPA data
    that should be used to fit to the model.}
  \item{xform}{(Experimental) function that takes a single input vector and
    returns a single output vector of the same length. The \code{measure}
    column is transformed using this function before fitting the model.}
  \item{method}{character string specifying the method for matching
    the individual dilution series to the response curve fitted for the slide.
    Valid values are:
    \tabular{ll}{
      \code{"nls"}   \tab uses the optimal fit based on nonlinear least
                          squares \cr
      \code{"nlrob"} \tab uses \code{nlrob} which is robust nls from
                          robustbase package \cr
      \code{"nlrq"}  \tab uses \code{nlrq} from quantreg package which is
                          robust median regression \cr
    }}
  \item{ci}{logical scalar. If \code{TRUE}, compute 90\% confidence intervals
    on the concentration estimates.}
  \item{ignoreNegative}{logical scalar. If \code{TRUE}, convert negative values
    to NA before fitting the model.}
  \item{trace}{logical scalar passed to \link{nls} in the \code{method} portion
    of the routine}
  \item{verbose}{logical scalar. If \code{TRUE}, print updates while fitting
    the data}
  \item{veryVerbose}{logical scalar. If \code{TRUE}, print voluminous updates
    as each individual dilution series is fitted}
  \item{warnLevel}{integer scalar used to set the \code{warn} option before
    calling \code{method}. Since this is wrapped in a \code{try} function, it
    won't cause failure but will give us a chance to figure out which dilution
    series are failing. Setting \code{warnLevel} to two or greater may change
    the values returned.}
  \item{trim}{logical scalar specifying if concentrations should be trimmed.
    If \code{TRUE}, concentrations will be trimmed to reflect min and max
    concentrations we can estimate given the background noise; otherwise,
    raw concentrations are returned without trimming.}
  \item{model}{character string specifying the model for the response curve
    fitted for the slide. Valid values are:
    \tabular{ll}{
      \code{"logistic"} \tab assumes a logistic shape for the curve \cr
      \code{"loess"}    \tab fits a loess curve to the response \cr
      \code{"cobs"}     \tab fits a b-spline curve to the slide with the
                             constraint that curve be strictly increasing \cr
    }}
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
  The \code{RPPAFitParams} generator returns an object of class
  \code{RPPAFitParams}.
}
\references{KRC}
\author{Kevin R. Coombes <kcoombes@mdanderson.org>}
\seealso{
  \code{\link{RPPAFit}}
}
\examples{
##path <- system.file("rppaTumorData", package="SuperCurve")
##cat("path=", path, "\n")
##erk2 <- RPPA("ERK2.txt", path=path)
}
\keyword{models}

