% $Id$

\name{getConfidenceInterval}
\alias{getConfidenceInterval}
\title{Compute Confidence Intervals for a Model Fit to Dilution Series}
\description{
  This function computes confidence intervals for the estimated
  concentrations in a four-parameter logistic model fit to a set of
  dilution series in a reverse-phase protein array experiment.
}
\usage{
getConfidenceInterval(result,
                      alpha=0.1,
                      nSim=50,
                      progmethod=NULL)
}
\arguments{
  \item{result}{object of class \code{\link{RPPAFit}} representing the
    result of fitting a four-parameter logistic model}
  \item{alpha}{numeric scalar specifying desired significance of the
    confidence interval; the width of the resulting interval is 1 - alpha.}
  \item{nSim}{numeric scalar specifying number of times to resample the
    data in order to estimate the confidence intervals.}
  \item{progmethod}{optional function that can be used to report progress.}
}
\details{
  In order to compute the confidence intervals, the function assumes
  that the errors in the observed \eqn{Y} intensities are independent
  normal values, with mean centered on the estimated curve and
  standard deviation that varies smoothly as a function of the (log)
  concentration. The smooth function is estimated using
  \code{\link[stats]{loess}}.
  The residuals are resampled from this estimate and the model is refit;
  the confidence intervals are computed empirically as symmetrically
  defined quantiles of the refit parameter sets.
}
\value{
  An object of class \code{\link{RPPAFit}}, containing updated values for the
  slots \code{lower}, \code{upper}, and \code{conf.width} that describe the
  confidence interval.
}
\author{
  Kevin R. Coombes \email{kcoombes@mdanderson.org},
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\link{RPPAFit-class}},
  \code{\link{RPPAFit}}
}
\examples{
  \dontrun{
extdata.dir <- system.file("extdata", package="SuperCurveSampleData")

txtdir <- file.path(extdata.dir, "rppaCellData")
akt <- RPPA("Akt.txt", path=txtdir)
design <- RPPADesign(akt,
                     grouping="blockSample",
                     controls=list("neg con", "pos con"))
fit.nls <- RPPAFit(akt, design, "Mean.Net")
## N.B.: this takes a while!
fit.nls <- getConfidenceInterval(fit.nls, alpha=0.10, nSim=50)
  }
}
\keyword{models}

