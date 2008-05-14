\name{RPPAFit}
\alias{RPPAFit}
\title{Fit Dilution Curves to Protein Lysate Series}
\description{
  \code{RPPAFit} fits an intensity response model to the dilution
  series in a reverse-phase protein array experiment. Individual sample
  concentrations are estimated by matching individual sample dilution
  series to the overall logistic response for the slide.  
}
\usage{
RPPAFit(rppa, design, measure, xform=function(x) x,
    method = c("nls", "nlrob", "nlrq"), trim = TRUE,
    ci = FALSE, ignoreNegative = TRUE, 
    trace = FALSE, verbose = FALSE, veryVerbose = FALSE,
    warnLevel = 0, model=c("logistic", "loess", "cobs"))
}
\arguments{
  \item{rppa}{object of type \code{\link{RPPA}} containing the raw data
    to be fit}
  \item{design}{object of type \code{\link{RPPADesign}} describing the layout
    of the array}
  \item{measure}{character string identifying the column of the raw RPPA data
    that should be used to fit to the model.}
  \item{xform}{(Experimental) function that takes a single input vector and
    returns a single output vector of the same length. The \code{measure}
    column is transformed using this function before fitting the model.}
  \item{method}{character string specifying the method for matching
    the individual dilution series to the response curve fitted for the slide.
    Default method is \code{nls}, which simply uses the optimal fit based on
    nonlinear least squares. Method \code{nlrob} uses \code{nlrob} which is
    robust nls from robustbase package. Method \code{nlrq} uses \code{nlrq}
    from quantreg package which is robust median regression.}
  \item{trim}{logical specifying if concentrations should be trimmed.
    If TRUE, concentrations will be trimmed to reflect min and max 
    concentrations we can estimate given the background noise.
    If FALSE, raw concentrations are returned with no trimming.}
  \item{ci}{logical specifying whether to compute 90\% confidence intervals
    on the concentration estimates.}
  \item{ignoreNegative}{logical specifying whether negative values should be
    converted to NA before fitting the model.}
  \item{trace}{logical passed to \link{nls} in the \code{method} portion of
    the routine}
  \item{verbose}{logical specifying whether to print updates while fitting
    the data}
  \item{veryVerbose}{logical specifying whether to print voluminous updates
    as each individual dilution series is fitted}
  \item{warnLevel}{numeric used to set the \code{warn} option before calling
    \code{method}. Since this is wrapped in a \code{try} function, it won't
    cause failure but will give us a chance to figure out which dilution series
    are failing. Setting \code{warnLevel} to two or greater may change the
    values returned}
  \item{model}{character string specifying the model for the response curve
    fitted for the slie. Default model is \code{logistic}, which assumes a
    logistic shape for the curve. Model \code{loess} fits a loess curve to the
    response. Model \code{cobs} fits a b-spline curve to the slide with the
    constraint that the curve be strictly increasing.}
}
\details{
  The basic mathematical model is given by \deqn{Y   = f(X-\delta_i),}
  where \eqn{Y} is the observed intensity, \eqn{X} is the designed dilution
  step and f is the model for the protein response curve.  
  By fitting a joint model, we assume that the response curve is the same for
  all dilution series on the array. The real point of the model, however, is
  to be able to draw inferences on the \eqn{\delta_i}, which represent the
  (log) concentration of the protein present in different dilution series.
  
  As the first step in fitting the model, we compute crude estimates of the
  individual \eqn{\delta_i} assuming a rough logistic shape for the protein
  response curve.
  
  Next, we fit an overall response curve for the slide \eqn{f} using the
  estimated concentrations and observed intensities \eqn{Y = f(\delta_i)}.
  The model for  \eqn{f} is specified in the \eqn{model} parameter.
  
  Next, we update the estimates of the individual \eqn{\delta_i} using our
  improved fitted model \eqn{f} for the overall slide response curve. These
  individual series are matched to the overall slide response curve using the
  algorithm specified in \code{method}. The default method is \code{nls}, a
  least squares matchup, but we also offer robust alternatives which can do
  better.
  
  Finally we re-estimate \eqn{f} using the improved estimates for \eqn{\delta_i}.
  We continue to iterate between \eqn{f} and \eqn{\delta_i}. We do this 2 times
  since that seems to give reasonable convergence.

  If the \code{ci} argument is set to TRUE, then the function also computes
  confidence intervals around the estimates of the log concentration.
  Since this step can be time-consuming, it is not performed by default.
  Moreover, confidence intervals can be computed after the main model is fit
  and evaluated, using the \code{\link{getConfidenceInterval}} function.
}
\value{
  This function constructs and returns an object of the
  \code{\link{RPPAFit}} class.
}
\references{KRC}
\author{Kevin R. Coombes <kcoombes@mdanderson.org>}
\seealso{
  \code{\link{RPPAFit-class}},
  \code{\link{RPPA}},
  \code{\link{RPPADesign}}
}
\examples{
path <- system.file("rppaTumorData", package="SuperCurve")
erk2 <- RPPA("ERK2.txt", path=path)
design <- RPPADesign(erk2,
                     grouping="blockSample",
                     controls=list("neg con", "pos con"))
fit.nls <- RPPAFit(erk2, design, "Mean.Net")
summary(fit.nls)
##coef(fit.nls)   ## :TBD: R CMD check error
}
\keyword{models}
\keyword{regression}
\keyword{nonlinear}
\keyword{robust}

