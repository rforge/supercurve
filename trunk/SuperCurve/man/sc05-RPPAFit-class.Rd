\name{RPPAFit-class}
\alias{RPPAFit-class}
\alias{summary,RPPAFit-method}
\alias{coef,RPPAFit-method}
\alias{coefficients,RPPAFit-method}
\alias{fitted,RPPAFit-method}
\alias{residuals,RPPAFit-method}
\alias{resid,RPPAFit-method}
\alias{image,RPPAFit-method}
\alias{hist,RPPAFit-method}
\alias{plot,RPPAFit,missing-method}
\docType{class}
\title{Class "RPPAFit"}
\description{
  Objects of the \code{RPPAFit} class represent the results of fitting a
  statistical model of response to the dilution series in a
  reverse-phase protein array experiment.
}
\usage{
\S4method{summary}{RPPAFit}(object, \dots)
\S4method{coef}{RPPAFit}(object, \dots)
\S4method{coefficients}{RPPAFit}(object, \dots)
\S4method{fitted}{RPPAFit}(object, type=c("Y", "y", "X", "x"), \dots)
\S4method{residuals}{RPPAFit}(object, type=c("raw", "standardized", "linear"), \dots)
\S4method{resid}{RPPAFit}(object, \dots)
\S4method{image}{RPPAFit}(x, type=c("Residuals", "StdRes", "X", "Y"), \dots)
\S4method{hist}{RPPAFit}(x, type=c("Residuals", "StdRes"),
  xlab=NULL, main=NULL, \dots)
\S4method{plot}{RPPAFit,missing}(x, y, type=c("cloud", "series", "individual"),
xlab="Log Concentration", ylab="Intensity", colors=NULL, \dots)
}
\arguments{
  \item{object}{object of class \code{RPPAFit}}
  \item{x}{object of class \code{RPPAFit}}
  \item{type}{character string describing the type of fitted values,
    residuals, images, histograms, or plots}
  \item{xlab}{graphics parameter specifying how the x-axis should be labeled}
  \item{ylab}{graphics parameter specifying how the y-axis should be labeled}
  \item{main}{character string specifying title for the plot}
  \item{y}{not used}
  \item{colors}{graphics parameter, used only if \code{type='series'},
    to color the lines connecting different dilution series.
    Eight default colors are used if the argument is NULL.}
  \item{\dots}{extra arguments for generic or plotting routines}
}
\details{
  The \code{RPPAFit} class holds the results of fitting a response model to
  all the dilution series on a reverse-phase protein array. For details on
  how the model is fit, see the \code{\link{RPPAFit}} function. By fitting
  a joint model, we assume that the response curve is the same for all
  dilution series on the array. The real point of the model, however, is
  to be able to draw inferences on the \eqn{\delta_i}, which represent the
  (log) concentration of the protein present in different dilution series.
}
\section{Objects from the Class}{
  Objects should be constructed using the \code{\link{RPPAFit}}
  function.
}
\section{Slots}{
  \describe{
    \item{\code{call}:}{object of class \code{call} specifying the function
      call that was used to generate this model fit}
    \item{\code{rppa}:}{object of class \code{RPPA} containing the raw data
      that was fit}
    \item{\code{design}:}{object of class \code{RPPADesign} describing the
      layout of the array}
    \item{\code{measure}:}{character string containing the name of the
      measurement column in the raw data that was fit by the model}
    \item{\code{method}:}{character string containing the name of the
      method that was used to estimate the upper and lower limit
      parameters in the model}
    \item{\code{trimset}:}{TBD}
    \item{\code{model}:}{object of class \code{FitClass} TBD}
    \item{\code{concentrations}:}{numeric vector of estimates of the relative
      log concentration of protein present in each sample}
    \item{\code{lower}:}{numeric vector containing the lower bounds on the
      confidence interval of the log concentration estimates}
    \item{\code{upper}:}{numeric vector containing the upper bounds on the
      confidence interval of the log concentration estimates}
    \item{\code{conf.width}:}{numeric scalar specifying width of the
      confidence interval}
    \item{\code{intensities}:}{numeric vector containing the predicted observed
      intensity at the estimated concentrations for each dilution series}
    \item{\code{ss.ratio}:}{numeric vector containing statistic measuring the
      \eqn{R^2} for each individual dilution series}
    \item{\code{warn}:}{character vector containing any warnings that arose
      when trying to fit the model to individual dilution series}
    \item{\code{version}:}{character string containing the version of
      SuperCurve that produced the fit}
  }
}
\section{Methods}{
  \describe{
    \item{summary(object, \dots)}{Print a summary of the \code{RPPAFit}
      object. At present, this only reports the function call used to
      fit the model.}
    \item{fitted(object, type=c("Y", "y", "X", "x"), \dots)}{Extract the
      fitted values of the model. This process is more complicated
      than it may seem at first, since we are estimating values on
      both the \eqn{X} and \eqn{Y} axes. By default, the fitted
      values are assumed to be the intensities, \eqn{Y}, which are
      obtained using either an uppercase or lowercase 'y' as the
      \code{type} argument. The fitted log concentrations are
      returned when \code{type} is set to either uppercase or
      lowercase 'x'. In the notation used above to describe the model,
      these fitted values are given by \eqn{X_i = X - \delta_i}.}
    \item{residuals(object, type=c("raw", "standardized"), \dots)}{
      Report the residual errors. The 'raw' residuals are defined
      as the difference between the observed intensities and the
      fitted intensities, as computed by the \code{fitted} function.
      The 'standardized' residuals are obtained by standardizing the
      raw residuals.}
    \item{resid(object, \dots)}{An alias for \code{residuals}.}
    \item{image(x, type=c("Residuals", "StdRes", "X", "Y"), \dots)}{
      The \code{image} method produces a 'geographic' plot of the
      residuals or of the fitted values, depending on the value of the
      \code{type} argument. The implementation reuses code from the
      \code{image} method for an \code{\link{RPPA}} object.}
    \item{hist(x, type=c("Residuals", "StdRes"), xlab=NULL, main=NULL,
      \dots)}{The \code{hist} method produces a histogram of
      the residuals. The exact form of the residuals being displayed
      depends on the value of the \code{type} argument.}
    \item{plot(x, y, type=c("cloud", "series", "individual"),
      xlab="Log Concentration", ylab="Intensity", \dots)}{
      The \code{plot} method produces a diagnostic plot of the model fit.
      The default \code{type}, 'cloud', simply plots the fitted \eqn{X} values
      against the observed \eqn{Y} values as a cloud of points around
      the jointly estimated sigmoid curve. The 'series' plot uses
      different colored lines to join points belonging to the same
      dilution series. The 'individual' plot produces separate graphs
      for each dilution series, laying each one alongside the jointly
      fitted sigmoid curve.}
  }
}
\value{
  The \code{summary} method has no return value.

  The \code{coef} and \code{coefficients} methods return a named vector
  of length three.

  The \code{fitted} method returns a numeric vector.

  The \code{resid} and \code{residuals} methods return a numeric vector.

  The \code{image} method invisibly returns the object \code{x} on which
  it was invoked.

  The \code{hist} method returns an object of class \code{histogram}.

  The \code{plot} method invisibly returns the object \code{x} on which
  it was invoked.
}
\references{KRC}
\author{Kevin R. Coombes <kcoombes@mdanderson.org>}
\seealso{
  \code{\linkS4class{RPPA}},
  \code{\linkS4class{RPPADesign}},
  \code{\link{RPPAFit}},
  \code{\link{hist}}
}
\examples{
path <- system.file("rppaTumorData", package="SuperCurve")
erk2 <- RPPA("ERK2.txt", path=path)
design <- RPPADesign(erk2,
                     grouping="blockSample",
                     controls=list("neg con", "pos con"))
fit.nls <- RPPAFit(erk2, design, "Mean.Net")
showMethods('image')
class(fit.nls)

image(fit.nls)
image(fit.nls, measure="Residuals")
plot(fit.nls, type="cloud")

fit.q <- RPPAFit(erk2, design, "Mean.Net")
hist(fit.q, type="StdRes")
plot(fit.q, type="series")

##coef(fit.nls)   ## :BUG: method 'coef' doesn't work
##coef(fit.q)   ## :BUG: method 'coef' doesn't work

plot(fitted(fit.q), resid(fit.q))
}
\keyword{regression}
\keyword{robust}

