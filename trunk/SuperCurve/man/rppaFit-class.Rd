\name{RPPAFit-class}
\alias{RPPAFit-class}
\docType{class}
\alias{summary,RPPAFit-method}
\alias{coef,RPPAFit-method}
\alias{coefficients,RPPAFit-method}
\alias{fitted,RPPAFit-method}
\alias{residuals,RPPAFit-method}
\alias{resid,RPPAFit-method}
\alias{image,RPPAFit-method}
\alias{hist,RPPAFit-method}
\alias{plot,RPPAFit-method}

\title{The RPPAFit Class}
\description{
  Objects of the \code{RPPAFit} class represent the results of
  fitting a four-parameter logistic model to the dilution series in a
  reverse-phase protein array experiment.
}
\usage{
\S4method{summary}{RPPAFit}(object, ...)
\S4method{coef}{RPPAFit}(object, ...)
\S4method{coefficients}{RPPAFit}(object, ...)
\S4method{fitted}{RPPAFit}(object, type=c("Y", "y", "X", "x"), ...)
\S4method{residuals}{RPPAFit}(object, type=c("raw", "standardized", "linear"), ...)
\S4method{resid}{RPPAFit}(object, ...)
\S4method{image}{RPPAFit}(x, type=c("Residuals", "StdRes", "X", "Y"), ...)
\S4method{hist}{RPPAFit}(x, type=c("Residuals", "StdRes"),
  xlab = NULL, main = NULL, ...)
\S4method{plot}{RPPAFit}(x, y, type=c("cloud", "series", "individual"),
xlab="Log Concentration", ylab="Intensity", colors=NULL, ...)
}
\arguments{
  \item{object}{A RPPAFit object.}
  \item{x}{A RPPAFit object.}
  \item{type}{A list of options describing the type of fitted values,
    residuals, images, histograms, or plots.}
  \item{xlab}{Graphics parameter; how the x-axis should be labeled.}
  \item{ylab}{Graphics parameter; how the y-axis should be labeled.}
  \item{main}{Character string used as a title for the plot.}
  \item{y}{not used.}
  \item{colors}{A graphical parameter, used only if
    \code{type='series'}, to color the lines connecting different
    dilution series. Eight default colors are used if the argument is NULL.} 
  \item{\dots}{The usual extra arguments for generic or plotting routines.}
}
\details{
  The \code{RPPAFit} class holds the results of fitting a
  response model to all the dilution series on a
  reverse-phase protein array. For details on how the model is fit, see
  the \code{\link{RPPAFit}} function. 
  By fitting a joint model, we assume that the response curve is the same for all dilution series on
  the array.  The real point of the model, however, is to be able to
  draw inferences on the \eqn{\delta_i}, which represent the (log)
  concentration of the protein present in different dilution series.
}
\section{Objects from the Class}{
  Objects should be constructed using the \code{\link{RPPAFit}}
  function.
}
\section{Slots}{
  \describe{
    \item{\code{call}:}{A call object: the function call that was used
      to generate this model fit.}
    \item{\code{rppa}:}{The RPPA object containing the raw data that was
    fit}
    \item{\code{design}:}{The RPPADesign object describing the layout of
    the array.}
    \item{\code{measure}:}{A character string containing the name of the
    measurement column in the raww data that was fit by the model}
    \item{\code{method}:}{A charcater string containing the name of the
      metohd that was used to estimate the upper and lower limit
      parameters in the model.}
    \item{\code{concentration}:}{A vector of estimates of the relative
      log concentration of protein presnt in each sample.}
    \item{\code{lower}:}{A vector containing the lower bounds on the
      confidence interval of the log concentration estimates}
    \item{\code{upper}:}{A vector containing the upper bounds on the
      confidence interval of the log concentration estimates}
    \item{\code{conf.width}:}{The width of the confidence interval}
    \item{\code{intensities}:}{The predicted observed intensity at the
      estimated concentrations for each dilution series.}
    \item{\code{ss.ratio}:}{A statistic measuring the R^2
      for each individual dilution series.}
    \item{\code{warn}:}{A character vector containing any warnings that
      arose when trying to fit the model to individual dilution series.}
    \item{\code{version}:}{A character string containing the version of
      SuperCurve that produced the fit}
  }
}
\section{Methods}{
  \describe{
    \item{summary(object, ...)}{Print a summary of the \code{RPPAFit}
	object. At present, this only reports the function call used to
	fit the model.}
    \item{fitted(object, type=c("Y", "y", "X", "x"), ...)}{Extract the
	fitted values of the model.   This process is more complictaed
	than it may seem at first, since we are estimating values on
	both the \eqn{X} and \eqn{Y} axes.  By default, the fitted
	values are assumed to be the intensities, \eqn{Y}, which are
	obtained using either an uppercase or lowercase 'y' as the
	\code{type} argument.  The fitted log concentrations are
	returned when \code{type} is set to either uppercase or
	lowercase 'x'. In the notation used above to describe the model,
	these fitted values are given by \eqn{X_i = X - \delta_i}.}
    \item{residuals(object, type=c("raw", "standardized"),
      ...)}{Report the residual errors. The 'raw' residuals are defined
	as the difference between the observed intensities and the
	fitted intensities, as computed by the \code{fitted} function.
	The 'standardized' residuals are obtained by standardizing the
	raw residuals. }
    \item{resid(object, ...)}{An alias for \code{residuals}.}
    \item{image(x, type=c("Residuals", "StdRes","X", "Y"),
      ...)}{The \code{image} method produces a 'geographic' plot of the
	residuals or of the fitted values, depending on the value of the
      \code{type} argument. The implementation reuses code from the
      \code{image} method for an \code{\link{RPPA}} object.}
    \item{hist(x, type=c("Residuals", "StdRes"), xlab = NULL,
      main = NULL, ...)}{The \code{hist} method produces a histogram of
	the residuals. The exact form of the residuals being displayed
	depends on the value of the \code{type} argument.} 
    \item{plot(x, y, type=c("cloud", "series", "individual"), xlab="Log
      Concentration", ylab="Intensity", ...)}{The \code{plot} method
      produces a diagnostic plot of the model fit.  The default
      \code{type}, 'cloud', simply plots the fited \eqn{X} values
      against the observed \eqn{Y} values as a cloud of points around
      the jointly estimated sigmoid curve. The 'series' plot uses
      different colored lines to join points belonging to the same
      dilution series. The 'individual' plot produces separate graphs
      for each dilution series, laying each one  alongside the jointly
      fitted  sigmoid curve.}
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

  The \code{hist} method returns an object of the \code{histogram}
  class.

  The \code{plot} method invisibly returns the object \code{x} on which
  it was invoked.
}
\references{}
\author{Kevin R. Coombes <kcoombes@mdanderson.org>}
\seealso{ \code{\link{RPPAFit}}, \code{\link{RPPA}},
  \code{\link{RPPADesign}}, \code{\link{hist}}}
\examples{
path <- system.file("rppaTumorData", package="SuperCurve")
erk2 <- RPPA("ERK2.txt", path=path)
design <- RPPADesign(erk2, grouping="blockSample",
                     controls=list("neg con", "pos con"))
fit.nls <- RPPAFit(erk2, design, "Mean.Net")
image(fit.nls, measure="Residuals")
plot(fit.nls, type="cloud")

fit.q <- RPPAFit(erk2, design, "Mean.Net")
hist(fit.q, type="StdRes")
plot(fit.q, type="series")

## :FIXME: coef method doesn't work
#coef(fit.nls)
#coef(fit.q)

plot(fitted(fit.q), resid(fit.q))
}
\keyword{regression}
\keyword{robust}
