\name{SuperCurveSettings-class}
\alias{SuperCurveSettings-class}
\alias{SuperCurveSettings}
\alias{paramString,SuperCurveSettings-method}
\alias{fitCurveAndSummarizeFromSettings}
\docType{class}
\title{Class "SuperCurveSettings"}
\description{
  The SuperCurveSettings class represents the arguments needed
  to perform curve fitting.
}
\usage{
SuperCurveSettings(txtdir,
                   imgdir,
                   outdir,
                   designparams,
                   fitparams)

\S4method{paramString}{SuperCurveSettings}(object, ...)
fitCurveAndSummarizeFromSettings(settings)
}
\arguments{
  \item{txtdir}{character string specifying the directory containing
    quantification files in text format}
  \item{imgdir}{character string specifying the directory containing
    \acronym{TIFF} image files associated with each of the aforementioned
    quantification files}
  \item{outdir}{character string specifying the directory where output
    from analysis should be stored. Must be writable.}
  \item{designparams}{object of class \code{RPPADesignParams}}
  \item{fitparams}{object of class \code{RPPAFitParams}}
  \item{object}{object of class \code{SuperCurveSettings}}
  \item{settings}{object of class \code{SuperCurveSettings}}
  \item{\dots}{extra arguments for generic routines.}
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{SuperCurveSettings} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{txtdir}:}{object of class \code{Directory} specifying the
      directory containing quantification files in text format}
    \item{\code{imgdir}:}{object of class \code{Directory} specifying the
      directory containing TIFF image files}
    \item{\code{outdir}:}{object of class \code{Directory} specifying the
      directory where analysis results should be stored}
    \item{\code{designparams}:}{object of class \code{RPPADesignParams}
      specifying the parameters that describe how a particular set of
      \acronym{RPPA} slides was designed}
    \item{\code{fitparams}:}{object of class \code{RPPAFitParams}
      specifying the parameters that control model fit}
    \item{\code{version}:}{character string containing the version of this
      package used to construct the object}
  }
}
\section{Methods}{
  \describe{
    \item{paramString(object)}{Returns string representation of object.}
  }
}
\value{
  The \code{SuperCurveSettings} generator returns an object of class
  \code{SuperCurveSettings}.

  The \code{paramString} method returns a character vector, possibly
  empty but never \code{NULL}.
}
\section{Warning}{
  The \code{paramString} method should not be called by user except for
  informational purposes. The content and format of the returned string
  may vary between different versions of this package.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\seealso{
  \code{\linkS4class{Directory}},
  \code{\linkS4class{RPPAFitParams}},
  \code{\linkS4class{RPPADesignParams}}
}
\examples{
  \dontrun{
designparams <- RPPADesignParams(center=FALSE,
                                 controls=list("neg con", "pos con"),
                                 grouping="blockSample")
fitparams <- RPPAFitParams(ignoreNegative=FALSE,
                           measure="Mean.Total",
                           method="nlrob",
                           model="loess",
                           warnLevel=-1)
fitparams <- RPPAFitParams(grouping="blockSample")
settings <- SuperCurveSettings(txtdir=system.file("rppaTumorData",
                                                  package="SuperCurve"),
                               imgdir=file.path("", "path", "to", "images"),
                               outdir=tempdir(),
                               designparams=designparams,
                               fitparams=fitparams)
fitCurveAndSummarizeFromSettings(settings)
  }
}
\keyword{file}

