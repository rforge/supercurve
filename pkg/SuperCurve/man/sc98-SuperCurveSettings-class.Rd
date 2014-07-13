% $Id$

\name{SuperCurveSettings-class}
\alias{SuperCurveSettings-class}
\alias{SuperCurveSettings}
\alias{paramString,SuperCurveSettings-method}
\alias{write.summary,SuperCurveSettings-method}
\alias{fitCurveAndSummarizeFromSettings}
\alias{is.SuperCurveSettings}
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
                   fitparams,
                   spatialparams=NULL,
                   normparams,
                   doprefitqc=FALSE,
                   onlynormqcgood=doprefitqc,
                   antibodyfile=NULL,
                   software=NULL,
                   alt.layout=NULL)
fitCurveAndSummarizeFromSettings(settings, monitor=NULL)
is.SuperCurveSettings(x)
\S4method{write.summary}{SuperCurveSettings}(object,
              path=as(settings@outdir, "character"),
              \dots)
\S4method{paramString}{SuperCurveSettings}(object,
            designparams.slots,
            fitparams.slots,
            spatialparams.slots,
            normparams.slots,
            \dots)
}
\arguments{
  \item{txtdir}{character string specifying the directory containing
    quantification files in text format}
  \item{imgdir}{character string specifying the directory containing
    \acronym{TIFF} image files associated with each of the aforementioned
    quantification files, or \code{NULL}}
  \item{outdir}{character string specifying the directory where output
    from analysis should be stored. Must be writable.}
  \item{designparams}{object of class \code{RPPADesignParams}}
  \item{fitparams}{object of class \code{RPPAFitParams}}
  \item{spatialparams}{object of class \code{RPPASpatialParams}, or \code{NULL}}
  \item{normparams}{object of class \code{RPPANormalizationParams}}
  \item{doprefitqc}{logical scalar. If \code{TRUE}, performs pre-fit quality control.}
  \item{onlynormqcgood}{logical scalar. If \code{TRUE}, filters the slides
    to be normalized according to their pre-fit quality control scores.}
  \item{antibodyfile}{character string specifying filename containing mapping
    from quantification files to antibodies, or \code{NULL}}
  \item{software}{character string specifying the software used to generate
    the quantification files (see section \sQuote{Details} of
    \code{\link{RPPA}}), or \code{NULL} to use the default value.}
  \item{alt.layout}{character string specifying the name of the alternative
    layout to be used (see section \sQuote{Details} of \code{\link{RPPA}}),
    or \code{NULL} to use the implicit layout.)}
  \item{monitor}{object of (sub)class \code{ProgressMonitor}, or \code{NULL}}
  \item{object}{object of class \code{SuperCurveSettings}}
  \item{settings}{object of class \code{SuperCurveSettings}}
  \item{x}{object of class \code{SuperCurveSettings}}
  \item{path}{character string specifying the directory where settings
    summary should be saved. Must be writable.}
  \item{designparams.slots}{strings specifying \code{RPPADesignParams}
slotnames to display (for debuggging)}
  \item{fitparams.slots}{strings specifying \code{RPPAFitParams}
slotnames to display (for debuggging)}
  \item{spatialparams.slots}{strings specifying \code{RPPASpatialParams}
slotnames to display (for debuggging)}
  \item{normparams.slots}{strings specifying \code{RPPANormalizationParams}
slotnames to display (for debuggging)}
  \item{\dots}{extra arguments for generic routines}
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
      directory containing \acronym{TIFF} image files}
    \item{\code{outdir}:}{object of class \code{Directory} specifying the
      directory where analysis results should be stored}
    \item{\code{designparams}:}{object of class \code{RPPADesignParams}
      specifying the parameters that describe how a particular set of
      \acronym{RPPA} slides was designed}
    \item{\code{fitparams}:}{object of class \code{RPPAFitParams}
      specifying the parameters that control model fit}
    \item{\code{spatialparams}:}{object of class \code{RPPASpatialParams}
      specifying the parameters that control spatial adjustment}
    \item{\code{normparams}:}{object of class \code{RPPANormalizationParams}
      specifying the parameters that control normalization}
    \item{\code{doprefitqc}:}{logical scalar specifying whether to perform
      pre-fit quality control}
    \item{\code{onlynormqcgood}:}{logical scalar specifying whether to filter
      the slides to be normalized according to their pre-fit quality control
      scores}
    \item{\code{antibodyfile}:}{character string specifying filename containing
      mapping from quantification files to antibodies, or \code{NULL}}
    \item{\code{software}:}{character string specifying the software used to
      generate the quantification files, or \code{NULL}}
    \item{\code{alt.layout}:}{character string specifying the name of the
      alternative layout to be used, or \code{NULL}}
    \item{\code{version}:}{character string containing the version of this
      package used to construct the object}
  }
}
\section{Methods}{
  \describe{
    \item{paramString}{
      \code{signature(object = "SuperCurveSettings")}:\cr
      Returns string representation of object.}
    \item{write.summary}{
      \code{signature(object = "SuperCurveSettings")}:\cr
      Writes a text file representation of object.}
  }
}
\value{
  The \code{SuperCurveSettings} generator returns an object of class
  \code{SuperCurveSettings}.

  The \code{is.SuperCurveSettings} method returns \code{TRUE} if its
  argument is an object of class \code{SuperCurveSettings}.

  The \code{paramString} method returns a character vector, possibly
  empty but never \code{NULL}.

  The \code{write.summary} method invisibly returns \code{NULL}.
}
\section{Warning}{
  The \code{paramString} method should not be called by user except for
  informational purposes. The content and format of the returned string
  may vary between different versions of this package.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\linkS4class{Directory}},
  \code{\linkS4class{RPPADesignParams}},
  \code{\linkS4class{RPPAFitParams}},
  \code{\linkS4class{RPPASpatialParams}}
}
\examples{
  \dontrun{
extdatadir <- system.file("extdata", package="SuperCurve")

txtdir <- file.path(extdatadir, "rppaTumorData")
designparams <- RPPADesignParams(center=FALSE,
                                 controls=list("neg con", "pos con"),
                                 grouping="blockSample")
fitparams <- RPPAFitParams(ignoreNegative=FALSE,
                           measure="Mean.Total",
                           method="nlrob",
                           model="loess",
                           warnLevel=-1)
normparams <- RPPANormalizationParams(method="median")
settings <- SuperCurveSettings(txtdir=txtdir,
                               imgdir=NULL,
                               outdir=tempdir(),
                               designparams=designparams,
                               spatialparams=NULL,
                               fitparams=fitparams,
                               normparams=normparams)
fitCurveAndSummarizeFromSettings(settings)
  }
}
\keyword{classes}
\keyword{file}

