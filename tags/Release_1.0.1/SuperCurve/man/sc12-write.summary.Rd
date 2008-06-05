\name{write.summary}
\alias{write.summary}
\title{TBD}
\description{
  This function produces a graphical summary for each array in an
  \code{RPPASet} and summarizes the quantification results into one file.  
}
\usage{
write.summary(rppaset, path, prefix="supercurve", graphs=TRUE, tiffdir=NULL)
}
\arguments{
  \item{rppaset}{object of class \code{RPPASet}}
  \item{path}{character string specifying directory where output should be
    stored}
  \item{prefix}{character string providing a filename prefix to be applied
    when creating result files}
  \item{graphs}{logical scalar specifying whether to save fit graphs}
  \item{tiffdir}{character string specifying directory containing TIFF images
    corresponding to the TEXT quantification files}
}
\details{
  Generates three CSV files: one for the raw concentrations, one for the
  \eqn{R^2} statistics, and one for the polished concentrations.
  If \code{tiffdir} is \code{NULL}, the directory is assumed to be a sibling
  directory to \code{path} named "tif". If \code{graphs} is \code{TRUE},
  two PNG files containing output graphs are created per protein. The
  ImageMagick "convert" binary is then used to merge these output graphs
  with the source TIFF files, generating an additional JPG file per protein.
}
\references{KRC}
\author{P. Roebuck \email{plroebuck@mdanderson.org}}
\seealso{
  \code{\link{RPPASet}}
}
\examples{
  \dontrun{
parentdir <- file.path("lysate")
txtdir <- file.path(parentdir, "txt")
tiffdir <- file.path(parentdir, "tif")
outputdir <- file.path(parentdir, "results")

designparams <- RPPADesignParams(grouping="blockSample",
                                 center=FALSE,
                                 controls=list("control, pos con, neg con"))
fitparams <- RPPAFitParams(measure="Mean.Net",
                           ignoreNegative=FALSE,
                           method="nlrob",
                           warnLevel=-1,
                           model="logistic")
fitset <- RPPASet(txtdir, designparams, fitparams)
write.summary(fitset, outputdir, graphs=TRUE, tiffdir=tiffdir)
  }
}
\keyword{models}

