\name{write.summary}
\alias{write.summary}
\title{TBD}
\description{
  This function produces a graphical summary for each array in an
  \code{RPPASet} and summarizes the quantification results into various files.  
}
\usage{
write.summary(rppaset,
              path,
              prefix="supercurve",
              graphs=TRUE,
              tiffdir=NULL)
}
\arguments{
  \item{rppaset}{object of class \code{RPPASet}}
  \item{path}{character string specifying directory where output should be
    stored}
  \item{prefix}{character string providing a filename prefix to be applied
    when creating result files}
  \item{graphs}{logical scalar specifying whether to save fit graphs}
  \item{tiffdir}{character string specifying directory containing
    \acronym{TIFF} images corresponding to the TEXT quantification files}
}
\details{
  Generates three \acronym{CSV} files: one for the raw concentrations, one
  for the \eqn{R^2} statistics, and one for the polished concentrations.
  If \code{tiffdir} is \code{NULL}, the directory is assumed to be a sibling
  directory to \code{path} named "tif". If \code{graphs} is \code{TRUE},
  two \acronym{PNG} files containing output graphs are created per antibody.
  The ImageMagick \file{convert} binary is then used to merge these output
  graphs with the source \acronym{TIFF} files, generating an additional
  \acronym{JPEG} file per antibody.
}
\references{KRC}
\author{P. Roebuck \email{plroebuck@mdanderson.org}}
\seealso{
  \code{\link{RPPASet}}
}
\examples{
  \dontrun{
parentdir <- file.path("lysate")
txtdir <- file.path(parentdir, "txt")     # quantification files
imgdir <- file.path(parentdir, "tif")     # and corresponding image files
outdir <- file.path(parentdir, "results") # output files

designparams <- RPPADesignParams(grouping="blockSample",
                                 center=FALSE,
                                 controls=list("control",
                                               "pos con",
                                               "neg con"))
fitparams <- RPPAFitParams(measure="Mean.Net",
                           method="nlrob",
                           model="logistic",
                           ignoreNegative=FALSE,
                           warnLevel=-1,
                           verbose=FALSE)
fitset <- RPPASet(txtdir,
                  designparams,
                  fitparams,
                  antibodyfile="proteinAssay.tsv")
write.summary(fitset,
              outdir,
              graphs=TRUE,
              tiffdir=imgdir)
  }
}
\keyword{models}

