\name{RPPAFitDir}
\alias{RPPAFitDir}
\title{Fit Dilution Curves for a Directory}
\description{
  Fit dilution curves for all the slide quantification files in a given
  directory.
}
\usage{
RPPAFitDir(path, designparams, fitparams, blanks=NULL)
}
\arguments{
  \item{path}{character string specifying path to the directory containing
    the spot quantification files (assumed to be text files)}
  \item{designparams}{object of class \code{RPPADesignParams} describing the
    layout of the array}
  \item{fitparams}{object of class \code{RPPAFitParams} describing the fitting
    parameters to pass to \code{\link{RPPAFit}}}
  \item{blanks}{logical vector TBD}
}
\details{
  Quantify all the slides in a directory using RPPAFit. This returns an object
  containing slide data and fits for each slide.
  Typically this is followed by a call to \code{write.summary} to write the
  resulting quantifications and diagnostic plots to a directory.
}
\value{
  This function constructs and returns an object of class \code{RPPASet}
  containing information about how the function was called and a matrix of
  fitted intensities. The returned \code{RPPASet} class contains at least the
  following slots:
  \item{rppas}{Array of \code{\link{RPPA}} objects containing slide data from
    the directory.}
  \item{fits}{Array of fitted concentrations for each slide}
}
\references{KRC}
\author{Kevin R. Coombes <kcoombes@mdanderson.org>}
\seealso{
  \code{\link{RPPAFit-class}},
  \code{\link{RPPA}},
  \code{\link{RPPADesign}}
}
\examples{
  \dontrun{
parentdir <- "Y:/MyData"
txtdir <- file.path(parentdir, "txt")     # quantification files
imgdir <- file.path(parentdir, "tif")     # images corresponding to each quantification
resdir <- file.path(parentdir, "results") # output files
designparams <- RPPADesignParams(grouping="blockSample",
                                 center=FALSE,
                                 controls=list("control",
                                               "pos con",
                                               "neg con"))
fitparams <- RPPAFitParams(measure="Mean.Net",
                           method="nlrob",
                           model="cobs",
                           ignoreNegative=FALSE,
                           warnLevel=-1,
                           verbose=FALSE)
rm(fitset)
fitset <- RPPAFitDir(txtdir, designparams, fitparams)
write.summary(fitset,
              namebase="supercurve",
              path=resdir,
              normalize="median",
              graphs=TRUE,
              tiffdir=imgdir)
  }

  \dontrun{
# :TBD: shitcan example if gui moved to separate package
# An even easier way to call this function is to use a simple gui we have set up
sc() # :TBD: gui really needs a more explicit name
  }
}
\keyword{models}
\keyword{regression}
\keyword{nonlinear}
\keyword{robust}

