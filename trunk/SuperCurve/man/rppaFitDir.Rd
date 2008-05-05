\name{RPPAFitDir}
\alias{RPPAFitDir}
\title{Fit Dilution Curves for a Directory}
\description{
  Fit dilution curves for all the slide quantification files in a given directory.  
}
\usage{
RPPAFitDir(path, designparams, fitparams)
}
\arguments{
  \item{path}{A path to the directory containing the spot quantification files. These are assumed to be of the form *.txt}
  \item{designparams}{A \code{RPPADesignParams} object describing the layout
    of the array}
  \item{fitparams}{A \code{RPPAFitParams} object describing the fitting parameters to pass to \code{\link{RPPAFit}}}
}
\details{
  Quantify all the slides in a directory using RPPAFit. This returns an object containing slide data and fits for each slide.
  Typically this is followed by a call to \code{write.summary} to write the
  resulting quantifications and diagnostic plots to a directory.
}
\value{
  This function constructs and returns an object of the \code{RPPASet} class containing information about how the function was called 
  and a matrix of fitted intensities.  The returned \code{RPPASet} class contains at least the following slots:
  \item{rppas}{Array of \link{RPPA} objects containing slide data from the directory.}
  \item{fits}{Array of fitted concentrations for each slide}
}
\references{KRC}
\author{Kevin R. Coombes <kcoombes@mdanderson.org>}
\seealso{ \code{\link{RPPAFit-class}}, \code{\link{RPPA}},
  \code{\link{RPPADesign}} }
\examples{
if (FALSE) { # we did not want to bundle a bunch of image files with our code
	txt <- "Y:/MyData/txt" # quantification files
	tiffdir <- "Y:/MyData/tif"  # tif image files corresponding to each quantification
	results <- "Y:/MyData/results"  # output dir
	designparams <- RPPADesignParams(grouping="blockSample", center=FALSE, controls=list("control", "pos con", "neg con"))
	fitparams <- RPPAFitParams(measure = "Mean.Net", ignoreNegative=FALSE, method='nlrob', warnLevel=-1, verbose = FALSE, model='cobs') 
	rm(fitset) 
	fitset <- RPPAFitDir(txt, designparams, fitparams)
	write.summary(fitset, file = 'supercurve', path = results, normalize = 'median', graphs=TRUE, tiffdir = tiffdir)
}

if (FALSE) { # An even easier way to call this function is to use a simple gui we have set up
	sc()
}
	  
}
\keyword{models}
\keyword{regression}
\keyword{nonlinear}
\keyword{robust}
