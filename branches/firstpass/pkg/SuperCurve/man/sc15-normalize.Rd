% $Id$

\name{normalize}
\alias{normalize,MatrixLike-method}
\title{Normalization}
\description{
  This function performs normalization for sample loading after quantification.
  It is typically invoked as part of the process of creating summary
  information from an \code{RPPASet} object.
}
\usage{
\S4method{normalize}{MatrixLike}(object,
          method=getRegisteredNormalizationMethodKeys(),
          calc.medians=TRUE,
          sweep.cols=calc.medians,
          \dots)
}
\arguments{
  \item{object}{data frame or matrix to be normalized}
  \item{method}{character string specifying name of method of sample loading
    normalization (see section \sQuote{Details} below)}
  \item{calc.medians}{logical scalar. If \code{TRUE}, calculate row and column
    median values from the data to be normalized.}
  \item{sweep.cols}{logical scalar. If \code{TRUE}, subtract column medians
    from data values prior to invoking the normalization method.}
  \item{\dots}{extra arguments for normalization routines}
}
\details{
  By default, column medians are subtracted from the input data values; these
  adjusted data values are then passed to the requested normalization routine
  for further processing.

  The \code{method} argument may be augmented with user-provided normalization
  methods. Package-provided values are:

  \tabular{lll}{
    medpolish \tab Tukey's median polish normalization\cr
    median    \tab sample median normalization\cr
    house     \tab housekeeping normalization\cr
    vs        \tab variable slope normalization\cr
  }

  Specifying \dQuote{median} as the \code{method} argument causes the row
  median to be subtracted from each sample. Specifying \dQuote{house} causes
  the median of one or more housekeeping antibodies to be used. The names of
  the antibodies to be used must be supplied as a named argument to this
  method. Specifying \dQuote{vs} causes the sample median to be used along
  with a multiplicative gamma (see reference below).
}
\value{
  Returns normalized concentrations as matrix appropriately annotated.
}
\references{
  Neeley ES, Kornblau SM, Coombes KR, Baggerly KA.\cr
  \emph{Variable slope normalization of reverse phase protein arrays}\cr
  Bioinformatics (2009) 25(11): 1384-1389.\cr
  \url{https://academic.oup.com/bioinformatics/article/25/11/1384/331482/Variable-slope-normalization-of-reverse-phase}
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org},
  E. Shannon Neeley \email{sneeley@stat.byu.edu}
}
\seealso{
  \code{\linkS4class{RPPASet}}
}
\examples{
antibodies <- c("FOO", "BAR", "PLUGH", "WALDO")
concs <- matrix(rnorm(1024),
                ncol=length(antibodies),
                dimnames=list(samples=NULL, antibodies=antibodies))

## Normalize using sample median
normconcs <- normalize(concs, method="median")
str(normconcs)

## Normalize using housekeeping antibodies
normconcs <- normalize(concs, method="house", antibodies=c("FOO", "PLUGH"))
str(normconcs)

## Normalize using variable slope
normconcs <- normalize(concs, method="vs")
str(normconcs)

## Normalize using Tukey's median polish (previous default method)
normconcs <- normalize(concs, method="medpolish", calc.medians=FALSE)
str(normconcs)

## Normalize using user-provided method (in this case, robust sample mean)
normalize.robustmean <- function(concs, trim=0, na.rm=FALSE) {
    stopifnot(is.matrix(concs) || is.data.frame(concs))
    stopifnot(is.numeric(trim))
    stopifnot(is.logical(na.rm))

    rowMean <- apply(concs, 1, mean, trim=trim, na.rm=na.rm)
    normconcs <- sweep(concs, 1, rowMean, FUN="-")

    ## Store method-specific info in "normalization" attribute
    attr(normconcs, "normalization") <- list(rowMean=rowMean)

    normconcs
}

registerNormalizationMethod("rmean", normalize.robustmean)
normconcs <- normalize(concs, method="rmean", trim=0.1, na.rm=TRUE)
str(normconcs)
}
\keyword{smooth}

