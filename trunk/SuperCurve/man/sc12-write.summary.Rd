\name{write.summary}
\alias{write.summary}
\title{TBD}
\description{
  This function TBD.
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
  TBD
  If \code{tiffdir} is \code{NULL}, the directory is assumed to be a sibling
  directory to \code{path} named "tif".
}
\references{KRC}
\author{P. Roebuck, \email{plroebuck@mdanderson.org}}
\seealso{
  \code{\link{RPPASet}}
}
\examples{
##TODO
}
\keyword{models}

