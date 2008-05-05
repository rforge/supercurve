\name{RPPA-class}
\docType{class}
\alias{RPPA-class}
\alias{RPPA}
\alias{summary,RPPA-method}
\alias{image,RPPA-method}

\title{The RPPA Class}
\description{
  The RPPA class represents the raw quantification data from a
  reverse-phase protein array experiment.
}
\usage{
RPPA(filename, path = ".")
\S4method{summary}{RPPA}(object, ...)
\S4method{image}{RPPA}(x, measure = "Mean.Net", main = measure, colorbar
= FALSE, col = terrain.colors(256), ...)
}
\arguments{
  \item{filename}{The name of a file containing MicroVigene
    quantifications of a reverse-phase protein array experiment.}
  \item{path}{An optional argument giving the path from the current
    directory to the file. The default value assumes the file is
    contained in the current direectory.}
  \item{object}{An RPPA object.}
  \item{x}{An RPPA object.}
  \item{measure}{A character string containing the name of the
    measurement column in \code{data} that should be displayed by the
    \code{image} method.}
  \item{main}{A character string used to title the image plot}
  \item{colorbar}{A logical value that determines whether to include a
    color bar in the plot.  Default is FALSE.  If set to TRUE, then the
    image cannot be used as one panel in a window with multiple plots.}
  \item{col}{The usual graphics parameter used by \link{image}. It is
    included here to change the default color scheme to use
    \link{terrain.colors}.}
  \item{\dots}{The usual extra arguments for generic or plotting routines.}
}
\details{
  The data frame slot (\code{data}) in a valid RPPA object constructed
  from a MicroVigne input file using the \code{RPPA} function is
  guaranteed to contain at least 6 columns of information:
  \code{Main.Row}, \code{Main.Col}, \code{Sub.Row}, \code{Sub.Col},
  \code{Sample}, and \code{Mean.Net}.  The first four pieces of
  information give the logical location of a spot on an array, after
  which we get a unique identifier of the sample spotted at that
  location and a measurement that represents the background-corrected
  mean intensity of the spot. Additional columns may be included or may
  be added later.
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link{new}, the preferred method is to use the \code{RPPA} function.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{A data.frame containing the contents of a
      MicroVigene or other quantification file}
    \item{\code{file}:}{A character string: the name of the file that
      the data was loaded from}
  }
}
\section{Methods}{
  \describe{
    \item{summary(object, ...)}{The summary method prints a summary of
      the underlying data frame.}
    \item{image(x, measure="Mean.Net", main=measure, colorbar=FALSE,
      col=terrain.colors(256), ...)}{The image method produces a
      "geographic" image of the measurement column named by the
      \code{measure} argument.  The colors in the image represent the
      intensity of the measurement at each spot on the array, and the
      display locations match the row and column locations of the spot.
      Any measurement column can be displayed using this function.  An
      optional color bar can be added; this will be placed at the right
      edge.  In order to display the color bar, the method uses the
      \code{\link{layout}} function.  Since \code{layout} works on the
      complete device, this prohibits the inclusion of an image with a
      color bar as one panel in a window of multiple plots.}
  }
}
\value{
  The \code{RPPA} constructor returns an object of the \code{RPPA} class.

  The \code{summary} method returns a summary of the underlying data
  frame.

  The \code{image} method invisibly returns the \code{RPPA} object on
  which it was invoked.
}
\references{KRC}
\author{Kevin R. Coombes <kcoombes@mdanderson.org>}
\seealso{\code{\link{RPPAFit}}, \code{\link{RPPADesign}}}
\examples{
path <- system.file("rppaTumorData", package="SuperCurve")
erk2 <- RPPA("ERK2.txt", path=path)
summary(erk2)
image(erk2)
image(erk2, colorbar=TRUE)
image(erk2, "Vol.Bkg", main="Background Estimates", colorbar=TRUE)
}
\keyword{hplot}
\keyword{color}
\keyword{file}
