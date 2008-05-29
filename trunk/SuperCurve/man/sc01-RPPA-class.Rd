\name{RPPA-class}
\alias{RPPA-class}
\alias{RPPA}
\alias{summary,RPPA-method}
\alias{image,RPPA-method}
\docType{class}
\title{Class "RPPA"}
\description{
  The RPPA class represents the raw quantification data from a
  reverse-phase protein array experiment.
}
\usage{
RPPA(filename, path=".", blanks=NULL)
\S4method{summary}{RPPA}(object, \dots)
\S4method{image}{RPPA}(x, measure="Mean.Net", main=measure,
    colorbar=FALSE, col=terrain.colors(256), \dots)
}
\arguments{
  \item{filename}{character string specifying the name of a text file
    containing MicroVigene quantifications of a reverse-phase protein array
    experiment}
  \item{path}{character string specifying the path from the current
    directory to the file. The default value assumes the file is
    contained in the current directory.}
  \item{blanks}{logical vector; DEPRECATED}
  \item{object}{object of class \code{RPPA}}
  \item{x}{object of class \code{RPPA}}
  \item{measure}{character string containing the name of the measurement column
    in \code{data} that should be displayed by the \code{image} method}
  \item{main}{character string used to title the image plot}
  \item{colorbar}{logical scalar that determines whether to include a
    color bar in the plot. If \code{TRUE}, the image cannot be used as
    one panel in a window with multiple plots. Default is \code{FALSE}.}
  \item{col}{graphics parameter used by \link{image}. It is included here
    to change the default color scheme to use \link{terrain.colors}.}
  \item{\dots}{extra arguments for generic or plotting routines}
}
\details{
  The data frame slot (\code{data}) in a valid RPPA object constructed
  from a MicroVigene input file using the \code{RPPA} generator function is
  guaranteed to contain at least 6 columns of information:
  \tabular{ll}{
    \code{Main.Row} \tab logical location of spot on the array \cr
    \code{Main.Col} \tab logical location of spot on the array \cr
    \code{Sub.Row}  \tab logical location of spot on the array \cr
    \code{Sub.Col}  \tab logical location of spot on the array \cr
    \code{Sample}   \tab unique identifier of sample spotted at location \cr
    \code{Mean.Net} \tab measurement representing background-corrected
                         mean intensity of the spot \cr
  } 
  The first four components (taken together) give the logical location of a
  spot on an array.  Additional columns may be included or may be added later.
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to \link{new},
  the preferred method is to use the \code{RPPA} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{data.frame containing the contents of a
      MicroVigene or other quantification file}
    \item{\code{file}:}{character string specifying the name of the file that
      the data was loaded from}
  }
}
\section{Methods}{
  \describe{
    \item{summary(object, \dots)}{The summary method prints a summary of
      the underlying data frame.}
    \item{image(x, measure="Mean.Net", main=measure, colorbar=FALSE,
      col=terrain.colors(256), \dots)}{The image method produces a
      "geographic" image of the measurement column named by the
      \code{measure} argument. The colors in the image represent the
      intensity of the measurement at each spot on the array, and the
      display locations match the row and column locations of the spot.
      Any measurement column can be displayed using this function. An
      optional color bar can be added; this will be placed at the right
      edge.}
  }
}
\value{
  The \code{RPPA} generator returns an object of class \code{RPPA}.

  The \code{summary} method returns a summary of the underlying data
  frame.

  The \code{image} method invisibly returns the \code{RPPA} object on
  which it was invoked.
}
\references{KRC}
\author{Kevin R. Coombes, \email{kcoombes@mdanderson.org}}
\seealso{
  \code{\linkS4class{RPPAFit}},
  \code{\linkS4class{RPPADesign}}
}
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

