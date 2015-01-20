% $Id$

\name{RPPA-class}
\alias{RPPA-class}
\alias{RPPA}
\alias{is.RPPA}
\alias{dim,RPPA-method}
\alias{image,RPPA-method}
\alias{summary,RPPA-method}
\docType{class}
\title{Class "RPPA"}
\description{
  The RPPA class represents the raw quantification data from a
  reverse-phase protein array experiment.
}
\usage{
RPPA(file,
     path=".",
     antibody=NULL,
     software="microvigene",
     alt.layout=NULL)
is.RPPA(x)
\S4method{dim}{RPPA}(x)
\S4method{image}{RPPA}(x, measure="Mean.Net", main,
      colorbar=FALSE, col=terrain.colors(256), \dots)
\S4method{summary}{RPPA}(object, \dots)
}
\arguments{
  \item{file}{character string or \link[base]{connection} specifying text file
    containing quantifications of a reverse-phase protein array experiment}
  \item{path}{character string specifying the path from the current
    directory to the file. The default value assumes the file is
    contained in the current directory. If \code{file} is a connection,
    this argument is ignored.}
  \item{antibody}{character string specifying antibody name. If missing,
    default value is filename (referenced by \code{file} argument) without
    extension.}
  \item{software}{character string specifying the software used to generate
    the quantification file (see section \sQuote{Details} below)}
  \item{alt.layout}{character string specifying the name of the alternative
    layout to be used (see section \sQuote{Details} below)}
  \item{object}{object of class \code{RPPA}}
  \item{x}{object of class \code{RPPA}}
  \item{measure}{character string containing the name of the measurement column
    in \code{data} that should be displayed by the \code{image} method}
  \item{main}{character string used to title the image plot}
  \item{colorbar}{logical scalar that determines whether to include a
    color bar in the plot. If \code{TRUE}, the image cannot be used as
    one panel in a window with multiple plots. Default is \code{FALSE}.}
  \item{col}{graphics parameter used by \link[graphics]{image}. It is
    included here to change the default color scheme to use
    \link[grDevices]{terrain.colors}.}
  \item{\dots}{extra arguments for generic or plotting routines}
}
\details{
  The data frame slot (\code{data}) in a valid RPPA object constructed
  from a quantification file using the \code{RPPA} generator function
  is guaranteed to contain at least 6 columns of information:
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
  spot on an array. Additional columns may be included or added later.

  Other methods can be specified to read the quantification files. The
  \code{software} argument is used in the selection of the actual method
  to perform this function. For example, if the argument value is \dQuote{foo},
  the code will attempt to invoke method \code{read.foo} to read the file.
  The method will be passed a connection object to the file and should
  return a data frame containing the file's data. The method will be searched
  for in the global namespace, then within the package itself. The default
  value selects method \code{read.microvigene}, which this package provides
  to read \emph{MicroVigene} quantification files in text format. Another
  method, \code{read.arraypro}, is also provided to read \emph{Array-Pro}
  quantification files in text format.

  Likewise, the logical layout of the slide can also be changed. The
  \code{alt.layout} argument is used in the selection of the actual method to
  perform this function. For example, if the argument value is \dQuote{bar},
  the code will attempt to invoke method \code{layout.as.bar} to convert the
  physical layout of the data to that specified by the method itself.
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the
  \code{RPPA} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{data}:}{data.frame containing the contents of a
      quantification file}
    \item{\code{file}:}{character string specifying the name of the file that
      the data was loaded from}
    \item{\code{antibody}:}{character string specifying name of antibody}
  }
}
\section{Methods}{
  \describe{
    \item{dim}{
      \code{signature(x = "RPPA")}:\cr
      Returns the dimensions of the slide layout.}
    \item{image}{
      \code{signature(x = "RPPA")}:\cr
      Produces a "geographic" image of the measurement column named by
      the \code{measure} argument. The colors in the image represent the
      intensity of the measurement at each spot on the array, and the
      display locations match the row and column locations of the spot.
      Any measurement column can be displayed using this function. An
      optional color bar can be added, placed along the right edge.}
    \item{summary}{
      \code{signature(object = "RPPA")}:\cr
      Prints a summary of the underlying data frame.}
  }
}
\value{
  The \code{RPPA} generator returns an object of class \code{RPPA}.

  The \code{is.RPPA} method returns \code{TRUE} if its
  argument is an object of class \code{RPPA}.

  The \code{dim} method returns a numeric vector of length 4.

  The \code{image} method invisibly returns the \code{RPPA} object on
  which it was invoked.

  The \code{summary} method returns a summary of the underlying data frame.
}
\note{
  The previous release provided a method, \code{read.singlesubgrid}, which
  could convert a single subgrid physical layout (in MicroVigene format) into
  its actual logical one.

  As this package now supports more than one software package, this had
  to be reworked! To achieve the same thing in this release, use:\cr
  \code{RPPA(file, path, software="microvigene", alt.layout="superslide")}
}
\author{
  Kevin R. Coombes \email{kcoombes@mdanderson.org},
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\linkS4class{RPPADesign}},
  \code{\linkS4class{RPPAFit}}
}
\examples{
extdata.dir <- system.file("extdata", package="SuperCurveSampleData")

## Converts file from single subgrid to its logical equivalent (4x12x11x11)
txtdir <- file.path(extdata.dir, "rppaSingleSubgridData")
waldo <- RPPA("Waldo.txt",
              path=txtdir,
              software="microvigene",
              alt.layout="superslide")
dim(waldo)

txtdir <- file.path(extdata.dir, "rppaTumorData")
erk2 <- RPPA("ERK2.txt", path=txtdir)
dim(erk2)
summary(erk2)
image(erk2)
image(erk2, colorbar=TRUE)
image(erk2, "Vol.Bkg", main="Background Estimates", colorbar=TRUE)
}
\keyword{color}
\keyword{classes}
\keyword{file}
\keyword{hplot}

