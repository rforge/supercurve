\name{Directory-class}
\alias{Directory-class}
\alias{Directory}
\alias{coerce,character,Directory-method}
\alias{coerce,Directory,character-method}
\docType{class}
\title{Class "Directory"}
\description{
  The Directory class represents a file system directory.
}
\usage{
Directory(path)
}
\arguments{
  \item{path}{character string specifying a directory}
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to \link{new},
  the preferred method is to use the \code{Directory} generator function.
}
\section{Slots}{
  \describe{
    \item{\code{path}:}{character string specifying a directory}
  }
}
\value{
  The \code{Directory} generator returns an object of class \code{Directory}.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\examples{
  txtdir <- Directory(system.file("rppaTumorData", package="SuperCurve"))
  txtdir.path <- as(txtdir, "character")
}
\keyword{file}

