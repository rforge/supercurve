\name{Directory-class}
\alias{Directory-class}
\alias{Directory}
\alias{coerce,character,Directory-method}
\alias{coerce,Directory,character-method}
\alias{is.Directory}
\docType{class}
\title{Class "Directory"}
\description{
  The Directory class represents a file system directory.
}
\usage{
Directory(path)
is.Directory(x)
}
\arguments{
  \item{path}{character string specifying a directory}
  \item{x}{object of class \code{Directory}}
}
\section{Objects from the Class}{
  Although objects of the class can be created by a direct call to
  \link[methods]{new}, the preferred method is to use the \code{Directory}
  generator function.
}
\section{Slots}{
  \describe{
    \item{\code{path}:}{character string specifying a directory}
  }
}
\value{
  The \code{Directory} generator returns an object of class \code{Directory}.

  The \code{is.Directory} method returns \code{TRUE} if its
  argument is an object of class \code{Directory}.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\examples{
  pkgdir <- Directory(system.file(package="SuperCurve"))
  pkgdir.path <- as(pkgdir, "character")
}
\keyword{classes}
\keyword{file}

