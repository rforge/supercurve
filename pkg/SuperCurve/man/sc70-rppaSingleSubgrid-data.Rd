% $Id$

\name{rppaSingleSubgrid-data}
\alias{rppaSingleSubgrid-data}
\alias{rppaSingleSubgrid}
\alias{foo}
\alias{bar}
\alias{plugh}
\alias{waldo}
\alias{sdesign}
\docType{data}
\title{FOO, BAR, PLUGH, and WALDO expression}
\description{
  This data set contains the expression levels of four imaginary
  proteins: FOO, BAR, PLUGH, and WALDO. Each sample on the slide is
  printed as a 5-step dilution series using reverse-phase protein arrays.

  See corresponding manpage of the raw data for a description of the
  design of the \acronym{RPPA}.
}
\usage{
data(rppaSingleSubgrid)
}
\details{
  The corresponding raw datafiles are available in the
  \file{extdata/rppaSingleSubgridData} subdirectory of the 
  \package{SuperCurveSampleData} package.
}
\format{
  The objects \code{foo}, \code{bar}, \code{plugh}, and \code{waldo}
  are objects of class \code{\link{RPPA}} converted to use logical layout.
  The object \code{sdesign} is an object of class \code{\link{RPPADesign}}
  describing that logical layout.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{ 
  \code{\link[SuperCurveSampleData]{rppaSingleSubgrid-extdata}}
}
\keyword{datasets}

