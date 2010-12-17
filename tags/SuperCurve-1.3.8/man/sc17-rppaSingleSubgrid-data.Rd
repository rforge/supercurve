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
  proteins: FOO, BAR, PLUGH, and WALDO. Each sample on the \acronym{RPPA}
  slide is printed as a 5-step dilution series. There are 4x12 subgrids
  on each slide, corresponding to the 48 pins on the printer. Each subgrid
  contains 11 subrows and 11 subcolumns. Each subrow is composed of two
  5-step dilution series from left to right, each series itself from most
  concentrated to most diluted, and a positive control spot. The eleventh
  subcolumn is composed of two mirrored 5-step positive control series,
  with most concentrated on top/bottom and most diluted near the middle;
  the middle is a negative control (typically blank).

  When these slide images were quantified using Microvigene, the whole
  slide was treated as a single subgrid (1x1x44x132) to save time (by not
  having to perform alignment on 48 individual subgrids).

  The data set also contains a description of the design of the
  reverse-phase protein array used in a set of experiments to measure
  protein levels. The associated Microvigene datafiles were converted back
  into their logical format (4x12x11x11) when read to facilitate downstream
  processing.
}
\usage{
data(rppaSingleSubgrid)
}
\format{
  The objects \code{foo}, \code{bar}, \code{plugh}, and \code{waldo}
  are objects of class \code{\link{RPPA}} converted to use logical layout.
  The object \code{sdesign} is an object of class \code{\link{RPPADesign}}
  describing that logical layout.
}
\references{KRC}
\keyword{datasets}

