% $Id$

\name{rppaTumor-data}
\alias{rppaTumor-data}
\alias{rppaTumor}
\alias{tDesign}
\alias{jnk}
\alias{erk2}
\alias{gsk3}
\docType{data}
\title{ERK2, GSK3, and JNK expression in tumor samples}
\description{
  This data set contains the expression levels of three proteins: ERK2,
  GSK3, and JNK in 96 breast tumor samples and controls, measured in
  dilution series using reverse-phase protein arrays.

  This data set also contains a description of the design of the
  reverse-phase protein array used in a set of experiments to measure
  protein levels.  Cell lysates were spotted
  on the array in seven-step dilution series with eith a positive or
  negative control at the end of the series. The layout of the array
  consisted of a grid of 4x4 subgrids. The first two rows of a subgrid contained
  a single dilution series and a negative control spot.  The last two
  rows of the subgrid contained another dilution series and a positive control spot. 
}
\usage{
data(rppaTumor)
}
\format{
  The objects \code{erk2}, \code{gsk3}, and \code{jnk} are objects
  of class \code{\link{RPPA}}. The object \code{tDesign} is an
  object of class \code{\link{RPPADesign}}.
}
\source{Doris Swank and Gordon Mills}
\references{KRC}
\keyword{datasets}

