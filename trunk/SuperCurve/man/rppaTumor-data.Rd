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
  protein levels in 101 different tumors. Cell lysates were spotted
  on the array in six-step dilution series. The layout of the array
  consisted of a grid of 4x6 subgrids. Each row of a subgrid contained
  the dilution series for a single sample, in decreasing concentration
  from left to right.
}
\usage{
data(rppaTumor)
}
\format{
  The objects \code{erk2}, \code{gsk3}, and \code{jnk} are objects
  of the \code{\link{RPPA}} class. The object \code{tDesign} is an
  object of the \code{\link{RPPADesign}} class.
}
\source{Bryan Hennessey and Gordon Mills}
\references{KRC}
\keyword{datasets}

