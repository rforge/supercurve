\name{rppaCell-data}
\alias{rppaCell-data}
\alias{akt}
\alias{ctnnb1}
\alias{c.erk2}
\alias{design40}
\docType{data}

\title{AKT, ERK2, and CTNNB1 expression in cell lines}
\description{
  This data set contains the expression levels of three proteins: AKT,
  ERK2, and beta catenin (CTNNB1) in 40 cell lines, measured in
  duplicate dilution series using reverse-phase protein arrays.

  The data set also contains a description of the design of the
  reverse-phase protein array used in a set of experiments to measure
  protein levels in 40 different cell lines. Cell lysates were spotted
  on the array in duplicate in eight-step dilution series. The layout of
  the array consisted of a grid of 4x4 subgrids. Each subgrid contained
  the duplicate dilution series for a single sample. Each of the
  identical top two rows of a subgrid contained the four most
  concentrated dilution steps (in decreasing concentrations from left to
  right), and the identical bottom two rows contained the four least
  concentrated dilution steps.
}
\usage{
data(rppaCell)
}
\format{
  The objects \code{akt}, \code{c.erk2}, and \code{ctnnb1} are
  objects of the \code{\link{RPPA}} class. The object \code{design40}
  is an object of the \code{\link{RPPADesign}} class.
}
\source{Bryan Hennessey and Gordon Mills}
\references{KRC}
\keyword{datasets}

