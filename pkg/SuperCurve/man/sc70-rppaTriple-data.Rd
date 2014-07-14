% $Id$

\name{rppaTriple-data}
\alias{rppaTriple-data}
\alias{rppaTriple}
\alias{tripledesign}
\alias{actb}
\alias{actb.gray}
\alias{cas3}
\alias{fak}
\alias{odc1}
\docType{data}
\title{ACTB, CAS3, FAK, and ODC1 expression in 14 fed/starved cell lines}
\description{
  This data set contains the expression levels of four proteins:
  beta-Actin (ACTB), Caspase 3 (CAS3), Focal adhesion kinase (FAK),
  and Ornithine decarboxylase (ODC1) from a study that was done
  to compare protein levels in 14 cell lines from both a \dQuote{fed}
  and a \dQuote{starved} state. There are two files included for
  beta-Actin, one that was scanned in color (actb) and the other
  in 16-bit grayscale (actb.gray); all other proteins were scanned
  in color.

  This data set also contains a description of the design used for
  the reverse-phase protein arrays from this study. Cell lysates
  were spotted on the array in six-step dilution series. The layout
  of the array consists of a grid of 6x6 subgrids. The first three
  rows of a subgrid contain 3 replicates of a cell line. The last
  three rows contain 3 replicates of another cell line. Each subgrid
  is replicated on the array 3 times, so that there are a total of
  9 replicates per cell line per state. The top part of the array
  contains the fed cell lines and the bottom part of the array
  contains the starved cell lines. There is one subgrid on the array
  that contains only buffer material and another subgrid that did
  not have anything printed (blank). There are a total of 18 spots
  each of buffer and blank material.
}
\usage{
data(rppaTriple)
}
\format{
  The objects \code{actb}, \code{actb.gray}, \code{cas3}, \code{fak},
  and \code{odc1} are objects of class \code{\link{RPPA}}. The object
  \code{tripledesign} is an object of class \code{\link{RPPADesign}}.
}
\source{Victor Levin}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\keyword{datasets}

