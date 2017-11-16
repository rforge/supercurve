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

  See corresponding manpage of the raw data for a description of the
  design of the \acronym{RPPA}.
}
\usage{
data(rppaTriple)
}
\details{
  The corresponding raw datafiles are available in the
  \file{extdata/rppaTripleData} subdirectory of the 
  \pkg{SuperCurveSampleData} package.
}
\format{
  The objects \code{actb}, \code{actb.gray}, \code{cas3}, \code{fak},
  and \code{odc1} are objects of class \code{\link{RPPA}}. The object
  \code{tripledesign} is an object of class \code{\link{RPPADesign}}.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{ 
  \code{\link[SuperCurveSampleData]{rppaTriple-extdata}}
}
\keyword{datasets}

