% $Id$

\name{rppaCell-data}
\alias{rppaCell-data}
\alias{rppaCell}
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

  See corresponding manpage of the raw data for a description of the
  design of the \acronym{RPPA}.
}
\usage{
data(rppaCell)
}
\details{
  The corresponding raw datafiles are available in the
  \file{extdata/rppaCellData} subdirectory of the 
  \pkg{SuperCurveSampleData} package.
}
\format{
  The objects \code{akt}, \code{c.erk2}, and \code{ctnnb1} are
  objects of class \code{\link{RPPA}}. The object \code{design40}
  is an object of class \code{\link{RPPADesign}}.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{ 
  \code{\link[SuperCurveSampleData]{rppaCell-extdata}}
}
\keyword{datasets}

