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

  See corresponding manpage of the raw data for a description of the
  design of the \acronym{RPPA}.
}
\usage{
data(rppaTumor)
}
\details{
  The corresponding raw datafiles are available in the
  \file{extdata/rppaTumorData} subdirectory of the
  \package{SuperCurveSampleData} package.
}
\format{
  The objects \code{erk2}, \code{gsk3}, and \code{jnk} are objects
  of class \code{\link{RPPA}}. The object \code{tDesign} is an
  object of class \code{\link{RPPADesign}}.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\link[SuperCurveSampleData]{rppaTumor-extdata}}
}
\keyword{datasets}

