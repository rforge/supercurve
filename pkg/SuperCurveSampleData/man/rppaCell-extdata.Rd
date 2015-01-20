% $Id$

\name{rppaCell-extdata}
\alias{rppaCell-extdata}
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
\format{
  The MicroVigene \acronym{TXT} datafiles contain the following columns:

  \itemize{
    \item Main Row
    \item Main Col
    \item Sub Row
    \item Sub Col
    \item GeneID
    \item mean_net
    \item mean_total
    \item median_net
    \item vol_bkg
    \item vol_dust
  }

  The file \file{proteinAssay.tsv} contains the following columns:

  \itemize{
    \item Antibody
    \item Filename
  }

  The file \file{cellLineInfo.tsv} contains the following columns:

  \itemize{
    \item CellLine
    \item ProteinConcentration
    \item LoadingControl
  }
}
\source{
  Bryan Hennessey and Gordon Mills
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\examples{
## Path to raw data directory
extdata.dir <- system.file("extdata", package="SuperCurveSampleData")
rawdata.dir <- file.path(extdata.dir, "rppaCellData")
rawdata.dir

## Directory Listing
files <- list.files(rawdata.dir, full.names=TRUE)
files
}
\keyword{datasets}

