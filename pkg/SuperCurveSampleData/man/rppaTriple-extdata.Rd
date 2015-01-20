% $Id$

\name{rppaTriple-extdata}
\alias{rppaTriple-extdata}
\docType{data}
\title{ACTB, CAS3, FAK, and ODC1 expression in 14 fed/starved cell lines}
\description{
  This data set contains the expression levels of four proteins:
  beta-Actin (ACTB), Caspase 3 (CAS3), Focal adhesion kinase (FAK),
  and Ornithine decarboxylase (ODC1) from a study that was done
  to compare protein levels in 14 cell lines from both a \dQuote{fed}
  and a \dQuote{starved} state. There are two files included for
  beta-Actin, one that was scanned in color (\var{actb}) and the other
  in 16-bit grayscale (\var{actb.gray}); all other proteins were scanned
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
    \item mean_bkg
    \item bkg_used
    \item mean_dust
    \item median_net
    \item median_total
    \item median_bkg
    \item vol_net
    \item vol_total
    \item vol_bkg
    \item vol_dust
    \item cv_spot
    \item cv_bkg
    \item xcenter
    \item ycenter
    \item area_signal
    \item area_spot
    \item area_bkg
    \item solidity
    \item circularity
    \item roundness
    \item aspect
    \item dustiness
    \item rank
    \item type
  }

  The file \file{proteinAssay.tsv} contains the following columns:

  \itemize{
    \item Antibody
    \item Filename
    \item Alias
  }

  The file \file{layoutInfo.tsv} contains the following columns:

  \itemize{
    \item Alias
    \item Sample
  }
}
\source{
  Victor Levin
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\examples{
## Path to raw data directory
extdata.dir <- system.file("extdata", package="SuperCurveSampleData")
rawdata.dir <- file.path(extdata.dir, "rppaTripleData")
rawdata.dir

## Directory Listing
files <- list.files(rawdata.dir, full.names=TRUE)
files
}
\keyword{datasets}

