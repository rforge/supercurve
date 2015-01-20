% $Id$

\name{rppaSingleSubgrid-extdata}
\alias{rppaSingleSubgrid-extdata}
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

  When these slide images were quantified using MicroVigene, the whole
  slide was treated as a single subgrid (1x1x44x132) to save time (by not
  having to perform alignment on 48 individual subgrids).

  The data set also contains a description of the design of the
  reverse-phase protein array used in a set of experiments to measure
  protein levels. The associated MicroVigene datafiles were converted back
  into their logical format (4x12x11x11) when read to facilitate downstream
  processing.
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
  }

  The file \file{slidedesign.tsv} contains the following columns:

  \itemize{
    \item Main.Row
    \item Main.Col
    \item Sub.Row
    \item Sub.Col
    \item SubgridAlias
    \item SpotType
    \item Dilution
    \item GridAlias
  }

  The file \file{slidedesign-singlesubgrid.tsv} contains the following columns:

  \itemize{
    \item Main.Row
    \item Main.Col
    \item Sub.Row
    \item Sub.Col
    \item SubgridAlias
    \item SpotType
    \item Dilution
    \item GridAlias
    \item Main.Row.Real
    \item Main.Col.Real
    \item Sub.Row.Real
    \item Sub.Col.Real
  }
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\examples{
## Path to raw data directory
extdata.dir <- system.file("extdata", package="SuperCurveSampleData")
rawdata.dir <- file.path(extdata.dir, "rppaSingleSubgridData")
rawdata.dir

## Directory Listing
files <- list.files(rawdata.dir, full.names=TRUE)
files
}
\keyword{datasets}

