% $Id$

\name{rppaTumor-extdata}
\alias{rppaTumor-extdata}
\docType{data}
\title{ERK2, GSK3, and JNK expression in tumor samples}
\description{
  This data set contains the expression levels of three proteins: ERK2,
  GSK3, and JNK in 96 breast tumor samples and controls, measured in
  dilution series using reverse-phase protein arrays.

  This data set also contains a description of the design of the
  reverse-phase protein array used in a set of experiments to measure
  protein levels. Cell lysates were spotted on the array in seven-step
  dilution series with eith a positive or negative control at the end
  of the series. The layout of the array consisted of a grid of 4x4 subgrids.
  The first two rows of a subgrid contained a single dilution series and
  a negative control spot. The last two rows of the subgrid contained
  another dilution series and a positive control spot. 
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
    \item vol_net
    \item vol_bkg
    \item vol_dust
  }

  The file \file{proteinAssay.tsv} contains the following columns:

  \itemize{
    \item Antibody
    \item Filename
  }

  The file \file{layoutInfo.tsv} contains the following columns:

  \itemize{
    \item Alias
    \item Sample
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
  }

  The file \file{supercurve.csv} contains the following columns:

  \itemize{
    \item CA19-9
    \item ERK2
    \item GSK3
    \item JNK
    \item p38
  }
}
\source{
  Doris Swank and Gordon Mills
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\examples{
## Path to raw data directory
extdata.dir <- system.file("extdata", package="SuperCurveSampleData")
rawdata.dir <- file.path(extdata.dir, "rppaTumorData")
rawdata.dir

## Directory Listing
files <- list.files(rawdata.dir, full.names=TRUE)
files
}
\keyword{datasets}

