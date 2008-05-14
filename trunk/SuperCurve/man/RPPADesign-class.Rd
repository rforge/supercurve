\name{RPPADesign-class}
\docType{class}
\alias{RPPADesign-class}
\alias{RPPADesign}
\alias{image,RPPADesign-method}
\alias{summary,RPPADesign-method}
\alias{names,RPPADesign-method}
\alias{seriesNames}
\alias{getSteps}

\title{The RPPADesign Class}
\description{
  This class represents the information that describes how a particular
  set of RPPA slides was designed.
}
\usage{
RPPADesign(raw,
           steps=rep(0, 1),
           series=factor(rep(0, 1)),
           grouping=c("byRow","byCol", "bySample", "blockSample"),
           ordering=c("decreasing","increasing"),
           alias=list(),
           center=FALSE,
           controls=list())

seriesNames(design)
getSteps(design)
\S4method{image}{RPPADesign}(x, \dots)
\S4method{summary}{RPPADesign}(object, \dots)
\S4method{names}{RPPADesign}(x)
}
\arguments{
  \item{raw}{A data frame or an RPPA object.}
  \item{steps}{An optional numeric vector listing the dilution step
    associated with each spot, on a logarithmic scale.}
  \item{series}{An optional character vector or factor identifying the
    dilution series to which each spot corresponds.}
  \item{grouping}{Describes the way dilution series are oriented on the
    array.}
  \item{ordering}{Are dilution series arranges in order of
    \code{increasing} or \code{decreasing} concentrations. Default is
    \code{decreasing}.}
  \item{alias}{A data frame containing two columns: Alias and Sample}
  \item{center}{A logical value: if TRUE, then dilution steps are
    centered around 0.}
  \item{controls}{A list containing the character strings that identify
    control spots on the array. }
  \item{x}{A RPPADesign object}
  \item{object}{A RPPADesign object}
  \item{design}{A RPPADesign object}
  \item{\dots}{The usual extra arguments for generic or plotting routines.}
}
\details{
  From their inception, reverse-phase protein array experiments have
  spotted samples on the array in diluiton series. Thus, a critical aspect
  of the design and analysis is to understand how the dilution series are
  placed on the array.

  The optional \code{grouping} and \code{ordering} arguments allows the
  user to specify several standard layouts without having to go into great
  detail. The most common layout is \code{byRow}, which indicates that
  each row of a subgrid on the array should be considered as a separate
  dilution series. Although considerably less common (for reasons related
  to the robotics of how arrays are printed), the \code{byCol} layout
  indicates that each column of a subgrid is its own dilution series. The
  \code{bySample} layout means that each unique sample name indicates its
  own dilution series. Finally, the \code{blockSample} layout indicates
  that all occurrences of a sample name within a subgrid (or block) refer
  to the same dilution series. The \code{blockSample} layout can be used,
  for example, when a dilution series is long enough to extend over more
  than one row of a subgrid. One layout we have seen used seven dilution
  steps followed by a control spot, contained in two successive rows of a
  design with 4x4 subgrids, leading to the pattern:
\deqn{7  6  5  4}
\deqn{3  2  1  C}

  If the design of an RPPA experiment does not follow one of the built-in
  patterns, you can create an object by supplying vectors of dilution
  series names (in the \code{series} argument) and corresponding dilution
  steps (in the \code{steps} argument) that explicitly provide the mapping
  for each spot.
}
\section{Objects from the Class}{
  Objects of the RPPADesign class should be constructed using the
  \code{RPPADesign} function.
}
\section{Slots}{
  \describe{
    \item{\code{layout}:}{A data frame}
    \item{\code{alias}:}{A data frame}
    \item{\code{sampleMap}:}{A character vector}
    \item{\code{controls}:}{A list containing the character strings that
      identify control spots on the array. Controls are not included as part
      of any dilution series.}
  }
}
\section{Methods}{
  \describe{
    \item{image(x, \dots)}{The \code{image} method produces a
      two-dimensional graphical display of the layout design. Colors are used
      to represent different dilution steps, and laid out in the same pattern
      as the rows and columns of the array. This provides a visual check that
      the design has been specified correctly.}
    \item{summary(object, \dots)}{The \code{summary} method lists the
      names of the control spots on the array and then prints a summary of the
      data frame describing the layout.}
    \item{names(x)}{The \code{names} method returns a character vector
      containing, for each non-control spot, the name of the dilution series
      to which that spot belongs.}
  }
}
\value{
  The \code{image} method invisibly returns the displayed matrix of
  dilution steps.

  The \code{summary} method returns the summary object of the
  \code{layout} data frame.

  The \code{names} method returns a character vector.

  The \code{getSteps} function returns a numeric vector containing, for
  each non-control spot, the step represented by that spot in its dilution
  series.

  The \code{seriesNames} function returns a character vector containing
  the names of the unique (non-control) dilution series on the array.
}
\references{KRC}
\author{Kevin R. Coombes <kcoombes@mdanderson.org>}
\seealso{
  \link{RPPA}
}
\examples{
path <- system.file("rppaTumorData", package="SuperCurve")
erk2 <- RPPA("ERK2.txt", path=path)
design <- RPPADesign(erk2, grouping="blockSample", center=TRUE)
image(design)
summary(design)

design <- RPPADesign(erk2,
                     grouping="blockSample",
                     controls=list("neg con", "pos con"))
image(design)
summary(design)

path <- system.file("rppaCellData", package="SuperCurve")
akt <- RPPA("Akt.txt", path=path)
# Uses duplicate 8-step dilution series within 4x4 subgrids.
# They are interleaved, with top two identical rows containing
# the first 4 steps and the bottom two identical rows
# containing the last 4 steps.
steps <- rep(c(rep(8:5, 2), rep(4:1, 2)), 40) - 4.5
rep.temp <- factor(paste('Rep', rep(rep(1:2, each=4), 80), sep=''))
series <- factor(paste(as.character(akt@data$Sample),
                             as.character(rep.temp),
                             sep='.'))
design40 <- RPPADesign(akt, steps=steps, series=series)
image(design40)
summary(design40)
}
\keyword{regression}

