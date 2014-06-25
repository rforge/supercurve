% $Id$

\name{slidedesignerGUI}
\alias{slidedesignerGUI}
\alias{sdui}
\title{Graphical User Interface for slide design layout}
\description{
  Graphical User Interface for recording layout of a slide design.
}
\usage{
slidedesignerGUI()
sdui()
}
\details{
  The function \code{slidedesignerGUI} launches a Graphical User Interface
  for recording the layout of positive and negative controls of a slide
  design, used with the \pkg{SuperCurve} package. The GUI uses Tk widgets
  (via the R-Tcl/Tk interface by Peter Dalgaard) in order to provide a
  simple interface.

  The function \code{sdui} is an alias for the aforementioned function.
}
\section{Resources}{
  The Tk options database is loaded from user-specific defaults files, such
  as \file{.Xdefaults}, and resource databases loaded into the X server.
  The package will attempt to load its resources upon startup, following the
  X11R5 method of merging app-default files from multiple sources. The
  standard X11 paths are searched first, followed by paths specified by the
  environment variables \env{XFILESEARCHPATH}, \env{XAPPLRESDIR},
  \env{XUSERFILESEARCHPATH}, respectively. Unlike X11, \strong{ALL} matching
  files will be loaded, not just the first.

  One way to make use of the options database is to create a file named
  \file{slidedesignerGUI.ad} in your home directory, specifying any desired
  resources. Set your \env{XUSERFILESEARCHPATH} environment variable within
  \R with the command: \cr
  \code{Sys.setenv(XUSERFILESEARCHPATH=file.path(path.expand("~"), "\%N.ad"))}
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\examples{
  \dontrun{
slidedesignerGUI()
# -or-
sdui()
  }
}
\keyword{IO}

