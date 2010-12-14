\name{supercurveGUI}
\alias{supercurveGUI}
\alias{scui}
\title{Graphical User Interface for the SuperCurve package}
\description{
  Graphical User Interface for the SuperCurve package.
}
\usage{
supercurveGUI()
scui()
}
\details{
  The function \code{supercurveGUI} launches a Graphical User Interface
  for the \pkg{SuperCurve} package. The GUI uses Tk widgets (via the
  R-Tcl/Tk interface by Peter Dalgaard) in order to provide a simple
  interface to the SuperCurve functions for analyzing reverse phase
  protein lysate arrays.

  The function \code{scui} is an alias for the aforementioned function.

  If set, the environment variable \env{SC_DIR} will be used as the
  initial location for the directory containing quantification files.
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
  \file{supercurveGUI.ad} in your home directory, specifying any desired
  resources. Set your \env{XUSERFILESEARCHPATH} environment variable within
  \R with the command: \cr
  \code{Sys.setenv(XUSERFILESEARCHPATH=file.path(path.expand("~"), "\%N.ad"))}

  For example, to cause \pkg{SuperCurve} to convert each quantification file
  as it's read from a single subgrid physical layout into its actual logical
  one by default, add the following resource value to the app-defaults file
  mentioned above: \cr
      \kbd{*software: superslide}
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\examples{
  \dontrun{
supercurveGUI()
# -or-
scui()
  }
}
\keyword{IO}

