% $Id$

\name{tkappdefaults}
\alias{tkloadappdefaults}
\title{Tk options database routines}
\description{
  This \code{tkloadappdefaults} method provides a high-level method
  that allows an application to load the Tk options resource database
  from various application-default files.
}
\usage{
tkloadappdefaults(classNames,
                  priority="startupFile")
}
\arguments{
  \item{classNames}{character vector specifying class names, though
    typically just the name of the application}
  \item{priority}{character string (or integer) specifying priority level
    for this option. Default is \code{"startupFile"}}
}
\details{
  The Tk options database is loaded from user-specific defaults files, such
  as \file{.Xdefaults}, and resource databases loaded into the X server.
  The package will attempt to load its resources upon startup, following the
  X11R5 method of merging app-default files from multiple sources. The
  standard X11 paths are searched first, followed by paths specified by the
  environment variables \env{XFILESEARCHPATH}, \env{XAPPLRESDIR},
  \env{XUSERFILESEARCHPATH}, respectively. Unlike X11, \strong{ALL} matching
  files will be loaded, not just the first.

  The value of each of the environment variables \env{XFILESEARCHPATH} and
  \env{XUSERFILESEARCHPATH} is a colon-separated list of pathnames.\cr
  The pathnames may contain replacement characters as follows:

  \tabular{ll}{
    \%N \tab value of the application's class name \cr
    \%T \tab value of the file's type, the literal string
      "\samp{app-defaults}" \cr
    \%C \tab customization resource \cr
    \%L \tab language, locale, and codeset (e.g., "\samp{ja_JP.EUC}") \cr
    \%l \tab language part of \samp{\%L} (e.g., "\samp{ja}") \cr
  }

  If \code{getOption("verbose")} is \code{TRUE}, a traceback of all pathnames
  considered is displayed, regardless of existence.
}
\value{
  Though invoked for its side-effect, returns (invisibly) a list of all
  pathnames from which resource loading was attempted.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\link{optiondb_readfile}}
}
\examples{
  \dontrun{
## Set directory paths for app-defaults processing
homedir <- path.expand("~")
xuserfilesearchpath <- paste(file.path(homedir, "\%L", "\%T", "\%N\%C"),
                             file.path(homedir, "\%l", "\%T", "\%N\%C"),
                             file.path(homedir, "\%T", "\%N\%C"),
                             file.path(homedir, "\%N.ad"),
                             sep=.Platform$path.sep)
Sys.setenv(XAPPLRESDIR=system.file())
Sys.setenv(XUSERFILESEARCHPATH=xuserfilesearchpath)

## Create user 'app-defaults' directory, if necessary
appdefaultsdir <- file.path(homedir, 'app-defaults')
if (!file.exists(appdefaultsdir)) {
    mkdir(appdefaultsdir)
}

## Create some fake resource files
classname <- "Foo"    ## For an R Tcl/Tk GUI named "foo"
customization <- "-color"
appdefaultsfiles <- c(file.path(homedir,
                                sprintf("\%s.ad", classname)),
                      file.path(appdefaultsdir, classname),
                      file.path(appdefaultsdir,
                                sprintf("\%s\%s", classname, customization)))
file.create(appdefaultsfiles)

## Load application defaults
message('*** loading application defaults')
options(verbose=TRUE)
tkloadappdefaults(classname)
cat('\n')

## Add customization aspect
optiondb_add("*customization", customization)

## Load application defaults
message('*** loading application defaults (customized)')
tkloadappdefaults(classname)

## Cleanup
on.exit(file.remove(appdefaultsfiles))
on.exit(options(verbose=FALSE), add=TRUE)
on.exit(optiondb_add("*customization", ""), add=TRUE)
  }
}
\keyword{IO}

