\name{tkprogressbar}
\alias{progressbar_create}
\alias{progressbar_updatebarcolor}
\alias{progressbar_value}
\title{Tk progressbar routines}
\description{
  These functions provide/control a simulated progressbar widget.
}
\usage{
progressbar_create(parent, col)
progressbar_value(progressbar, value)
progressbar_updatebarcolor(progressbar, col)
}
\arguments{
  \item{parent}{object of the class \code{tkwin} representing the
    parent widget}
  \item{progressbar}{object of the class \code{tkwin} representing the
    progressbar widget}
  \item{col}{character string specifying progress color}
  \item{value}{numeric scalar specifying progress with value in range [0..100]}
}
\details{
  Progressbar widgets are created with \code{progressbar_create}.

  The function \code{progressbar_value} sets the percent completion value
  that allows the widget to reflect the amount of progress that has been
  made, filling from left to right; that value is displayed as well.

  The function \code{progressbar_updatebarcolor} changes the color used
  by the widget to display progress.
}
\value{
  The \code{progressbar_create} method returns a \code{tkwin} object
  representing the progressbar.

  Other methods are invoked for their side-effect.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\examples{
  \dontrun{
toplevel <- tktoplevel()
progbar <- progressbar_create(toplevel, "blue")
tkpack(progbar)
for (complete in seq_len(100)) {
    progressbar_value(progbar, complete)
    Sys.sleep(0.1)
}
progressbar_updatebarcolor(progbar, "green")
  }
}
\keyword{IO}

