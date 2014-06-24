% $Id$

\name{tktabnotebook}
\alias{tabnotebook_create}
\alias{tabnotebook_page}
\alias{tabnotebook_display}
\title{Tk tabnotebook routines}
\description{
  These functions provide/control a simulated tabbed notebook widget.
}
\usage{
tabnotebook_create(parent)
tabnotebook_display(tabnotebook, tabname)
tabnotebook_page(tabnotebook, tabname)
}
\arguments{
  \item{parent}{object of the class \code{tkwin} representing the
    parent widget}
  \item{tabnotebook}{object of the class \code{tkwin} representing the
    tabnotebook widget}
  \item{tabname}{character string specifying name of tabnotebook page}
}
\details{
  Tabnotebook widgets are created with \code{tabnotebook_create}.

  The function \code{tabnotebook_page} creates a tabbed notebook page,
  a container in which to embed other widgets to display.

  The function \code{tabnotebook_display} displays the requested page.
}
\value{
  The \code{tabnotebook_create} method returns a \code{tkwin} object
  representing the tabnotebook.

  The \code{tabnotebook_page} method returns a \code{tkwin} object
  representing a page in the tabbed notebook.

  Other methods are invoked for their side-effect.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\examples{
  \dontrun{
toplevel <- tktoplevel()
## Create tabnotebook
tabnotebook <- tabnotebook_create(toplevel)
tkpack(tabnotebook, expand=TRUE, fill="both")
## Create tabnotebook page and contents
pg1 <- tabnotebook_page(tabnotebook, "Page 1")
label <- tklabel(pg1, bitmap="info")
tkpack(label, side="left", padx=8, pady=8)
label <- tklabel(pg1, text="Something\non\nPage 1")
tkpack(label, side="left", expand=TRUE, pady=8)
## Create another tabnotebook page and contents
pg2 <- tabnotebook_page(tabnotebook, "Page 2")
label <- tklabel(pg2, text="Something else on Page 2")
tkpack(label, side="left", expand=TRUE, padx=8, pady=8)
## Controls are inherent; click on tabs to change pages
  ## :BUG: Example doesn't work, but it should. Why?
  }
}
\keyword{IO}

