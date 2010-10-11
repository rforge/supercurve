\name{tknotebook}
\alias{notebook_create}
\alias{notebook_page}
\alias{notebook_display}
\title{Tk notebook routines}
\description{
  These functions provide/control a simulated notebook widget.
}
\usage{
notebook_create(parent)
notebook_display(notebook, pagename)
notebook_page(notebook, pagename)
}
\arguments{
  \item{parent}{object of the class \code{tkwin} representing the
    parent widget}
  \item{notebook}{object of the class \code{tkwin} representing the
    notebook widget}
  \item{pagename}{character string specifying name of notebook page}
}
\details{
  Notebook widgets are created with \code{notebook_create}.

  The function \code{notebook_page} creates a notebook page, a container
  in which to embed other widgets to display.

  The function \code{notebook_display} displays the requested page.
}
\value{
  The \code{notebook_create} method returns a \code{tkwin} object
  representing the notebook.

  The \code{notebook_page} method returns a \code{tkwin} object
  representing a page in the notebook.

  Other methods are invoked for their side-effect.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\examples{
  \dontrun{
toplevel <- tktoplevel()
## Create notebook
notebook <- notebook_create(toplevel)
tkpack(notebook, side="bottom", expand=TRUE, fill="both", padx=4, pady=4)
npages <- 0
## Create notebook page and contents
pg1 <- notebook_page(notebook, "Page 1")
label <- tklabel(pg1, bitmap="info")
tkpack(label, side="left", padx=8, pady=8)
label <- tklabel(pg1, text="Something\non\nPage 1")
tkpack(label, side="left", expand=TRUE, pady=8)
npages <- npages + 1
## Create another notebook page and contents
pg2 <- notebook_page(notebook, "Page 2")
label <- tklabel(pg2, text="Something else on Page 2")
tkpack(label, side="left", expand=TRUE, padx=8, pady=8)
npages <- npages + 1
## Create page controls
choice.var <- tclVar("1")
controls <- tkframe(toplevel, class="Radiobox")
border <- tkframe(controls, borderwidth=2, relief="groove")
tkpack(border, expand=TRUE, fill="both")
tkpack(controls, side="top", fill="x", padx=4, pady=4)
for (i in seq_len(npages)) {
    pg <- sprintf("Page %d", i)
    rb <- tkradiobutton(controls,
                        text=paste("Show", pg),
                        variable=choice.var,
                        value=as.integer(i))
    cmd <- substitute(function() notebook_display(notebook, pgname),
                      list(pgname=pg))
    tkconfigure(rb, command=eval(cmd))
    tkpack(rb, side="top", anchor="w")
}
  }
}
\keyword{IO}

