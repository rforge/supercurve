\name{tkmessagebox}
\alias{askokcancel}
\alias{askquestion}
\alias{askretrycancel}
\alias{askyesno}
\alias{showerror}
\alias{showinfo}
\alias{showwarning}
\title{Tk alert dialog routines}
\description{
  These functions are wrappers around the tkmessageBox() method, in the
  style of TkInter.
}
\usage{
showinfo(title, message, \dots)
showwarning(title, message, \dots)
showerror(title, message, \dots)
askquestion(title, message, \dots)
askokcancel(title, message, \dots)
askyesno(title, message, \dots)
askretrycancel(title, message, \dots)
}
\arguments{
  \item{title}{character string specifying title for dialog window}
  \item{message}{character string specifying message displayed inside the alert}
  \item{\dots}{extra arguments for \code{tkmessageBox} routine}
}
\details{
  All methods above display a modal message dialog to allow the user to
  provide input. The application will wait until the user responds.

  A list of other arguments is shown here:
  \describe{
    \item{default}{character string specifying the default button of the
      dialog}
    \item{detail}{character string specifying a secondary message, usually
      displayed in a smaller font under the main message}
    \item{parent}{object of the class \code{tkwin} representing the window
      of the application for which this dialog is being posted.}
    \item{type}{character string specifying predefined set of buttons to
      be displayed (\code{askquestion} only). Possible values are:
      \describe{
        \item{abortretryignore}{displays three buttons whose symbolic names are
          \samp{abort}, \samp{retry} and \samp{ignore}}
        \item{ok}{displays one button whose symbolic name is \samp{ok}}
        \item{okcancel}{displays two buttons whose symbolic names are
          \samp{ok} and \samp{cancel}}
        \item{retrycancel}{displays two buttons whose symbolic names are
          \samp{retry} and \samp{cancel}}
        \item{yesno}{displays two buttons whose symbolic names are
          \samp{yes} and \samp{no}}
        \item{yesnocancel}{displays three buttons whose symbolic names are
          \samp{yes}, \samp{no} and \samp{cancel}}
      }
    }
  }
}
\value{
  The \code{askquestion} method returns character string of the button's
  symbolic name corresponding to the user response.

  The \code{askokcancel} method returns \code{TRUE} if user responds \samp{OK}.

  The \code{askyesno} method returns \code{TRUE} if user responds \samp{YES}.

  The \code{askretrycancel} method returns \code{TRUE} if user responds
  \samp{RETRY}.

  Other methods are invoked for their side-effect.
}
\author{
  P. Roebuck \email{plroebuck@mdanderson.org}
}
\note{
  The \code{parent} argument controls display location. If specified, the
  dialog will be centered over the parent widget; otherwise, center screen.
}
\seealso{
  \code{\link[tcltk]{tkmessageBox}}
}
\examples{
  \dontrun{
## Display informational dialog
showinfo('Gumptions', 'Stupid is as stupid does.')

## Ask a question (string response)
button <- askquestion("Proceed?",
                      "Destroy Western Civilization?",
                      detail="It's going down the tubes anyway...",
                      type="yesnocancel",
                      default="cancel")

## Ask a question (logical response)
toplevel <- tktoplevel()
if (askyesno('Install',
             'Are you sure you want to install SuperVirus?',
             default="no",
             parent=toplevel)) {
    cat("yes", "\n")
} else {
    cat("no", "\n")
}
  }
}
\keyword{IO}

