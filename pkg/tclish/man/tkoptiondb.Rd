% $Id$

\name{tkoptiondb}
\alias{optiondb_add}
\alias{optiondb_get}
\alias{optiondb_readfile}
\title{Tk options database routines}
\description{
  These functions allow interaction with the Tk options resource database.
}
\usage{
optiondb_add(pattern,
             value,
             priority="interactive",
             verbose=FALSE)
optiondb_get(widget=".",
             rsrcName,
             rsrcClass,
             verbose=FALSE)
optiondb_readfile(filename,
                  priority="userDefault",
                  verbose=FALSE)
}
\arguments{
  \item{pattern}{character string containing the option being specified,
    and consists of names and/or classes separated by asterisks or dots,
    in the usual X format}
  \item{widget}{object of the class \code{tkwin} or character string
    specifying Tk widget from which to retrieve resource}
  \item{filename}{character string specifying pathname to resource file}
  \item{value}{character string specifying text to associate with pattern}
  \item{rsrcName}{character string specifying resource name}
  \item{rsrcClass}{character string specifying resource class name}
  \item{priority}{character string (or integer) specifying priority level
    for this option}
  \item{verbose}{logical scalar. If \code{TRUE}, displays the Tcl command}
}
\details{
  \code{optiondb_add} adds an entry into Tcl options database with given
  priority; \code{optiondb_readfile} adds a file's contents. To clear an
  entry, invoke \code{optiondb_add} with an empty string for \code{value}.

  \code{optiondb_get} fetches a value from Tcl options database.

  There is a fundamental difference between the way Tcl handles options
  and the way Xt handles options; Tcl has separate priorities and within
  a level prefers the latest matching option, Xt prefers options with
  "more exact" specifiers and has no level semantics.

  A resource class name begins with a capital letter; the resource name
  begins with a lowercase letter.

  There are four primary priority levels. From lowest to highest, they are:
  \describe{
    \item{widgetDefault}{Level 20.
      Used for default values hard-coded into widgets.}
    \item{startupFile}{Level 40.
      Used for options specified in application-specific startup files.}
    \item{userDefault}{Level 60.
      Used for options specified in user-specific defaults files, such as
      .Xdefaults, resource databases loaded into the X server, or
      user-specific startup files.}
    \item{interactive}{Level 80.
      Used for options specified interactively after the application starts
      running. (default)}
  }

  In addition, priorities may be specified numerically using integers
  between 0 and 100, inclusive. However, the numeric form is discouraged.
}
\value{
  The \code{optiondb_get} method returns a \code{tclObj} object representing
  the Tcl variable. Use the \code{tclvalue} method to extract its character
  string value.

  The other methods are invoked for their side-effect.
}
\author{
  P. Roebuck \email{proebuck@mdanderson.org}
}
\seealso{
  \code{\link[tcltk]{tclvalue}}
}
\examples{
  \dontrun{
## Set a couple button resource values
optiondb_add("*Button.background", "red", "startupFile")
optiondb_add("*Button.width", 100, "startupFile")

## Even make up "fake" application-specific resources
optiondb_add("*foo", "bar", "userDefault")

## Prove it works
toplevel <- tktoplevel()
tkpack(button <- tkbutton(toplevel)) # large red button appears

## Fetch "fake" application-specific value
foo.obj <- optiondb_get(rsrcName="foo", rsrcClass="Foo")
foo <- as.character(tclvalue(foo.obj))  # bar

## Read an application-defaults file containing resources
optiondb_readfile("/path/to/app-defaults/myapp.ad")
  }
}
\keyword{IO}

