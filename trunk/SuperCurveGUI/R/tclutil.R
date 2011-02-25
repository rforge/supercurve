###
### TCLUTIL.R
###

require(tcltk) || stop("tcltk support is missing")

tclpackage.require("Tcl", "8.4")               # Requires Tcl 8.4 or later


##-----------------------------------------------------------------------------
## Displays button widgets as "pressed in".
displayButtonAsPressed <- function(button) {
    .appEntryStr("displayButtonAsPressed")
    stopifnot(is.tkwin(button))
    stopifnot(tclvalue(tkwinfo.class(button)) == "Button")

    tkconfigure(button,
                relief="sunken")
}


##-----------------------------------------------------------------------------
## Displays button widgets as "normal".
displayButtonAsNormal <- function(button) {
    .appEntryStr("displayButtonAsNormal")
    stopifnot(is.tkwin(button))
    stopifnot(tclvalue(tkwinfo.class(button)) == "Button")

    tkconfigure(button,
                relief="raised")
}


##-----------------------------------------------------------------------------
## Returns widget bindtags as character vector
getBindtags <- function(widget) {
    stopifnot(is.tkwin(widget))

    tagstr <- as.character(tclvalue(tkbindtags(widget)))
    bindtags <- unlist(strsplit(tagstr, split=' ', fixed=TRUE))
}

