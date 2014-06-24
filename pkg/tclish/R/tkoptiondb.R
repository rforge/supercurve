###
### $Id$
###

options(warn=1)
require(tcltk) || stop("tcltk support is missing")


##
## Public Methods
##

##-----------------------------------------------------------------------------
## Adds entry into Tcl options database with given priority.
optiondb_add <- function(pattern,
                         value,
                         priority="interactive",
                         verbose=FALSE) {
    stopifnot(is.character(pattern) && length(pattern) == 1)
    stopifnot(!missing(value))
    stopifnot(is.character(priority) || is.numeric(priority))
    stopifnot(is.logical(verbose) && length(verbose) == 1)

    if (is.character(priority)) {
        priorities <- c("widgetDefault",
                        "startupFile",
                        "userDefault",
                        "interactive")
        priority <- match.arg(priority, priorities)
    } else {
        stopifnot(length(priority) == 1)
        stopifnot(priority >= 0 && priority <= 100)
    }

    if (verbose) {
        cat("option", "add", pattern, value, priority, "\n")
    }

    tcl("option", "add", pattern, value, priority)
}


##-----------------------------------------------------------------------------
## Fetches value from Tcl options database.
optiondb_get <- function(widget=".",
                         rsrcName,
                         rsrcClass,
                         verbose=FALSE) {
    if (is.tkwin(widget)) {
        widget <- widget$ID
    }

    stopifnot(is.character(widget) && length(widget) == 1)
    stopifnot(is.character(rsrcName) && length(rsrcName) == 1)
    stopifnot(is.character(rsrcClass) && length(rsrcClass) == 1)

    if (verbose) {
        cat("option", "get", widget, rsrcName, rsrcClass, "\n")
    }

    tcl("option", "get", widget, rsrcName, rsrcClass)
}


##-----------------------------------------------------------------------------
## Adds file contents to Tcl options database with given priority.
optiondb_readfile <- function(filename,
                              priority="userDefault",
                              verbose=FALSE) {
    stopifnot(is.character(filename) && length(filename) == 1)
    stopifnot(is.character(priority) || is.numeric(priority))
    stopifnot(is.logical(verbose) && length(verbose) == 1)

    if (is.character(priority)) {
        priorities <- c("widgetDefault",
                        "startupFile",
                        "userDefault",
                        "interactive")
        priority <- match.arg(priority, priorities)
    } else {
        stopifnot(length(priority) == 1)
        stopifnot(priority >= 0 && priority <= 100)
    }

    if (verbose) {
        cat("option", "readfile", filename, priority, "\n")
    }

    tcl("option", "readfile", filename, priority)
}

