###
### $Id$
### Methods implementing Tcl routines not in tcltk package
###

require(tcltk) || stop("tcltk support is missing")


##
## Private Methods
##

##-----------------------------------------------------------------------------
is.tclVar <- function(x) {
    inherits(x, "tclVar")
}


##
## Public Methods
##

##-----------------------------------------------------------------------------
## Executes a command after a time delay.
tclafter <- function(...) {
    tcl("after", ...)
}


##-----------------------------------------------------------------------------
## Cancels execution of a delayed command.
tclafter.cancel <- function(...) {
    tcl("after", "cancel", ...)
}


##-----------------------------------------------------------------------------
## Executes a command as an idle callback.
tclafter.idle <- function(...) {
    tcl("after", "idle", ...)
}


##-----------------------------------------------------------------------------
## Returns information about existing event handlers.
tclafter.info <- function(...) {
    tcl("after", "info", ...)
}


##-----------------------------------------------------------------------------
## Evaluates command and trap exceptional returns.
tclcatch <- function(command,
                     result) {
    if (missing(result)) {
        tcl("catch", command)
    } else {
        tcl("catch", command, result)
    }
}


##-----------------------------------------------------------------------------
## Return information about state of Tcl interpreter.
tclinfo <- function(...) {
    tcl("info", ...)
}


##-----------------------------------------------------------------------------
## Return TRUE if Tcl variable exists.
tclinfo.exists <- function(...) {
    tcl("info", "exists", ...)
}


##-----------------------------------------------------------------------------
## Returns name of the computer on which this invocation is being executed.
tclinfo.hostname <- function(...) {
    tcl("info", "hostname", ...)
}


##-----------------------------------------------------------------------------
## Alternative version of tclRequire() that allows version checking.
tclpackage.require <- function(package,
                               version="",
                               warn=TRUE) {
    a <- if (!nzchar(version)) {
             try(tcl("package", "require", package), silent=TRUE)
         } else {
             try(tcl("package", "require", package, version), silent=TRUE)
         }

    if (inherits(a, "try-error")) {
        if (warn) {
            warning(gsub("\n", "", strsplit(a, "[tcl] ", fixed=TRUE)[[1]][2]))
        }
        FALSE
    } else {
        a
    }
}


##-----------------------------------------------------------------------------
## Process pending events and idle callbacks.
tclupdate <- function(...) {
    tcl("update", ...)
}

