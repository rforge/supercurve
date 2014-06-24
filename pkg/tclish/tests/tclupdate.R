###
### $Id$
###

library(tcltk)
library(tclish)


##-----------------------------------------------------------------------------
## Test processing pending events and idle callbacks.
test.tclupdate <- function(idletasks=FALSE) {
    stopifnot(is.logical(idletasks) && length(idletasks) == 1)

    testenv <- new.env()
    assign("done", FALSE, envir=testenv)

    ##-------------------------------------------------------------------------
    delayedCmd <- function() {
        evalq(done <- TRUE, envir=testenv)
    }

    if (idletasks) {
        tclafter.idle(delayedCmd)
        tclupdate("idletasks")
    } else {
        tclafter(500, delayedCmd)

        startTime <- proc.time()[3]
        while ((elapsed <- (proc.time()[3] - startTime)) < 1) {
            tclupdate()
        }
    }

    isTRUE(done <- get("done", envir=testenv))
}


## Run tests
test.tclupdate()
test.tclupdate(idletasks=TRUE)

