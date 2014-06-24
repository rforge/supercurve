###
### $Id$
###

library(tcltk)
library(tclish)


##-----------------------------------------------------------------------------
.test.tclafter <- function(input, func, delay, expected) {
    stopifnot(is.list(input) && length(input) >= 1)
    stopifnot(is.function(func))
    stopifnot(is.list(expected) && length(expected) == length(input))

    funcenv <- new.env()
    assign(".testfunc", func, envir=funcenv)

    testenv <- new.env(parent=funcenv)
    sapply(seq_along(input),
           function(i, lst, env) {
               assign(names(lst)[i], lst[[i]], envir=env)
           },
           input,
           testenv)

    ##-------------------------------------------------------------------------
    delayedCmd <- function() {
        evalq(sapply(ls(),
                     .testfunc),
              envir=testenv)
    }

    cmd.id <- if (missing(delay)) {
                  sleep.secs <- 1
                  tclafter.idle(delayedCmd)
              } else {
                  stopifnot(is.numeric(delay) && length(delay) == 1)
                  sleep.secs <- ceiling((abs(delay) + 1) / (1000 - 1))
                  tclafter(delay, delayedCmd)
              }
    cat("cmd.id =", tclvalue(cmd.id), "\n")

    cat("sleep.secs =", sleep.secs, "\n")
    Sys.sleep(sleep.secs)

    output <- lapply(varnames <- ls(envir=testenv),
                     get,
                     envir=testenv)
    names(output) <- varnames

    isTRUE(all.equal(output, expected))
}


##-----------------------------------------------------------------------------
## Test executing a command after a time delay.
test.tclafter <- function(input, func, delay=500, expected) {
    stopifnot(is.list(input) && length(input) >= 1)
    stopifnot(is.function(func))
    stopifnot(is.numeric(delay) && length(delay) == 1)
    stopifnot(is.list(expected) && length(expected) == length(input))

    .test.tclafter(input, func, delay, expected)
}


##-----------------------------------------------------------------------------
## Test executing a command as an idle callback.
test.tclafter.idle <- function(input, func, expected) {
    stopifnot(is.list(input) && length(input) >= 1)
    stopifnot(is.function(func))
    stopifnot(is.list(expected) && length(expected) == length(input))

    .test.tclafter(input, func, expected=expected)
}


##-----------------------------------------------------------------------------
## Test cancelling a delayed command.
test.tclafter.cancel <- function(input=1, expected=input) {
    stopifnot(is.numeric(input) && length(input) >= 1)
    stopifnot(is.numeric(expected) && length(expected) == length(input))

    testenv <- new.env()
    assign("x", input, envir=testenv)

    ##-------------------------------------------------------------------------
    delayedCmd <- function() {
        evalq(x <- x + 1, envir=testenv)
    }

    cmd.id <- tclafter(2000, delayedCmd)
    cat("cmd.id =", tclvalue(cmd.id), " : ",
        tclvalue(tclafter.info(cmd.id)), "\n")

    Sys.sleep(1)    # sleep for half of the delay
    tclafter.cancel(cmd.id)
    Sys.sleep(1)    # sleep for rest of the delay

    output <- get("x", envir=testenv)

    isTRUE(all.equal(output, expected))
}


## Run tests
test.tclafter(input <- list(x=5, y=6, z=3),
              plus1 <- function(name.li) {
                  stopifnot(is.character(name.li))

                  stopifnot(exists(name.li, envir=parent.frame(n <- 3)))
                  assign(name.li,
                         get(name.li, envir=parent.frame(n)) + 1,
                         envir=parent.frame(n))
              },
              expected=lapply(input, function(li) { li + 1 }))

test.tclafter.idle(input <- list(x=5, y=6, z=3),
                   minus1 <- function(name.li) {
                       stopifnot(is.character(name.li))

                       stopifnot(exists(name.li, envir=parent.frame(n <- 3)))
                       assign(name.li,
                              get(name.li, envir=parent.frame(n)) - 1,
                              envir=parent.frame(n))
                   },
                   expected=lapply(input, function(li) { li - 1 }))

test.tclafter.cancel(input=c(1, 2, 3))

