###
### UTILS.R
###


##-----------------------------------------------------------------------------
## Used to enable providing consistent naming for measures.
.capwords <- function(s, strict=FALSE) {
    ## Check arguments
    stopifnot(is.character(s))

    ##-------------------------------------------------------------------------
    cap <- function(s) {
        paste(toupper(substring(s, 1, 1)),
              {
                  if (strict) {
                      tolower(substring(s, 2))
                  } else {
                      substring(s, 2)
                  }
              },
              sep="",
              collapse=".")
    }


    ## Begin processing
    return(sapply(strsplit(s, split="\\."),
                  cap,
                  USE.NAMES=!is.null(names(s))))
}


##-----------------------------------------------------------------------------
## Specifies measures used for determining location on lysate array.
.locationColnames <- function() {
    return(c("Main.Row",
             "Main.Col",
             "Sub.Row",
             "Sub.Col"))
}


##-----------------------------------------------------------------------------
## Specifies measures capable of being used for fits.
## N.B.: use intersection of this with what is actually available in data.frame
.fitColnames <- function() {
    return(c("Mean.Net",
             "Mean.Total",
             "Median.Net",
             "Median.Total"))
}


##-----------------------------------------------------------------------------
## Returns dimensions of slide layout as numeric vector.
.dimOfLayout <- function(layout) {
    ## Check arguments
    stopifnot(is.data.frame(layout))

    ## Begin processing
    return(sapply(.locationColnames(),
                  function(df, column) {
                      max(df[[column]])
                  },
                  df=layout))
}


##-----------------------------------------------------------------------------
## Tests whether pathname is absolute, with system-dependent results.
## On UNIX systems, a pathname is absolute if its prefix is "/".
## On Windows systems, a pathname is absolute if its prefix is a drive letter
## followed by "\\", or if its prefix is "\\\\".
## Based on original work by Henrik Bengtsson.
.isAbsolutePathname <- function(pathname) {
    ## Check arguments
    stopifnot(is.character(pathname) && length(pathname) == 1)

    ## Begin processing
    if (!nzchar(pathname)) {
        return(FALSE)
    }

    absolute <- switch(EXPR=.Platform$OS.type,
                       unix={
                           ## Tilde expansion
                           regexpr("^~", pathname) != -1
                       },
                       windows={
                           ## Drive paths
                           regexpr("^[A-Za-z]:(/|\\\\)", pathname) != -1 ||
                           ## Network paths
                           regexpr("^\\\\", pathname) != -1
                       },
                       stop(sprintf("unrecognized operating system family %s",
                                    sQuote(.Platform$OS.type))))

    if (absolute) {
        return(TRUE)
    }

    ## Split pathname into components
    components <- strsplit(pathname, split="[/\\]")[[1]]
    if (length(components) == 0) {
        return(FALSE)
    }

    absolute <- components[1] == ""
}


##-----------------------------------------------------------------------------
.pkgRversion <- function(pkgname) {
    ## Check arguments
    stopifnot(is.character(pkgname) && length(pkgname) == 1)

    ## Begin processing
    substring(packageDescription(pkgname)[["Built"]], 3, 5)
}


##-----------------------------------------------------------------------------
## A version of all.equal() for the slots of an object
slot.all.equal <- function(x,
                           y,
                           ...) {
    ## Check arguments
    stopifnot(isS4(x))
    stopifnot(isS4(y))

    ## Begin processing
    msg <- NULL
    slotnames <- slotNames(x)
    for (slotname in slotnames) {
        aeq <- all.equal(slot(x, slotname),
                         slot(y, slotname),
                         ...)
        if (!isTRUE(aeq)) {
            msg <- c(msg, paste("slot ", sQuote(slotname), ": ", aeq, sep=''))
        }
    }

    ## Pass or fail?
    if (is.null(msg)) {
        TRUE
    } else {
        msg
    }
}

