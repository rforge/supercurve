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

