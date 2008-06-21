###
### UTILS.R
###


##-----------------------------------------------------------------------------
## Used to enable providing consistent naming for measures.
.capwords <- function(s, strict=FALSE) {
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
## Returns dimensions of slide layout as numeric vector.
.dimOfLayout <- function(layout) {
    ## Check arguments
    stopifnot(is.data.frame(layout))

    return(sapply(.locationColnames(),
                  function(df, column) {
                      max(df[[column]])
                  },
                  df=layout))
}

