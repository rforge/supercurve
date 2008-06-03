###
### UTILS.R
###


##-----------------------------------------------------------------------------
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
.locationColnames <- function() {
    return(c("Main.Row",
             "Main.Col",
             "Sub.Row",
             "Sub.Col"))
}


##-----------------------------------------------------------------------------
.dimOfLayout <- function(layout) {
    ## Check arguments
    stopifnot(is.data.frame(layout))

    return(sapply(.locationColnames(),
                  function(df, column) {
                      max(df[[column]])
                  },
                  df=layout))
}

