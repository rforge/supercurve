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

    return(sapply(.locationColnames(),
                  function(df, column) {
                      max(df[[column]])
                  },
                  df=layout))
}

