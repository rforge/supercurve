###
### UTIL.R
###


##-----------------------------------------------------------------------------
## Returns TRUE if path represents a directory; otherwise, FALSE.
dir.exists <- function(path) {
    ## Check arguments
    stopifnot(is.character(path) && length(path) == 1)

    ##-------------------------------------------------------------------------
    dirTest <- function(x) {
        !is.na(isdir <- file.info(x)$isdir) & isdir
    }

    ## Begin processing
    file.exists(path) && dirTest(path)
}


##-----------------------------------------------------------------------------
## Remove spaces from beginning of string.
.trimleft <- function(string) {
    stopifnot(is.character(string))

    gsub("^[[:space:]]+", "", string)
}


##-----------------------------------------------------------------------------
## Remove spaces from end of string.
.trimright <- function(string) {
    stopifnot(is.character(string))

    gsub("[[:space:]]+$", "", string)
}


##-----------------------------------------------------------------------------
## Remove spaces from both ends of string.
.trim <- function(string) {
    stopifnot(is.character(string))

    .trimright(.trimleft(string))
}

