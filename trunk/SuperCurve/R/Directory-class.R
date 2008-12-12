###
### DIRECTORY-CLASS.R
###


##=============================================================================
setClass("Directory",
         representation(path="character"))


##-----------------------------------------------------------------------------
## Invoked by validObject() method.
validDirectory <- function(object) {

    cat("validating", class(object), "object", "\n")
    msg <- NULL

    ## Validate path slot
    {
        path <- object@path

        ## Ensure only single path
        if (length(path) != 1) {
            msg <- c(msg, "path must be of length 1")
        }

        ## Ensure path exists (and appropriate filesystem object)
        if (!file.exists(path)) {
            msg <- c(msg, "path does not exist")
        } else if (!file.info(path)$isdir) {
            msg <- c(msg, "path is not directory")
        }
    }

    ## Pass or fail?
    if (is.null(msg)) {
        TRUE
    } else {
        msg
    }
}

setValidity("Directory", validDirectory)


##-----------------------------------------------------------------------------
is.Directory <- function(x) {
    inherits(x, "Directory")
}


##-----------------------------------------------------------------------------
## Coercion method
setAs("Directory", "character",
      function(from) {
          from@path
      })


##-----------------------------------------------------------------------------
## Coercion method
setAs("character", "Directory",
      function(from) {
          new("Directory",
              path=from)
      })


##-----------------------------------------------------------------------------
## Generator method
Directory <- function(path) {
    ## Check arguments
    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    }

    if (!file.exists(path)) {
        stop(sprintf("path %s does not exist",
                     dQuote(path)))
    } else if (!file.info(path)$isdir) {
        stop(sprintf("path %s is not directory",
                     dQuote(path)))
    }

    ## Create new class
    new("Directory",
        path=path)
}


##-----------------------------------------------------------------------------
## :TODO: make generic...
pathname <- function(object) {
    if (!is.Directory(object)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("object"), "Directory"))

    }

    return(as(object@path, "character"))
}

