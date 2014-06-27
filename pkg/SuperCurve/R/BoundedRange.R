###
### $Id$
###

require(methods)
options(warn=1)


##
## Classes
##

##=============================================================================
setClass("BoundedRange",
         representation(minimum="integer",          ## bounded range min value
                        maximum="integer",          ## bounded range max value
                        value="integer"),           ## bounded range value
         prototype(minimum=as.integer(1),
                   maximum=as.integer(100)))


##-----------------------------------------------------------------------------
## Invoked by validObject() method.
validBoundedRange <- function(object) {

    #cat("validating", class(object), "object", "\n")
    msg <- NULL

    ## Validate value slot
    {
        value <- object@value
        minimum <- object@minimum
        maximum <- object@maximum

        ## Ensure valid range
        if (!(minimum <= maximum)) {
            msg <- c(msg, sprintf("invalid interval [%d..%d]",
                                  minimum, maximum))
        }

        ## Ensure value within range
        if (!((value >= minimum) &&
              (value <= maximum))) {
            msg <- c(msg, sprintf("value (%d) must be in interval [%d..%d]",
                                  value, minimum, maximum))
        }
    }

    ## Pass or fail?
    if (is.null(msg)) {
        TRUE
    } else {
        msg
    }
}

setValidity("BoundedRange", validBoundedRange)


##-----------------------------------------------------------------------------
is.BoundedRange <- function(x) {
    extends(class(x), "BoundedRange")
}


##-----------------------------------------------------------------------------
## Generates a BoundedRange object.
BoundedRange <- function(value,
                         minimum=1,
                         maximum=100) {
    stopifnot(length(value) == 1)
    stopifnot(length(minimum) == 1)
    stopifnot(length(maximum) == 1)

    new("BoundedRange",
        value=as.integer(value),
        minimum=as.integer(minimum),
        maximum=as.integer(maximum))
}


##
## Methods
##


##-----------------------------------------------------------------------------
setMethod("progressMinimum",
    signature(object="BoundedRange"),
    function(object) {
        object@minimum
    })

##-----------------------------------------------------------------------------
setReplaceMethod("progressMinimum",
    signature(object="BoundedRange", value="numeric"),
    function(object,
             ...,
             value) {
        #message("progressMinimum<-(BoundedRange, numeric)")
        stopifnot(length(value) == 1)
        object@minimum <- as.integer(value)
        #message("leaving progressMinimum<-(BoundedRange)")
        object
    })

##-----------------------------------------------------------------------------
setMethod("progressMaximum",
    signature(object="BoundedRange"),
    function(object) {
        object@maximum
    })

##-----------------------------------------------------------------------------
setReplaceMethod("progressMaximum",
    signature(object="BoundedRange", value="numeric"),
    function(object,
             ...,
             value) {
        #message("progressMaximum<-(BoundedRange, numeric)")
        stopifnot(length(value) == 1)
        object@maximum <- as.integer(value)
        #message("leaving progressMaximum<-(BoundedRange)")
        object
    })

##-----------------------------------------------------------------------------
setMethod("progressValue",
    signature(object="BoundedRange"),
    function(object) {
        object@value
    })

##-----------------------------------------------------------------------------
setReplaceMethod("progressValue",
    signature(object="BoundedRange", value="numeric"),
    function(object,
             ...,
             value) {
        #message("progressValue<-(BoundedRange, numeric)")
        stopifnot(length(value) == 1)
        object@value <- as.integer(value)
        #message("leave progressValue<-(BoundedRange)")
        object
    })

