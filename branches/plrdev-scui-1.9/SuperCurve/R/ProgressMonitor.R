###
### PROGRESSMONITOR.R
###

require(methods)
options(warn=1)


##
## Classes
##

##=============================================================================
## Virtual
setClass("ProgressMonitor",
         representation("VIRTUAL"))


##=============================================================================
setClass("DefaultProgressMonitor",
         contains="ProgressMonitor",                ## inheritance
         representation(range="BoundedRange",       ## progressbar range model
                        label="character",          ## progressbar label value
                        err="logical",              ## has error occurred?
                        done="logical"),            ## completed yet?
         prototype(err=FALSE,
                   done=FALSE))


##-----------------------------------------------------------------------------
is.ProgressMonitor <- function(x) {
    extends(class(x), "ProgressMonitor")
}


##-----------------------------------------------------------------------------
## Generates a DefaultProgressMonitor object.
DefaultProgressMonitor <- function(label,
                                   value,
                                   minimum=0,
                                   maximum=100) {
    new("DefaultProgressMonitor",
        range=BoundedRange(value, minimum=minimum, maximum=maximum),
        label=as.character(label))
}


##
## Methods
##

##-----------------------------------------------------------------------------
mkDefaultMethod <- function(methodName) {
    stopifnot(is.character(methodName) && length(methodName) == 1)

    ##-------------------------------------------------------------------------
    setMethod(methodName,
        signature(object="ProgressMonitor"),
        function(object) {
            stop(sprintf("%s method must be implemented by any subclass of %s",
                         sQuote(methodName),
                         sQuote("ProgressMonitor")))
        })
}


##-----------------------------------------------------------------------------
mkDefaultReplaceMethod <- function(methodName) {
    stopifnot(is.character(methodName) && length(methodName) == 1)

    ##-------------------------------------------------------------------------
    setReplaceMethod(methodName,
        signature(object="ProgressMonitor", value="ANY"),
        function(object,
                 ...,
                 value) {
            stop(sprintf("%s method must be implemented by any subclass of %s",
                         sQuote(methodName),
                         sQuote("ProgressMonitor")))
        })
}


methodBaseNames <- c("Label", "Value", "Maximum", "Minimum", "Error", "Done")
methodNames <- sprintf("progress%s", methodBaseNames)
sapply(methodNames, mkDefaultMethod)
sapply(methodNames, mkDefaultReplaceMethod)
rm(mkDefaultMethod)
rm(mkDefaultReplaceMethod)


##
## DefaultProgressMonitor
##

##-----------------------------------------------------------------------------
setMethod("progressLabel",
    signature(object="DefaultProgressMonitor"),
    function(object) {
        object@label
    })

##-----------------------------------------------------------------------------
setReplaceMethod("progressLabel",
    signature(object="DefaultProgressMonitor", value="character"),
    function(object,
             ...,
             value) {
        stopifnot(length(value) == 1)
        object@label <- value
        object
    })

##-----------------------------------------------------------------------------
setMethod("progressValue",
    signature(object="DefaultProgressMonitor"),
    function(object) {
        callGeneric(object@range)
    })

##-----------------------------------------------------------------------------
setReplaceMethod("progressValue",
    signature(object="DefaultProgressMonitor", value="numeric"),
    function(object,
             ...,
             value) {
        message("progressValue<-(DefaultProgressMonitor, numeric)")
        ## :BUG?: This works in SuperCurve itself, but not from SuperCurveGUI
        #object@range <- callGeneric(object@range, ..., value=value)
        ## Break encapsulation and manipulate internal slots directly
        #object@range@value <- value
        ## Invoke same method for BoundedRange object
        progressValue(object@range) <- value
        object
    })

##-----------------------------------------------------------------------------
setMethod("progressMinimum",
    signature(object="DefaultProgressMonitor"),
    function(object) {
        callGeneric(object@range)
    })

##-----------------------------------------------------------------------------
setReplaceMethod("progressMinimum",
    signature(object="DefaultProgressMonitor", value="numeric"),
    function(object,
             ...,
             value) {
        ## Using alternative for consistency
        #object@range <- callGeneric(object@range, ..., value=value)
        ## Invoke same method for BoundedRange object
        progressMinimum(object@range) <- value
        object
    })

##-----------------------------------------------------------------------------
setMethod("progressMaximum",
    signature(object="DefaultProgressMonitor"),
    function(object) {
        callGeneric(object@range)
    })

##-----------------------------------------------------------------------------
setReplaceMethod("progressMaximum",
    signature(object="DefaultProgressMonitor", value="numeric"),
    function(object,
             ...,
             value) {
        ## Using alternative for consistency
        #object@range <- callGeneric(object@range, ..., value=value)
        ## Invoke same method for BoundedRange object
        progressMaximum(object@range) <- value
        object
    })

##-----------------------------------------------------------------------------
setMethod("progressError",
    signature(object="DefaultProgressMonitor"),
    function(object) {
        object@err
    })

##-----------------------------------------------------------------------------
setReplaceMethod("progressError",
    signature(object="DefaultProgressMonitor", value="logical"),
    function(object,
             ...,
             value) {
        stopifnot(length(value) == 1)
        object@err <- value
        #object@done <- value
        ### :PLR?: progressDone(object) <- value
        progressDone(object) <- value
        object
    })

##-----------------------------------------------------------------------------
setMethod("progressDone",
    signature(object="DefaultProgressMonitor"),
    function(object) {
        object@done
    })

##-----------------------------------------------------------------------------
setReplaceMethod("progressDone",
    signature(object="DefaultProgressMonitor", value="logical"),
    function(object,
             ...,
             value) {
        stopifnot(length(value) == 1)
        object@done <- value
        object
    })

