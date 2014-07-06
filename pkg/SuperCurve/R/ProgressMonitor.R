###
### $Id$
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
setOldClass("difftime")
setClass("DefaultProgressMonitor",
         contains="ProgressMonitor",                ## inheritance
         representation(range="BoundedRange",       ## progressbar range model
                        label="character",          ## progressbar label value
                        err="logical",              ## has error occurred?
                        done="logical",             ## completed yet?
                        etime="ElapsedTime",        ## elapsed time object
                        elapsed="difftime"),        ## elapsed time (secs)
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
setMethod("initialize",
    signature(.Object="DefaultProgressMonitor"),
    function(.Object,
             ...) {
        .Object@elapsed <- structure(0, units="secs", class="difftime")
        callNextMethod(.Object, ...)
    })


##-----------------------------------------------------------------------------
setMethod("elapsed",
    signature(object="DefaultProgressMonitor"),
    function(object) {
        elapsed(object@etime, units="secs")
    })


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
             value) {
        stopifnot(length(value) == 1)
        object@label <- value
        object@elapsed <- elapsed(object)
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
             value) {
        #message("progressValue<-(DefaultProgressMonitor, numeric)")
        progressValue(object@range) <- value
        object@elapsed <- elapsed(object)
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
             value) {
        progressMinimum(object@range) <- value
        object@elapsed <- elapsed(object)
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
             value) {
        progressMaximum(object@range) <- value
        object@elapsed <- elapsed(object)
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
             value) {
        stopifnot(length(value) == 1)
        object@err <- value
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
             value) {
        stopifnot(length(value) == 1)
        object@done <- value
        object@elapsed <- elapsed(object)
        object
    })

