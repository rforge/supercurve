###
### SCPROGRESSMONITOR.R
###


##=============================================================================
setClass("SCProgressMonitor",
         representation=list("DefaultProgressMonitor", ## inheritance
                             stage="character",        ## stage label value
                             marquee="character"))     ## marquee label value


##-----------------------------------------------------------------------------
is.SCProgressMonitor <- function(x) {
    extends(class(x), "SCProgressMonitor")
}


##-----------------------------------------------------------------------------
## Generates a SCProgressMonitor object.
SCProgressMonitor <- function(stage="") {
    new("SCProgressMonitor",
        stage=stage)
}


##
## SCProgressMonitor
##

##-----------------------------------------------------------------------------
setMethod("progressStage",
    signature(object="SCProgressMonitor"),
    function(object) {
        #message('progressStage(SCProgressMonitor)')
        object@stage
    })


##-----------------------------------------------------------------------------
setReplaceMethod("progressStage",
    signature(object="SCProgressMonitor", value="character"),
    function(object,
             ...,
             value) {
        #message('progressStage<-(SCProgressMonitor, character)')
        stopifnot(length(value) == 1)
        object@stage <- value
        #progressMarquee(object) <- ""
        object@marquee <- ""
        object@label <- ""
        object@elapsed <- as.numeric(proc.time()["elapsed"]) - object@start
        object
    })


##-----------------------------------------------------------------------------
setMethod("progressMarquee",
    signature(object="SCProgressMonitor"),
    function(object) {
        #message('progressMarquee(SCProgressMonitor)')
        object@marquee
    })


##-----------------------------------------------------------------------------
setReplaceMethod("progressMarquee",
    signature(object="SCProgressMonitor", value="character"),
    function(object,
             ...,
             value) {
        #message('progressMarquee<-(SCProgressMonitor, character)')
        stopifnot(length(value) == 1)
        object@marquee <- value
        #progressLabel(object) <- ""
        object@label <- ""
        object@elapsed <- as.numeric(proc.time()["elapsed"]) - object@start
        object
    })

