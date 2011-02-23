###
### TKPROGRESSMONITOR.R
###

require(tcltk)


##
## Classes
##

##=============================================================================
setOldClass("tclVar")
setOldClass("tkwin")
setClassUnion("OptionalWidget", c("tkwin", "NULL"))
setClass("TkProgressMonitor",
         contains="SCProgressMonitor",                ## inheritance
         representation(widget="OptionalWidget",      ## dialog or NULL
                        stage.var="tclVar",           ## stage label tclvalue
                        marquee.var="tclVar",         ## marquee label tclvalue
                        label.var="tclVar"))          ## progbar label tclvalue


##-----------------------------------------------------------------------------
## Generates a TkProgressMonitor object.
TkProgressMonitor <- function(widget) {
    stopifnot(is.tkwin(widget))

    new("TkProgressMonitor",
        widget=widget,
        stage.var=tclVar(""),
        marquee.var=tclVar(""),
        label.var=tclVar(""))
}


##
## TkProgressMonitor
##

##-----------------------------------------------------------------------------
setReplaceMethod("progressStage",
    signature(object="TkProgressMonitor", value="character"),
    function(object,
             ...,
             value) {
    message('progressStage<-(TkProgressMonitor, character)')
        ## Perform superclass processing...
        object <- callNextMethod()
        ## Update widget-tracked variables
        tclvalue(object@stage.var)   <- progressStage(object)
        #progressMarquee(object)      <- ""
        tclvalue(object@marquee.var) <- ""
        tclvalue(object@label.var)   <- ""
    str(object)
        ## Update progress dialog 'stage' radiobox objects appropriately
        if (!is.null(dialog <- object@widget)) {
            tryCatch({
                radiobox <- .getRadioBoxFromDialog(dialog)
                children <- .getRadioButtons(radiobox)
    #cat("radiobox$ID:", radiobox$ID, "\n")
    #cat("value=", value, '\n')
    #cat("children:", children, "\n")
                stages <- getenv("stages")
                x.value <- pmatch(value, stages)

                ## Update radiobuttons
                newStageID <- children[x.value]
                currStageID <- if (x.value > 1) {
                                   children[x.value-1]
                               } else {
                                   NULL
                               }
    message(sprintf("******** %s ********", value))
                sapply(c(currStageID, newStageID),
                       function(buttonID) {
                           if (nzchar(buttonID)) {
                               radiobutton <- .Tk.newwin(buttonID)
                               tkconfigure(radiobutton,
                                           state="normal")
                               message(sprintf("\tinvoking %s (%s)",
                                               buttonID,
                                               tclvalue(tkcget(radiobutton,
                                                               "-state"))))
                               tkinvoke(radiobutton)
                           }
                       })

                tclupdate()
            },
            error=function(e) {
                e
            })
        }

        object
    })


##-----------------------------------------------------------------------------
setReplaceMethod("progressMarquee",
    signature(object="TkProgressMonitor", value="character"),
    function(object,
             ...,
             value) {
    message('enter progressMarquee<-(TkProgressMonitor, character)')
        ## Perform superclass processing...
        object <- callNextMethod()
        ## Update widget-tracked variables
        tclvalue(object@marquee.var) <- progressMarquee(object)
        #progressLabel(object)        <- ""
        tclvalue(object@label.var)   <- ""
    str(object)
        object
    })


##-----------------------------------------------------------------------------
setReplaceMethod("progressLabel",
    signature(object="TkProgressMonitor", value="character"),
    function(object,
             ...,
             value) {
    message('enter progressLabel<-(TkProgressMonitor, character)')
        ## Perform superclass processing...
        object <- callNextMethod()
        ## Update widget-tracked variables
        tclvalue(object@label.var) <- progressLabel(object)
        tclupdate()
    str(object)
        object
    })


##-----------------------------------------------------------------------------
setReplaceMethod("progressValue",
    signature(object="TkProgressMonitor", value="numeric"),
    function(object,
             ...,
             value) {
    message("progressValue<-(TkProgressMonitor, numeric)")
        ## Perform superclass processing...
        object <- callNextMethod()

    str(object)
        ## Update progressbar with updated values
        if (!is.null(dialog <- object@widget)) {
            tryCatch({
                progressbar <- .getProgressBarFromDialog(dialog)
    #cat("progressbar$ID:", progressbar$ID, "\n")
                ## :BUG: Doesn't seem to work...
                #varExists <- as.logical(tclvalue(tclinfo.exists(progressbar)))

    cat("value=", value, '\n')
                maximum <- progressMaximum(object)
    cat("maximum=", maximum, '\n')
                percent <- as.integer((value / maximum) * 100)
    cat("percent=", percent, '\n')
                progressbar_value(progressbar, percent)
            },
            error=function(e) {
                e
            })
        }

        object
    })
#trace("progressValue<-", browser)


##-----------------------------------------------------------------------------
setReplaceMethod("progressError",
    signature(object="TkProgressMonitor", value="logical"),
    function(object,
             ...,
             value) {
    message("progressError<-(TkProgressMonitor, logical)")
        ## Perform superclass processing...
        object <- callNextMethod()

        ## :TEMPORARY: Make sure user knows an error occurred...
        if (!is.null(dialog <- object@widget)) {
            progressbar <- .getProgressBarFromDialog(dialog)

            ##varExists <- as.logical(tclvalue(tclinfo.exists(progressbar)))
            ## :BUG: Doesn't seem to work... override for now
            varExists <- TRUE

            if (varExists) {
                progressbar_updatebarcolor(progressbar, "red")
                progressbar_value(progressbar, progressMaximum(object))
            } else {
                message("progress bar doesn't exist")
            }
        }

        object
    })


##-----------------------------------------------------------------------------
setReplaceMethod("progressDone",
    signature(object="TkProgressMonitor", value="logical"),
    function(object,
             ...,
             value) {
    message("progressDone<-(TkProgressMonitor, logical)")
        ## Perform superclass processing...
        object <- callNextMethod()

        ## :TEMPORARY: Make sure user knows the processing is now complete
        if (!is.null(dialog <- object@widget)) {
            progressbar <- .getProgressBarFromDialog(dialog)
            tkpack.forget(progressbar)
            ## :TODO: Add icon label for success/failure
            if (!progressError(object)) {
                progressMarquee(object) <- "SUCCESS"
                progressLabel(object) <- "Processing completed successfully!"
                ## :NOTE: SuperCurveGUI should overwrite the label with
                ## number of fitted slides upon return...
            } else {
                progressMarquee(object) <- "FAILED"
                progressLabel(object) <- "See session logfile for details..."
            }
        }

        object
    })

