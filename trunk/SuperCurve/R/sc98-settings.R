###
### SUPERCURVESETTINGS-CLASS.R
###


##=============================================================================
setClass("SuperCurveSettings",
         representation(txtdir="Directory",
                        imgdir="Directory",
                        outdir="Directory",
                        designparams="RPPADesignParams",
                        fitparams="RPPAFitParams",
                        version="character"),
         prototype(version="0.0-0"))


##-----------------------------------------------------------------------------
## Validity method
validSuperCurveSettings <- function(object) {

    cat("validating", class(object), "object", "\n")
    problems <- NULL

    ## Validate txtdir slot
    {
        path <- object@txtdir@path

        ## Ensure directory contains TXT files
        txt.re <- ".*[tT][xX][tT]$"
        if (length(list.files(path, pattern=txt.re)) == 0) {
            problems <- c(problems, "txt directory contains no text files")
        }
    }

    ## Validate imgdir slot
    {
        path <- object@imgdir@path

        ## Ensure directory contains TIFF files
        tiff.re <- ".*[tT][iI][fF]{1,2}$"
        if (length(list.files(path, pattern=tiff.re)) == 0) {
            problems <- c(problems, "img directory contains no TIFF files")
        }
    }

    ## Validate outdir slot
    {
        path <- object@outdir@path

        ## Ensure directory is writable
        if (!(file.access(path, mode=2) == 0)) {
            problems <- c(problems, "output directory is not writable")
        }
    }

    ## Pass or fail?
    if (!is.null(problems)) {
        return(problems)
    } else {
        return(TRUE)
    }
}

setValidity("SuperCurveSettings", validSuperCurveSettings)


##-----------------------------------------------------------------------------
## Generator method
SuperCurveSettings <- function(txtdir,
                               imgdir,
                               outdir,
                               designparams,
                               fitparams) {
    ## Check arguments
    if (!is.character(txtdir)) {
        stop(sprintf("argument %s must be character",
                     sQuote("txtdir")))
    }

    if (!is.character(imgdir)) {
        stop(sprintf("argument %s must be character",
                     sQuote("imgdir")))
    }

    if (!is.character(outdir)) {
        stop(sprintf("argument %s must be character",
                     sQuote("outdir")))
    }

    if (!is.RPPADesignParams(designparams)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("designparams"), "RPPADesignParams"))
    }

    if (!is.RPPAFitParams(fitparams)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("fitparams"), "RPPAFitParams"))
    }

    ## Create new class
    new("SuperCurveSettings",
        txtdir=as(txtdir, "Directory"),
        imgdir=as(imgdir, "Directory"),
        outdir=as(outdir, "Directory"),
        designparams=designparams,
        fitparams=fitparams,
        version=packageDescription("SuperCurve", fields="Version"))
}


##-----------------------------------------------------------------------------
## Returns a string representation of this instance. The content and format of
## the returned string may vary between versions. Returned string may be
## empty, but never null.
setMethod("paramString", "SuperCurveSettings",
          function(object,
                   designparams.slots,
                   fitparams.slots,
                   ...) {
    if (missing(designparams.slots)) {
        designparams.slots <- c("grouping",
                                "ordering",
                                "center",
                                "controls")
    }

    if (missing(fitparams.slots)) {
        fitparams.slots <- c("measure",
                             "model",
                             "method",
                             "trim",
                             "ci",
                             "ignoreNegative",
                             "warnLevel")
    }


    ##---------------------------------------------------------------------
    indent <- function(params.text,
                       indention="  ") {
        paste(unlist(lapply(strsplit(params.text, '\n'),
                            function(x, indention) {
                                paste(indention, x)
                            },
                            indention)),
              collapse="\n")
    }


    paste(paste("txtdir:", shQuote(object@txtdir@path)), "\n",
          paste("imgdir:", shQuote(object@imgdir@path)), "\n",
          paste("outdir:", shQuote(object@outdir@path)), "\n",
          "designparams:", "\n",
          indent(paramString(object@designparams, designparams.slots)), "\n",
          "fitparams:", "\n",
          indent(paramString(object@fitparams, fitparams.slots)), "\n",
          sep="")
})


##-----------------------------------------------------------------------------
is.SuperCurveSettings <- function(x) {
    inherits(x, "SuperCurveSettings")
}

