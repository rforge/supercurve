###
### SUPERCURVESETTINGS-CLASS.R
###


##=============================================================================
setClassUnion("OptionalString", c("character", "NULL"))

setClass("SuperCurveSettings",
         representation(txtdir="Directory",
                        imgdir="OptionalDirectory",
                        outdir="Directory",
                        designparams="RPPADesignParams",
                        fitparams="RPPAFitParams",
                        spatialparams="OptionalRPPASpatialParams",
                        antibodyfile="OptionalFilename",
                        software="OptionalString",
                        version="character"),
         prototype(version="0.0-0"))


##
## :NOTE: If custom read.software routine used, settings will not reproduce
## the runtime environment. How to cope? :TBD:
##


##-----------------------------------------------------------------------------
## Invoked by validObject() method.
validSuperCurveSettings <- function(object) {

    cat("validating", class(object), "object", "\n")
    msg <- NULL

    ## Validate txtdir slot
    {
        path <- object@txtdir@path

        ## Ensure directory contains TXT files
        txt.re <- ".*[tT][xX][tT]$"
        if (length(list.files(path, pattern=txt.re)) == 0) {
            msg <- c(msg, "txt directory contains no text files")
        }
    }

    ## Validate imgdir slot
    {
        if (!is.null(object@imgdir)) {
            path <- object@imgdir@path

            ## Ensure directory contains TIFF files
            tiff.re <- ".*[tT][iI][fF]{1,2}$"
            if (length(list.files(path, pattern=tiff.re)) == 0) {
                msg <- c(msg, "img directory contains no TIFF files")
            }
        }
    }

    ## Validate outdir slot
    {
        path <- object@outdir@path

        ## Ensure directory is writable
        if (!dir.writable(path)) {
            msg <- c(msg, "output directory is not writable")
        }
    }

    ## Validate antibodyfile slot
    {
        file <- object@antibodyfile
        if (!is.null(file)) {
            if (!.isAbsolutePathname(file)) {
                file <- file.path(object@txtdir@path, file)
            }

            ## Ensure file exists
            if (!file.exists(file)) {
                msg <- c(msg, "antibody file does not exist")
            }
        }
    }

    ## Validate software slot
    {
        ## Ensure read method exists
        software <- object@software
        readMethod <- suppressWarnings(.getReadMethod(software))
        if (is.null(readMethod)) {
            msg <- c(msg, "no user-provided method for software found")
        }
    }

    ## Pass or fail?
    if (is.null(msg)) {
        TRUE
    } else {
        msg
    }
}

setValidity("SuperCurveSettings", validSuperCurveSettings)


##-----------------------------------------------------------------------------
is.SuperCurveSettings <- function(x) {
    inherits(x, "SuperCurveSettings")
}


##-----------------------------------------------------------------------------
## Generator method
SuperCurveSettings <- function(txtdir,
                               imgdir,
                               outdir,
                               designparams,
                               fitparams,
                               spatialparams=NULL,
                               antibodyfile=NULL,
                               software=NULL) {
    ## Check arguments
    if (!is.character(txtdir)) {
        stop(sprintf("argument %s must be character",
                     sQuote("txtdir")))
    }

    if (!is.null(imgdir)) {
        if (!is.character(imgdir)) {
            stop(sprintf("argument %s must be character",
                         sQuote("imgdir")))
        }
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

    if (!is.null(spatialparams)) {
        if (!is.RPPASpatialParams(spatialparams)) {
            stop(sprintf("argument %s must be object of class %s",
                         sQuote("spatialparams"), "RPPASpatialParams"))
        }
    }

    if (!is.null(antibodyfile)) {
        if (!is.character(antibodyfile)) {
            stop(sprintf("argument %s must be character",
                         sQuote("antibodyfile")))
        } else if (!(length(antibodyfile) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("antibodyfile")))
        } else if (!nzchar(antibodyfile)) {
            stop(sprintf("argument %s must not be empty string",
                         sQuote("antibodyfile")))
        }
    }

    if (!is.null(software)) {
        if (!is.character(software)) {
            stop(sprintf("argument %s must be character",
                         sQuote("software")))
        } else if (!(length(software) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("software")))
        } else if (!nzchar(software)) {
            stop(sprintf("argument %s must not be empty string",
                         sQuote("software")))
        }
    } else {
        software <- formals(RPPASet)$software
    }

    ## Create new class
    new("SuperCurveSettings",
        txtdir=as(txtdir, "Directory"),
        imgdir=if (!is.null(imgdir)) as(imgdir, "Directory") else NULL,
        outdir=as(outdir, "Directory"),
        designparams=designparams,
        fitparams=fitparams,
        spatialparams=spatialparams,
        antibodyfile=antibodyfile,
        software=software,
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
                   spatialparams.slots,
                   ...) {
    if (missing(designparams.slots)) {
        designparams.slots <- c("grouping",
                                "ordering",
                                "center",
                                "controls",
                                "aliasfile",
                                "designfile")
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

    if (missing(spatialparams.slots)) {
        spatialparams.slots <- c("cutoff",
                                 "k",
                                 "gamma",
                                 "plotSurface")
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


    ## Handle unspecified image directory
    imgdir <- if (!is.null(object@imgdir)) {
                  object@imgdir@path
              } else {
                  NULL
              }

    ## Handle parameters
    designparams  <- paramString(object@designparams, designparams.slots)
    fitparams     <- paramString(object@fitparams, fitparams.slots)
    spatialparams <- if (!is.null(object@spatialparams)) {
                         paramString(object@spatialparams, spatialparams.slots)
                     } else {
                         NULL
                     }

    ## Create param string
    paste(paste("txtdir:", shQuote(object@txtdir@path)), "\n",
          paste("imgdir:", shQuote(imgdir)), "\n",
          paste("outdir:", shQuote(object@outdir@path)), "\n",
          "designparams:", "\n",
          indent(designparams), "\n",
          "fitparams:", "\n",
          indent(fitparams), "\n",
          if (!is.null(spatialparams)) {
              paste("spatialparams:", "\n",
                    indent(spatialparams), "\n")
          },
          if (!is.null(object@antibodyfile)) {
              paste("antibodyfile:", shQuote(object@antibodyfile), "\n")
          },
          if (!is.null(object@software)) {
              paste("software:", object@software, "\n")
          },
          sep="")
})

