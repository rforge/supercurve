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
                        doprefitqc="logical",
                        antibodyfile="OptionalFilename",
                        software="OptionalString",
                        version="character"),
         prototype(version="0.0.0"))


##
## :NOTE: If custom read.software routine used, settings will not reproduce
## the runtime environment. How to cope? :TBD:
##


##-----------------------------------------------------------------------------
## Invoked by validObject() method.
validSuperCurveSettings <- function(object) {

    #cat("validating", class(object), "object", "\n")
    msg <- NULL

    ## Validate txtdir slot
    {
        path <- object@txtdir@path

        ## Ensure directory contains TEXT files
        txt.re <- "\\.*[tT][xX][tT]$"
        txtfiles <- list.files(path, pattern=txt.re)
        if (length(txtfiles) == 0) {
            msg <- c(msg, "txt directory contains no TEXT files")
        }
    }

    ## Validate imgdir slot
    {
        if (!is.null(object@imgdir)) {
            path <- object@imgdir@path

            ## Ensure directory contains TIFF files
            tif.re <- "\\.*[tT][iI][fF]{1,2}$"
            tiffiles <- list.files(path, pattern=tif.re)
            if (length(tiffiles) == 0) {
                #msg <- c(msg, "image directory contains no TIFF files")
                ## :PLR: K. Coombes wants warning here (2010/08/17)
                warning(sprintf("image directory %s contains no TIFF files",
                                dQuote(path)))
            } else {
                ## :TODO: Do they correspond to ANY of the TEXT files?
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
    is(x, "SuperCurveSettings")
}


##-----------------------------------------------------------------------------
## Generator method
SuperCurveSettings <- function(txtdir,
                               imgdir,
                               outdir,
                               designparams,
                               fitparams,
                               spatialparams=NULL,
                               doprefitqc=FALSE,
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

    if (!is.logical(doprefitqc)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("doprefitqc")))
    } else if (!(length(doprefitqc) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("doprefitqc")))
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
        doprefitqc=doprefitqc,
        antibodyfile=antibodyfile,
        software=software,
        version=packageDescription("SuperCurve", fields="Version"))
}


##-----------------------------------------------------------------------------
## Returns a string representation of this instance. The content and format of
## the returned string may vary between versions. Returned string may be
## empty, but never null.
setMethod("paramString", signature(object="SuperCurveSettings"),
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
    paste(sprintf("txtdir: %s\n", shQuote(object@txtdir@path)),
          sprintf("imgdir: %s\n", shQuote(imgdir)),
          sprintf("outdir: %s\n", shQuote(object@outdir@path)),
          sprintf("designparams:\n%s\n", indent(designparams)),
          sprintf("fitparams:\n%s\n", indent(fitparams)),
          if (!is.null(spatialparams)) {
              sprintf("spatialparams:\n%s\n", indent(spatialparams))
          } else {
              sprintf("dospatialadj: %s\n", FALSE)
          },
          if (!is.null(object@doprefitqc)) {
              sprintf("doprefitqc: %s\n", object@doprefitqc)
          },
          if (!is.null(object@antibodyfile)) {
              sprintf("antibodyfile: %s\n", shQuote(object@antibodyfile))
          },
          if (!is.null(object@software)) {
              sprintf("software: %s\n", object@software)
          },
          sep="")
})


##-----------------------------------------------------------------------------
## Returns list of prerequisite packages based on requested processing.
getPrerequisitePackages <- function(settings) {
    ## Check arguments
    if (!is.SuperCurveSettings(settings)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("settings"), "SuperCurveSettings"))
    }

    ## Begin processing

    ## Get model-specific prerequisites
    model.prereqs <- switch(EXPR=settings@fitparams@model,
                            cobs=c("cobs", "splines"),
                            logistic="boot")
    prerequisites <- model.prereqs

    ## Get fitmethod-specific prerequisites
    method.prereqs <- switch(EXPR=settings@fitparams@method,
                             nlrq="quantreg",
                             nlrob="robustbase")
    prerequisites <- c(prerequisites, method.prereqs)

    ## Get processing-specific prerequisites
    if (!is.null(settings@spatialparams)) {
        prerequisites <- c(prerequisites, "mgcv")
    }

    if (settings@doprefitqc) {
        prerequisites <- c(prerequisites, "timeDate")
    }

    prerequisites
}

