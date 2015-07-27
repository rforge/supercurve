###
### $Id$
###


##=============================================================================
setClassUnion("OptionalString", c("character", "NULL"))

setClass("SuperCurveSettings",
         representation(txtdir="Directory",
                        imgdir="OptionalDirectory",
                        outdir="Directory",
                        designparams="RPPADesignParams",
                        spatialparams="OptionalRPPASpatialParams",
                        doprefitqc="logical",
                        fitparams="RPPAFitParams",
                        normparams="RPPANormalizationParams",
                        onlynormqcgood="logical",
                        antibodyfile="OptionalFilename",
                        software="OptionalString",
                        alt.layout="OptionalString",
                        version="character"),
         prototype(alt.layout=NULL,
                   version="0.0.0"))


##
## :NOTE: If custom read.<software> or layout.as.<alt.layout> routine used,
## settings may not be able to reproduce the runtime environment.
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

    ## Validate alt.layout slot
    {
        alt.layout <- object@alt.layout
        if (!is.null(alt.layout)) {
            ## Ensure layout method exists
            layoutMethod <- suppressWarnings(.getLayoutMethod(alt.layout))
            if (is.null(layoutMethod)) {
                msg <- c(msg, "no user-provided method for layout found")
            }
        }
    }

    ## Validate onlynormqcgood slot against doprefitqc slot
    if (object@onlynormqcgood && !object@doprefitqc) {
        msg <- c(msg, "cannot normalize only good slides unless qc performed")
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
                               normparams,
                               doprefitqc=FALSE,
                               onlynormqcgood=doprefitqc,
                               antibodyfile=NULL,
                               software=NULL,
                               alt.layout=NULL) {
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

    if (!is.RPPANormalizationParams(normparams)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("normparams"), "RPPANormalizationParams"))
    }

    if (!is.logical(doprefitqc)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("doprefitqc")))
    } else if (!(length(doprefitqc) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("doprefitqc")))
    }

    if (!is.logical(onlynormqcgood)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("onlynormqcgood")))
    } else if (!(length(onlynormqcgood) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("onlynormqcgood")))
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

    if (!is.null(alt.layout)) {
        if (!is.character(alt.layout)) {
            stop(sprintf("argument %s must be character",
                         sQuote("alt.layout")))
        } else if (!(length(alt.layout) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("alt.layout")))
        } else if (!nzchar(alt.layout)) {
            stop(sprintf("argument %s must not be empty string",
                         sQuote("alt.layout")))
        }
    }

    ## Create new class
    new("SuperCurveSettings",
        txtdir=as(txtdir, "Directory"),
        imgdir=if (!is.null(imgdir)) as(imgdir, "Directory") else NULL,
        outdir=as(outdir, "Directory"),
        designparams=designparams,
        spatialparams=spatialparams,
        doprefitqc=doprefitqc,
        fitparams=fitparams,
        normparams=normparams,
        onlynormqcgood=onlynormqcgood,
        antibodyfile=antibodyfile,
        software=software,
        alt.layout=alt.layout,
        version=packageDescription("SuperCurve", fields="Version"))
}


##-----------------------------------------------------------------------------
setMethod("write.summary", signature(object="SuperCurveSettings"),
          function(object,
                   path=as(object@outdir, "character"),
                   ...) {
    ## Check arguments
    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    } else if (!dir.exists(path)) {
        stop(sprintf("directory %s does not exist",
                     dQuote(path)))
    } else if (!dir.writable(path)) {
        stop(sprintf("directory %s is not writable",
                     dQuote(path)))
    }

    ##---------------------------------------------------------------------
    makeFileHeader <- function(string) {
        stopifnot(is.character(string) && length(string) == 1)

        paste("###",
              paste("###", string),
              "###",
              "\n",
              sep="\n")
    }


    ## Begin processing
    version <- packageDescription("SuperCurve", fields="Version")
    cat(makeFileHeader("SuperCurve settings"),
        paramString(object),
        paste("supercurve version:", version), "\n",
        "\n",  # blank line at EOF
        sep="",
        file=file.path(path, "sc-settings.txt"))

    invisible(NULL)
})


##-----------------------------------------------------------------------------
## Returns a string representation of this instance. The content and format of
## the returned string may vary between versions. Returned string may be
## empty, but never null.
setMethod("paramString", signature(object="SuperCurveSettings"),
          function(object,
                   designparams.slots,
                   fitparams.slots,
                   spatialparams.slots,
                   normparams.slots,
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

    if (missing(normparams.slots)) {
        normparams.slots <- c("name",
                              "method",
                              "arglist")
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
    spatialparams <- if (!is.null(object@spatialparams)) {
                         paramString(object@spatialparams, spatialparams.slots)
                     } else {
                         NULL
                     }
    fitparams     <- paramString(object@fitparams, fitparams.slots)
    normparams    <- paramString(object@normparams, normparams.slots)
    software      <- if (!is.null(object@software)) {
                         object@software
                     } else {
                         "microvigene"
                     }

    ## Create param string
    paste(sprintf("txtdir: %s\n", shQuote(object@txtdir@path)),
          sprintf("imgdir: %s\n", shQuote(imgdir)),
          sprintf("outdir: %s\n", shQuote(object@outdir@path)),
          sprintf("designparams:\n%s\n", indent(designparams)),
          if (!is.null(spatialparams)) {
              sprintf("spatialparams:\n%s\n", indent(spatialparams))
          } else {
              sprintf("dospatialadj: %s\n", FALSE)
          },
          if (!is.null(object@doprefitqc)) {
              sprintf("doprefitqc: %s\n", object@doprefitqc)
          },
          sprintf("fitparams:\n%s\n", indent(fitparams)),
          sprintf("normparams:\n%s\n", indent(normparams)),
          sprintf("onlynormqcgood: %s\n", object@onlynormqcgood),
          if (!is.null(object@antibodyfile)) {
              sprintf("antibodyfile: %s\n", shQuote(object@antibodyfile))
          },
          sprintf("software: %s\n", software),
          if (!is.null(object@alt.layout)) {
              sprintf("alt.layout: %s\n", object@alt.layout)
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

