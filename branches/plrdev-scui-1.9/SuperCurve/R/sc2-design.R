###
### RPPADESIGN.R
###


##=============================================================================
setClass("RPPADesign",
         representation(call="call",
                        layout="data.frame",
                        alias="list",
                        sampleMap="character",
                        controls="list"))


##=============================================================================
setClassUnion("OptionalFilename", c("character", "NULL"))
setClassUnion("OptionalList", c("list", "NULL"))
setClass("RPPADesignParams",
         representation(steps="numeric",
                        series="factor",
                        grouping="character",
                        ordering="character",
                        center="logical",
                        controls="OptionalList",
                        alias="OptionalList",
                        aliasfile="OptionalFilename",
                        designfile="OptionalFilename"))


##-----------------------------------------------------------------------------
## Invoked by validObject() method.
validRPPADesign <- function(object) {

    #cat("validating", class(object), "object", "\n")
    msg <- NULL

    ## Ensure required columns
    reqdMeasures <- c(.locationColnames(), "Sample")
    found <- reqdMeasures %in% colnames(object@layout)
    if (!all(found)) {
        missingColumns <- reqdMeasures[!found]
        msg <- c(msg, sprintf(ngettext(length(missingColumns),
                                       "missing required column %s",
                                       "missing required columns %s"),
                              paste(dQuote(missingColumns), collapse = ", ")))
    }

    ## Pass or fail?
    if (is.null(msg)) {
        TRUE
    } else {
        msg
    }
}

setValidity("RPPADesign", validRPPADesign)


##-----------------------------------------------------------------------------
is.RPPADesign <- function(x) {
    is(x, "RPPADesign")
}


##-----------------------------------------------------------------------------
is.RPPADesignParams <- function(x) {
    is(x, "RPPADesignParams")
}


##-----------------------------------------------------------------------------
RPPADesignParams <- function(steps=rep(0, 1),
                             series=factor(rep(0, 1)),
                             grouping=c("byRow",
                                        "byCol",
                                        "bySample",
                                        "blockSample"),
                             ordering=c("decreasing",
                                        "increasing"),
                             alias=NULL,
                             center=FALSE,
                             controls=NULL,
                             aliasfile=NULL,
                             designfile=NULL,
                             path=".") {
  #:KRC:  Since you have defined a validity function, why are you
  # replicating the error checking?
    ## Check arguments
    if (!is.numeric(steps)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("steps")))
    }

    if (!(is.character(series) || is.factor(series))) {
        stop(sprintf("argument %s must be character or factor",
                     sQuote("series")))
    }

    if (all(c(length(steps), length(series)) == 1)) {
        grouping <- match.arg(grouping)
        ordering <- match.arg(ordering)

        ## Further checking deferred until RPPADesignFromParams...
    } else if (any(c(length(steps), length(series)) == 1)) {
        stop(sprintf("arguments %s and %s must both be specified if either is",
                     sQuote("steps"),
                     sQuote("series")))
    } else {
        ## Unnecessary when steps and series are specified
        grouping <- as.character(NA)
        ordering <- as.character(NA)
        center <- as.logical(NA)

        ## Further checking deferred until RPPADesignFromParams...
    }

    if (!is.null(alias)) {
        if (!(is.list(alias) || is.data.frame(alias))) {
          # why can't it be coerce'd into one of those?
            stop(sprintf("argument %s must be list or data.frame, if specified",
                         sQuote("alias")))
        } else if (is.data.frame(alias)) {
            alias <- as.list(alias)
        }
    }

    ## Convert numeric argument to logical counterpart
    if (is.numeric(center)) {
        center <- as.logical(center)
    }
    if (!is.logical(center)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("center")))
    } else if (!(length(center) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("center")))
    }

    if (!is.null(controls)) {
        if (!(is.list(controls) || is.character(controls))) {
            stop(sprintf("argument %s must be character or list, if specified",
                         sQuote("controls")))
        } else if (is.list(controls)) {
            if (any(sapply(controls, is.character) == FALSE)) {
                stop(sprintf("argument %s components must be character",
                             sQuote("controls")))
            } else if (any(sapply(controls, length) > 1)) {
                stop(sprintf("argument %s components must be of length 1",
                             sQuote("controls")))
            }
        } else if (is.character(controls)) {
            controls <- as.list(controls)
        }
    }

    if (!is.null(aliasfile)) {
        if (!is.null(alias)) {
            stop(sprintf("arguments %s and %s are mutually exclusive",
                         sQuote("alias"),
                         sQuote("aliasfile")))
        }

        if (!(length(aliasfile) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("aliasfile")))
        } else if (!nzchar(aliasfile)) {
            stop(sprintf("argument %s must not be empty string",
                         sQuote("aliasfile")))
        }

        if (!.isAbsolutePathname(aliasfile)) {
            if (!is.character(path)) {
                stop(sprintf("argument %s must be character",
                             sQuote("path")))
            } else if (!(length(path) == 1)) {
                stop(sprintf("argument %s must be of length 1",
                             sQuote("path")))
            }

            aliasfile <- file.path(path, aliasfile)
        }

        ## Further checking deferred until RPPADesignFromParams...
    }

    if (!is.null(designfile)) {
        if (!is.null(controls)) {
            stop(sprintf("arguments %s and %s are mutually exclusive",
                         sQuote("controls"),
                         sQuote("designfile")))
        }

        if (!(length(designfile) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("designfile")))
        } else if (!nzchar(designfile)) {
            stop(sprintf("argument %s must not be empty string",
                         sQuote("designfile")))
        }

        if (!.isAbsolutePathname(designfile)) {
            if (!is.character(path)) {
                stop(sprintf("argument %s must be character",
                             sQuote("path")))
            } else if (!(length(path) == 1)) {
                stop(sprintf("argument %s must be of length 1",
                             sQuote("path")))
            }

            designfile <- file.path(path, designfile)
        }

        ## Further checking deferred until RPPADesignFromParams...
    }

    ## Create new class
    new("RPPADesignParams",
        steps=steps,
        series=series,
        grouping=grouping,
        ordering=ordering,
        alias=alias,
        center=center,
        controls=controls,
        aliasfile=aliasfile,
        designfile=designfile)
}


##-----------------------------------------------------------------------------
## Returns a string representation of this instance. The content and format of
## the returned string may vary between versions. Returned string may be
## empty, but never null.
setMethod("paramString", "RPPADesignParams",
          function(object,
                   slots=slotNames(object),
                   ...) {
    ## Check arguments
    stopifnot(is.character(slots) && length(slots) >= 1)

    ## :TODO: Implementation currently ignores the 'slots' argument
    ## and returns string containing parameters from various slots
    ## as though:
    ##     slotsToDisplay <- c("grouping", "ordering", "center",
    ##                         "controls", "aliasfile", "designfile")
    ##     paramString(dp, slotsToDisplay)
    ##
    controls <- paste(object@controls, collapse=", ")
    paste(paste("grouping:", shQuote(object@grouping)), "\n",
          paste("ordering:", shQuote(object@ordering)), "\n",
          paste("center:", object@center), "\n",
          paste("controls:", shQuote(controls)), "\n",
          paste("aliasfile:", shQuote(object@aliasfile)), "\n",
          paste("designfile:", shQuote(object@designfile)), "\n",
          sep="")
})


##-----------------------------------------------------------------------------
RPPADesignFromParams <- function(raw,
                                 designparams) {
    ## If RPPA object, use its data slot value
    if (is.RPPA(raw)) {
        raw <- raw@data
    }

    ## Check arguments
    if (!(is.data.frame(raw) || is.matrix(raw))) {
        stop(sprintf("argument %s must be matrix or data.frame",
                     sQuote("raw")))
    }

    if (!is.RPPADesignParams(designparams)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("designparams"), "RPPADesignParams"))
    }

    ## :WORKAROUND: codetools (via R CMD check) complains unnecessarily about
    ## "no visible binding" as code below uses assign() rather than "<-".
    steps <- series <- grouping <- ordering <- alias <-
    center <- controls <- aliasfile <- designfile <- NULL

    ## Create variables from 'designparams' slots
    for (slotname in slotNames(designparams)) {
        assign(slotname, slot(designparams, slotname))
    }

    ## Begin processing
    call <- match.call()

    reqdMeasures <- c(.locationColnames(), "Sample")
    raw.df <- data.frame(raw[, reqdMeasures])
    if (all(c(length(steps), length(series)) == 1)) {
        steps <- rep(NA, nrow(raw))
        series <- rep(NA, nrow(raw))
        if (grouping == "byRow") {
            series <- factor(paste("Series",
                                   raw.df$Main.Row,
                                   raw.df$Main.Col,
                                   raw.df$Sub.Row,
                                   sep="."))
            steps <- if (ordering == "increasing") {
                         raw.df$Sub.Col - 1
                     } else {
                         max(raw.df$Sub.Col) - raw.df$Sub.Col
                     }
        } else if (grouping == "byCol") {
            series <- factor(paste("Series",
                                   raw.df$Main.Row,
                                   raw.df$Main.Col,
                                   raw.df$Sub.Col,
                                   sep="."))
            steps <- if (ordering == "increasing") {
                         raw.df$Sub.Row - 1
                     } else {
                         max(raw.df$Sub.Row) - raw.df$Sub.Row
                     }
        } else if (grouping == "bySample") {
            series <- raw.df$Sample
            for (sam in levels(raw.df$Sample)) {
                where <- raw.df$Sample == sam
                n <- sum(where)
                steps[where] <- if (ordering == "increasing") {
                                    -1 + (1:n)
                                } else {
                                    n - (1:n)
                                }
            }
        } else if (grouping == "blockSample") {
            series <- with(raw.df,
                           factor(paste(as.character(Sample),
                                        Main.Row,
                                        Main.Col,
                                        sep=".")))
            for (sam in levels(series)) {
                where <- series == sam
                n <- sum(where)
                steps[where] <- if (ordering == "increasing") {
                                    -1 + (1:n)
                                } else {
                                    n - (1:n)
                                }
            }
        }

        if (center) {
            for (ser in levels(series)) {
                where <- series == ser
                steps[where] <- steps[where] - median(steps[where])
            }
        } else {
            ## Set top intensity (undiluted) spot to zero
            for (ser in levels(series)) {
                where <- series == ser
                steps[where] <- steps[where] - max(steps[where])
            }
        }
    } else if (any(c(length(steps), length(series)) == 1)) {
        stop(sprintf("arguments %s and %s must both be specified if either is",
                     sQuote("steps"),
                     sQuote("series")))
    } else {
        ## Both series and steps supplied
        if (length(steps) != nrow(raw.df) ||
            length(series) != nrow(raw.df)) {
            stop(sprintf("arguments %s (%d) and %s (%d) must be of length %d",
                         sQuote("steps"),
                         length(steps),
                         sQuote("series"),
                         length(series),
                         nrow(raw.df)))
        }
        ## Override sample names from file with user-supplied ones to allow
        ## users to specify controls with reference to their series names
        raw.df$Sample <- series
    }

    if (any(is.na(steps))) {
        warning("some dilution steps have not been specified")
    }

    raw.df$Steps <- steps
    raw.df$Series <- series
    sampleMap <- as.vector(tapply(as.character(raw.df$Sample),
                                  list(series),
                                  function(x) {
                                      x[[1]]
                                  }))
    sampleMap <- tolower(sampleMap)
    names(sampleMap) <- levels(series)

    ## Specify datatype of location columns
    storage.mode(raw.df$Main.Row) <- "integer"
    storage.mode(raw.df$Main.Col) <- "integer"
    storage.mode(raw.df$Sub.Row) <- "integer"
    storage.mode(raw.df$Sub.Col) <- "integer"

    ## Process slide design data, if specified
    if (!is.null(designfile)) {
        tryCatch({
                stopifnot(file.exists(designfile))
                design.df <- read.delim(designfile)
                dim.raw.df <- .dimOfLayout(raw.df)
                dim.design.df <- .dimOfLayout(design.df)
                if (!identical(dim.raw.df, dim.design.df)) {
                    stop(sprintf("dim of argument %s (%s) must match that of slide design (%s)",
                                 sQuote("raw"),
                                 paste(dim.raw.df, collapse="x"),
                                 paste(dim.design.df, collapse="x")))
                }

                ## Attempt to merge columns from slide design file
                raw.df <- .mergeDataWithLayout(raw.df, design.df)

                ## Provide control names from slide design information
                ctrlnames <- as.character(with(raw.df,
                                               Sample[SpotType != "Sample"]))
                controls <- as.list(unique(ctrlnames))
                remove(ctrlnames)
            },
            error=function(cond) {
                stop(sprintf("cannot load slide design data from file %s - %s",
                             dQuote(designfile),
                             conditionMessage(cond)))
            })
    } else if (is.null(controls)) {
        controls <- list()
    }

    ## Provide alias if NULL from any source
    if (is.null(alias)) {
        alias <- if (is.null(aliasfile)) {
                     ## Default case
                     list(Alias=levels(raw$Sample),
                          Sample=levels(factor(tolower(as.character(raw$Sample)))))
                 } else {
                     tryCatch({
                              stopifnot(file.exists(aliasfile))
                              alias.df <- read.delim(aliasfile,
                                                     quote="",
                                                     row.names=NULL)
                              as.list(alias.df)
                         },
                         error=function(cond) {
                             stop(sprintf("cannot load alias data from file %s - %s",
                                          dQuote(aliasfile),
                                          conditionMessage(cond)))
                         })
                 }
    }

    ## Validate alias
    {
        reqdNames <- c("Alias", "Sample")
        if (!(all(reqdNames %in% names(alias)))) {
            missingNames <- reqdNames[!reqdNames %in% names(alias)]
            stop(sprintf(ngettext(length(missingNames),
                                  "slot %s missing component: %s",
                                  "slot %s missing components: %s"),
                         sQuote("alias"),
                         paste(missingNames, collapse=", ")))
        }
    }

    ## Create new class
    new("RPPADesign",
        call=call,
        layout=raw.df,
        alias=alias,
        sampleMap=sampleMap,
        controls=controls)
}


##-----------------------------------------------------------------------------
## Keep for backwards compatibility. Note that code has been refactored for
## maintainability so nothing is done twice.
RPPADesign <- function(raw,
                       steps=rep(0, 1),
                       series=factor(rep(0, 1)),
                       grouping=c("byRow",
                                  "byCol",
                                  "bySample",
                                  "blockSample"),
                       ordering=c("decreasing",
                                  "increasing"),
                       alias=NULL,
                       center=FALSE,
                       controls=NULL,
                       aliasfile=NULL,
                       designfile=NULL,
                       path=".") {
    params <- RPPADesignParams(steps=steps,
                               series=series,
                               grouping=grouping,
                               ordering=ordering,
                               alias=alias,
                               center=center,
                               controls=controls,
                               aliasfile=aliasfile,
                               designfile=designfile,
                               path=path)
    RPPADesignFromParams(raw, params)
}


##-----------------------------------------------------------------------------
setMethod("dim", "RPPADesign",
          function(x) {
    .dimOfLayout(x@layout)
})


##-----------------------------------------------------------------------------
setMethod("summary", "RPPADesign",
          function(object,
                   ...) {
    cat(sprintf("An %s object constructed via the function call:",
                class(object)), "\n")
    ## :TODO: Revisit this when class versioning is in place
    funccall <- if ("call" %in% slotNames(object)) {
                    ## :HACK: Workaround until versioning implemented!
                    ## slotNames() checks class definition of the object, not
                    ## the object itself. As the current implementation of R
                    ## stores slots as attributes, ...
                    if ("call" %in% names(attributes(object))) {
                        as.character(list(object@call))
                    }
                }
    if (is.null(funccall)) {
        funccall <- "unknown"
    }

    cat(" ", funccall, "\n")
    if (length(object@controls) != 0) {
        cat("with controls:", "\n")
        cat(sprintf("  %s\n",
                    unlist(object@controls)), sep="")
    }
    cat("\n")
    print(dim(object))
    cat("\n")
    unneededColnames <- c(.locationColnames(), "Sample")
    summarizable <- !colnames(object@layout) %in% unneededColnames
    print(summary(object@layout[summarizable]))
})


##-----------------------------------------------------------------------------
setMethod("image", signature(x="RPPADesign"),
          function(x,
                   main=.mkPlotTitle("Steps", ""),
                   ...) {
    data.df <- x@layout
    dim.design <- dim(x)
    my <- dim.design["Main.Row"] * dim.design["Sub.Row"]
    mx <- dim.design["Main.Col"] * dim.design["Sub.Col"]
    yspot <- 1+my-(max(data.df$Sub.Row)*(data.df$Main.Row-1) + data.df$Sub.Row)
    xspot <- max(data.df$Sub.Col)*(data.df$Main.Col-1) + data.df$Sub.Col
    geo.steps <- tapply(data.df$Steps,
                        list(xspot, yspot),
                        mean)
    image(seq_len(mx),
          seq_len(my),
          geo.steps,
          main=main,
          sub=paste(paste(class(x), ":", sep=""), paste(dim(x), collapse="x")),
          xaxt="n",
          xlab="",
          yaxt="n",
          ylab="",
          ...)

    at.x <- seq(from=dim.design["Sub.Col"],
                to=dim.design["Sub.Col"]*dim.design["Main.Col"],
                by=dim.design["Sub.Col"])
    at.y <- seq(from=dim.design["Sub.Row"],
                to=dim.design["Sub.Row"]*dim.design["Main.Row"],
                by=dim.design["Sub.Row"])
    axis(1, at=at.x)
    axis(2, at=at.y)

    abline(h=(0.5 + seq(0, my, length=1+dim.design["Main.Row"])))
    abline(v=(0.5 + seq(0, mx, length=1+dim.design["Main.Col"])))
    invisible(geo.steps)
})


##-----------------------------------------------------------------------------
## Plot the series in an RPPA under a given design layout to see if the series
## makes sense under this layout.
## :TBD: Is this signature backwards?
setMethod("plot", signature(x="RPPA", y="RPPADesign"),
          function(x,
                   y,
                   measure="Mean.Net",
                   main="",
                   ...) {
    ## Check arguments
    if (!is.character(measure)) {
        stop(sprintf("argument %s must be character",
                     sQuote("measure")))
    } else if (!(length(measure) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("measure")))
    } else if (!(measure %in% colnames(x@data))) {
        stop(sprintf("invalid measure %s",
                     sQuote(measure)))
    }

    if (!is.character(main)) {
        stop(sprintf("argument %s must be character",
                     sQuote("main")))
    } else if (!(length(main) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("main")))
    }

    ## Begin processing
    vert <- x@data[, measure]
    horz <- y@layout$Steps

    if (!nzchar(main)) {
        main <- .mkPlotTitle(paste(measure, "Intensity vs. Dilution Step"),
                             x@antibody)
    }

    is.ctrl <- .controlVector(y)  # Get the indexes of the control spots
    par(mfrow=c(1, 1))  # Avoid existing partitions of graphic device
    plot(c(min(horz[!is.ctrl]), max(horz[!is.ctrl])),
         c(min(vert), max(vert)),
         main=main,
         sub=paste("File:", x@file),
         type="n",
         xlab="Dilution Step",
         ylab="Intensity")
    series <- y@layout$Series
    s <- seriesNames(y) # Strip out control spots
    bow <- rainbow(length(s))
    for (i in seq_along(s)) {
        lines(x=horz[series == s[i]],
              y=vert[series == s[i]],
              col=bow[i],
              type="b")
    }
})


##-----------------------------------------------------------------------------
.controlVector <- function(design) {
    ## Check arguments
    if (!is.RPPADesign(design)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("design"), "RPPADesign"))
    }

    ## Begin processing
    sample <- as.character(design@layout$Sample)
    temp <- rep(FALSE, length(unique(sample)))
    names(temp) <- unique(sample)
    temp[unlist(design@controls)] <- TRUE
    temp[sample]
}


##-----------------------------------------------------------------------------
seriesNames <- function(design) {
    ## Check arguments
    if (!is.RPPADesign(design)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("design"), "RPPADesign"))
    }

    ## Begin processing
    isControl <- .controlVector(design)
    series <- as.character(design@layout$Series[!isControl])
    unique(series)
}


##-----------------------------------------------------------------------------
getSteps <- function(design) {
    ## Check arguments
    if (!is.RPPADesign(design)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("design"), "RPPADesign"))
    }

    ## Begin processing
    isControl <- .controlVector(design)
    design@layout$Steps[!isControl]
}


##-----------------------------------------------------------------------------
setMethod("names", "RPPADesign",
          function(x) {
    isControl <- .controlVector(x)
    as.character(x@layout$Series[!isControl])
})

