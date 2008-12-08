###
### RPPADESIGN.R
###


##=============================================================================
setClass("RPPADesign",
         representation=list(call="call",
                             layout="data.frame",
                             alias="list",
                             sampleMap="character",
                             controls="list"))


##=============================================================================
setClass("RPPADesignParams",
         representation=list(steps="numeric",
                             series="factor",
                             grouping="character",
                             ordering="character",
                             alias="list",
                             center="logical",
                             controls="list"))


##-----------------------------------------------------------------------------
is.RPPADesign <- function(x) {
    inherits(x, "RPPADesign")
}


##-----------------------------------------------------------------------------
is.RPPADesignParams <- function(x) {
    inherits(x, "RPPADesignParams")
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
                             alias=list(),
                             center=FALSE,
                             controls=list()) {
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

        ## Further checking deferred until paired with data.frame...
    } else if (any(c(length(steps), length(series)) == 1)) {
        stop(sprintf("arguments %s and %s must both be specified if either is",
                     sQuote("steps"),
                     sQuote("series")))
    } else {
        ## Unnecessary when steps and series are specified
        grouping <- as.character(NA)
        ordering <- as.character(NA)
        center <- as.logical(NA)

       ## Further checking deferred until paired with data.frame...
    }

    if (!(is.list(alias) || is.data.frame(alias))) {
        stop(sprintf("argument %s must be list or data.frame",
                     sQuote("alias")))
    }

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

    if (!(is.list(controls) || is.character(controls))) {
        stop(sprintf("argument %s must be character or list",
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


    ## Create new class
    new("RPPADesignParams",
        steps=steps,
        series=series,
        grouping=grouping,
        ordering=ordering,
        alias=alias,
        center=center,
        controls=controls)
}


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

    steps    <- designparams@steps
    series   <- designparams@series
    grouping <- designparams@grouping
    ordering <- designparams@ordering
    alias    <- designparams@alias
    center   <- designparams@center
    controls <- designparams@controls

    if (length(alias) < 1) {
        alias <- list(Alias=levels(raw$Sample),
                      Sample=levels(factor(tolower(as.character(raw$Sample)))))
    } else {
        reqdNames <- c("Alias", "Sample")
        if (!(length(alias) >= length(reqdNames))) {
            stop(sprintf("argument %s must be of length %d or greater",
                         sQuote("alias"), length(reqdNames)))
        } else if (!(all(reqdNames %in% names(alias)))) {
            missingNames <- reqdNames[!reqdNames %in% names(alias)]
            stop(sprintf(ngettext(length(missingNames),
                                  "argument %s missing component: %s",
                                  "argument %s missing components: %s"),
                         sQuote("alias"), paste(missingNames, collapse=", ")))
        }
    }

    ## Begin processing
    call <- match.call()

    raw.df <- data.frame(raw[, c("Main.Row",
                                 "Main.Col",
                                 "Sub.Row",
                                 "Sub.Col",
                                 "Sample")])
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

    ## If controls contains a pathname, attempt to merge the columns
    ## from slide design file and determine control names automatically.
    if (length(controls) == 1) {
        slidedesignPathname <- controls[[1]]
        if (file.exists(slidedesignPathname)) {
            tryCatch({
                    slidedesign.df <- read.delim(slidedesignPathname)
                    dim.raw.df <- .dimOfLayout(raw.df)
                    dim.slidedesign.df <- .dimOfLayout(slidedesign.df)
                    if (!identical(dim.raw.df, dim.slidedesign.df)) {
                        stop(sprintf("dim of argument %s (%s) must match that of slide design (%s)",
                                     sQuote("raw"),
                                     paste(dim.raw.df, collapse="x"),
                                     paste(dim.slidedesign.df, collapse="x")))
                    }
                    raw.df <- merge(raw.df, slidedesign.df)
                    ctrlnames <- as.character(with(raw.df,
                                                   Sample[SpotType != "Sample"]))
                    controls <- as.list(unique(ctrlnames))
                    rm(ctrlnames)
                },
                error=function(e) {
                    stop(sprintf("cannot load slide design data from file %s - %s",
                                 dQuote(slidedesignPathname),
                                 e$message))
                })
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
                       alias=list(),
                       center=FALSE,
                       controls=list()) {
    params <- RPPADesignParams(steps,
                               series,
                               grouping,
                               ordering,
                               alias,
                               center,
                               controls)
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
                    unlist(object@controls), sep=""))
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
                   ...) {
    data.df <- x@layout
    my <- max(data.df$Main.Row) * max(data.df$Sub.Row)
    mx <- max(data.df$Main.Col) * max(data.df$Sub.Col)
    yspot <- 1+my-(max(data.df$Sub.Row)*(data.df$Main.Row-1) + data.df$Sub.Row)
    xspot <- max(data.df$Sub.Col)*(data.df$Main.Col-1) + data.df$Sub.Col
    geo.steps <- tapply(data.df$Steps,
                        list(xspot, yspot),
                        mean)
    image(seq_len(mx),
          seq_len(my),
          geo.steps,
          ...)
    abline(h=(0.5 + seq(0, my, length=1+max(data.df$Main.Row))))
    abline(v=(0.5 + seq(0, mx, length=1+max(data.df$Main.Col))))
    invisible(geo.steps)
})


##-----------------------------------------------------------------------------
## Plot the series in an RPPA under a given design layout to see if the series
## makes sense under this layout.
## :TBD: Is this signature backwards?
setMethod("plot", signature(x="RPPA", y="RPPADesign"),
          function(x,
                   y,
                   measure="Mean.Total",
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

    is.ctrl <- .controlVector(y)  # Get the indexes of the control spots
    par(mfrow=c(1, 1))  # Avoid existing partitions of graphic device
    plot(c(min(horz[!is.ctrl]), max(horz[!is.ctrl])),
         c(min(vert), max(vert)),
         main=paste(measure, "Intensity vs. Dilution Step", "-", main),
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


if (FALSE) {
  source("AllGenerics.R")
  source("sc1-rppa.R")
  source("sc2-design.R")
  path <- "../inst/rppaTumorData"
  erk2 <- RPPA("ERK2.txt", path=path)
  design <- RPPADesign(erk2,
                       grouping="blockSample",
                       center=TRUE)
  image(design)
  summary(design)
  design <- RPPADesign(erk2,
                       grouping="blockSample",
                       controls=list("neg con", "pos con"))
  image(design)
  summary(design)
  rm(path, erk2, design)

  plot(erk2, design)
}

