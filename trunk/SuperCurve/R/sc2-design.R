###
### DESIGN.R
###


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

    grouping <- match.arg(grouping)
    ordering <- match.arg(ordering)

    if (!(is.list(alias) || is.data.frame(alias))) {
        stop(sprintf("argument %s must be list or data.frame",
                     sQuote("alias")))
    } else if (!(length(alias) == 0)) {
        reqdNames <- c("Alias", "Sample")
        if (!(length(alias) >= length(reqdNames))) {
            stop(sprintf("argument %s must be of length %d or greater",
                         sQuote("alias"), length(reqdNames)))
        } else if (!(all(reqdNames %in% names(alias)))) {
            missingNames <- reqdNames[!reqdNames %in% names(alias)]
            stop(sprintf(ngettext(length(missingNames),
                                  "argument %s missing element: %s",
                                  "argument %s missing elements: %s"),
                         sQuote("alias"), paste(missingNames, collapse=', ')))
        }
    }

    if (!is.logical(center)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("center")))
    } else if (!(length(center) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("center")))
    }

    if (!is.list(controls)) {
        stop(sprintf("argument %s must be list",
                     sQuote("controls")))
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
RPPADesignFromParams <- function(raw, designparams) {
    ## Check arguments
    if (!inherits(designparams, "RPPADesignParams")) {
        stop(sprintf("argument %s must be RPPADesignParams object",
                     sQuote("designparams")))
    }

    ## Begin processing
    RPPADesign(raw,
               designparams@steps,
               designparams@series,
               designparams@grouping,
               designparams@ordering,
               designparams@alias,
               designparams@center,
               designparams@controls)
}


##-----------------------------------------------------------------------------
# This is bad to have two constructors here since now this has to be kept in
# sync with PRRADesignParams
# Only keep for backwards compatibility atm
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
    ## If RPPA object, use its data slot value
    if (inherits(raw, "RPPA")) {
        raw <- raw@data
    }

    ## Check arguments
    if (!(is.data.frame(raw) || is.matrix(raw))) {
        stop(sprintf("argument %s must be matrix or data.frame",
                     sQuote("raw")))
    }

    if (!is.numeric(steps)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("steps")))
    }

    if (!(is.character(series) || is.factor(series))) {
        stop(sprintf("argument %s must be character or factor",
                     sQuote("series")))
    }

    ## :TBD: grouping, ordering

    if (!(is.list(alias) || is.data.frame(alias))) {
        stop(sprintf("argument %s must be list or data.frame",
                     sQuote("alias")))
    }

    if (length(alias) < 1) {
        alias <- list(Alias=levels(raw$Sample),
                      Sample=levels(factor(tolower(as.character(raw$Sample)))))
    }

    if (!is.logical(center)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("center")))
    } else if (!(length(center) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("center")))
    }

    if (!is.list(controls)) {
        stop(sprintf("argument %s must be list",
                     sQuote("controls")))
    }

    ## Begin processing
    raw.df <- data.frame(raw[, c("Main.Row",
                                 "Main.Col",
                                 "Sub.Row",
                                 "Sub.Col",
                                 "Sample")])
    if (length(steps) < 2 && length(series) < 2) {
        grouping <- match.arg(grouping)
        ordering <- match.arg(ordering)
        steps <- rep(NA, nrow(raw))
        series <- rep(NA, nrow(raw))
        if (grouping == "byRow") {
            series <- factor(paste("Series",
                                   raw.df$Main.Row,
                                   raw.df$Main.Col,
                                   raw.df$Sub.Row,
                                   sep='.'))
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
                                   sep='.'))
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
            attach(raw.df)
            series <- factor(paste(as.character(Sample),
                                   Main.Row,
                                   Main.Col,
                                   sep='.'))
            detach()
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
            # set top intensity (undiluted) spot to zero
            for (ser in levels(series)) {
                where <- series == ser
                steps[where] <- steps[where] - max(steps[where])
            }
        }

    } else if (length(steps) < 2 || length(series) < 2) {
        stop("You must supply both 'steps' and 'series' if you supply either one")
    } else {
        # both series and steps supplied
        if (length(steps) != nrow(raw.df) || length(series) != nrow(raw.df)) {
            stop("lengths do not match")
        }
        # override sample names from file, with user supplied series names
        raw.df$Sample <- series
        # it is important to override so that users can specify
        # controls with reference to their series names
    }

    if (any(is.na(steps))) {
        warning("Some dilution steps have not been specified")
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

    ## Create new class
    new("RPPADesign",
        layout=raw.df,
        alias=alias,
        sampleMap=sampleMap,
        controls=controls)
}


##-----------------------------------------------------------------------------
# plot the series in an RPPA under a given design layout
# see if the series make sense under this layout
plotDesign.org <- function(rppa,
                           design,
                           measure='Mean.Total',
                           main='') {
    y <- rppa@data[, measure]
    x <- design@layout$Steps
    plot(c(min(x), max(x)),
         c(min(y), max(y)),
         type='n',
         main=paste(measure, "Intensity vs. Dilution Step", main),
         xlab='Dilution Step',
         ylab='Intensity')
    series <- design@layout$Series
    s <- seriesNames(design) # strip out control spots
    rows <- length(s)
    bow <- rainbow(rows)
    for (i in seq(1, rows)) {
        lines(x=x[series == s[i]],
              y=y[series == s[i]],
              col=bow[i],
              type='b')
    }
}


##-----------------------------------------------------------------------------
plotDesign <- function(rppa,
                       design,
                       measure='Mean.Total',
                       main='') {
    ## Check arguments
    if (!inherits(rppa, "RPPA")) {
        stop(sprintf("argument %s must be RPPA object",
                     sQuote("rppa")))
    }

    if (!inherits(design, "RPPADesign")) {
        stop(sprintf("argument %s must be RPPADesign object",
                     sQuote("design")))
    }

    if (!is.character(measure)) {
        stop(sprintf("argument %s must be character",
                     sQuote("measure")))
    } else if (!(length(measure) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("measure")))
    }

    if (!is.character(main)) {
        stop(sprintf("argument %s must be character",
                     sQuote("main")))
    } else if (!(length(main) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("main")))
    }

    ## Begin processing
    y <- rppa@data[, measure]
    x <- design@layout$Steps

    ########
    ##The following were modified by Wenbin Liu:
    # Sometimes there are many (e.g., hundreds of) appearances of 'control'
    # in the Sample column and the original max(x) will mess up the plot.
    #######

    is.ctrl <- .controlVector(design)  # get the indexes of the control spots
    par(mfrow=c(1, 1)) # avoid existing partitions of graphic device.
    plot(c(min(x[!is.ctrl]), max(x[!is.ctrl])),
         c(min(y), max(y)),
         type='n',
         main=paste(measure, "Intensity vs. Dilution Step", main),
         xlab='Dilution Step',
         ylab='Intensity')
    series <- design@layout$Series
    s <- seriesNames(design) # strip out control spots
    rows <- length(s)
    bow <- rainbow(rows)
    for (i in seq(1, rows)) {
        lines(x=x[series == s[i]],
              y=y[series == s[i]],
              col=bow[i],
              type='b')
    }
}


##-----------------------------------------------------------------------------
.controlVector <- function(design) {
    ## Check arguments
    if (!inherits(design, "RPPADesign")) {
        stop(sprintf("argument %s must be RPPADesign object",
                     sQuote("design")))
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
    if (!inherits(design, "RPPADesign")) {
        stop(sprintf("argument %s must be RPPADesign object",
                     sQuote("design")))
    }

    ## Begin processing
    isControl <- .controlVector(design)
    series <- as.character(design@layout$Series[!isControl])
    unique(series)
}


##-----------------------------------------------------------------------------
getSteps <- function(design) {
    ## Check arguments
    if (!inherits(design, "RPPADesign")) {
        stop(sprintf("argument %s must be RPPADesign object",
                     sQuote("design")))
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
    .attachslot <- function(x) {
        xname <- substitute(x)
        sl <- names(getSlots(class(x)))
        slotnames <- paste(sl, " <<- ", xname, "@", sl, sep="")
        for (i in slotnames) {
            eval(parse(text=substitute(slotnames,
                                       list(slotnames=slotnames))))
        }
    }
}


if (FALSE) {
  path <- "../inst/rppaTumorData"
  erk2 <- RPPA("ERK2.txt", path=path)
  design <- RPPADesign(erk2, grouping="blockSample", center=TRUE)
  image(design)
  summary(design)
  design <- RPPADesign(erk2, grouping="blockSample",
                       controls=list("neg con", "pos con"))
  image(design)
  summary(design)
  rm(path, erk2, design)
}

