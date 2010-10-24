###
### PREFITQC.R
###


##=============================================================================
setClass("RPPAPreFitQC",
         representation("VIRTUAL"))


##=============================================================================
setClass("DS5RPPAPreFitQC",
         contains="RPPAPreFitQC",
         representation(slopediff="numeric",
                        cvs="numeric",
                        drdiffs="numeric",
                        slopes="numeric",
                        skews="numeric",
                        percentgood="numeric",
                        adjusted="logical"),
         prototype(adjusted=FALSE))


##-----------------------------------------------------------------------------
## Returns TRUE if class of argument is subclass of RPPAPreFitQC.
is.RPPAPreFitQC <- function(x) {
    extends(x, "RPPAPreFitQC")
}


##-----------------------------------------------------------------------------
## Generates subclass instance of an RPPAPreFitQC object (factory-style)
RPPAPreFitQC <- function(rppa,
                         design,
                         useAdjusted=TRUE) {
    ## Check arguments
    if (!is.RPPA(rppa)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("rppa"), "RPPA"))
    } else {
        measures <- c("Mean.Net", "Mean.Total")
        if (as.logical(useAdjusted)) {
            measures <- paste("Adj", measures, sep=".")
        }
        reqdNames <- measures
        if (!(all(reqdNames %in% colnames(rppa@data)))) {
            missingNames <- reqdNames[!reqdNames %in% colnames(rppa@data)]
            stop(sprintf(ngettext(length(missingNames),
                                  "argument %s missing required column: %s",
                                  "argument %s missing required columns: %s"),
                         sQuote("rppa"),
                         paste(missingNames, collapse=", ")))
        }
    }

    if (!is.RPPADesign(design)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("design"), "RPPADesign"))
    } else {
        ## Requires SlideDesignerGUI-provided columns
        reqdNames <- c("SpotType", "Dilution")
        if (!(all(reqdNames %in% colnames(design@layout)))) {
            missingNames <- reqdNames[!reqdNames %in% colnames(design@layout)]
            stop(sprintf(ngettext(length(missingNames),
                                  "argument %s missing required column: %s",
                                  "argument %s missing required columns: %s"),
                         sQuote("design"), paste(missingNames, collapse=", ")))
        }

        if (!(any(design@layout$SpotType == "PosCtrl"))) {
            stop("design contains no positive controls")
        }
    }

    if (!identical(dim.rppa <- dim(rppa), dim.design <- dim(design))) {
        stop(sprintf("dim of argument %s (%s) must match that of argument %s (%s
)",
                     sQuote("rppa"),
                     paste(dim.rppa, collapse="x"),
                     sQuote("design"),
                     paste(dim.design, collapse="x")))
    }

    ## Begin processing
    ndilutions <- with(subset(design@layout,
                              SpotType == "PosCtrl"),
                       length(unique(Dilution)))
    switch(EXPR=as.character(ndilutions),
           "5"=DS5RPPAPreFitQC(rppa, design, measures, useAdjusted),
           stop(sprintf("unsupported slide layout - ndilutions = %d",
                        ndilutions)))
}


##-----------------------------------------------------------------------------
## Generates a DS5RPPAPreFitQC object
DS5RPPAPreFitQC <- function(rppa,
                            design,
                            measures,
                            useAdjusted=TRUE) {
    ## Check arguments
    stopifnot(is.RPPA(rppa))
    stopifnot(is.RPPADesign(design))
    stopifnot(is.character(measures) && length(measures) == 2)
    stopifnot(is.character(useAdjusted) && length(useAdjusted) == 1)


    ##-------------------------------------------------------------------------
    ## Calculate coefficient of variance
    CV <- function(thedata) {
        stopifnot(is.numeric(thedata))

        log.measure <- log(thedata)
        sd(log.measure, na.rm=TRUE) / mean(log.measure, na.rm=TRUE)
    }


    ##-------------------------------------------------------------------------
    percentGood <- function(df) {
        stopifnot(is.data.frame(df))

        mean.total <- df[[measure.total]]
        mean.net <- df[[measure.net]]
        bsr <- (mean.total - mean.net) / mean.net
        100 * (sum(bsr < 1) / length(bsr))
    }


    ##-------------------------------------------------------------------------
    slopeDiff <- function(df) {
        stopifnot(is.data.frame(df))

        ## :TODO: Modify this to use slide design over physical layout
        steps <- rep(c(4:0, 0:4), times=12*4)
        mean.net <- df[[measure.net]]
        model <- lm(log(mean.net) ~ steps)
        slope <- coef(model)["steps"]
        ## See how far the observed slopes are from that of perfect dilution.
        perfect.slope <- 0.6931
        abs(slope - perfect.slope)
    }


    ## Begin processing
    if (!require(timeDate)) {
        stop(sprintf("%s package required for pre-fit QC in the %s method",
                     sQuote("timeDate"),
                     sQuote("DS5RPPAPreFitQC")))
    }

    mydata <- .mergeDataWithLayout(rppa, design)

    x.samples <- with(mydata, SpotType == "Sample")
    x.posctrl <- with(mydata, SpotType == "PosCtrl")

    positives.df <- mydata[x.posctrl, ]
    samples.df <- mydata[x.samples, ]
    rownames(samples.df) <- 1:nrow(samples.df)

    dilutions <- sort(unique(positives.df$Dilution), decreasing=TRUE)
    ndilutions <- length(dilutions)  # number of dilution series per slide
    ## :NOTE: Code written for superslide data
    stopifnot(ndilutions == 5)
    strengths <- sapply(seq_len(ndilutions), function(n) 2^(n-1))

    ## Set measure names to use
    measure.net   <- measures[1]
    measure.total <- measures[2]

    ## Make list containing 'Sub.Row' value pairs for each PC dilution
    plocats <- lapply(dilutions,
                      function(dilution, layout.df) {
                          pc.tf <- with(layout.df,
                                        (SpotType == "PosCtrl" &
                                         Dilution == dilution))
                          unique(layout.df[pc.tf, "Sub.Row"])
                      },
                      layout.df=mydata)
    names(plocats) <- dilutions

    positive.locats <- lapply(plocats,
                              function(locat, pos.df) {
                                  locat.tf <- with(pos.df, Sub.Row %in% locat)
                                  pos.df[locat.tf, measure.net]
                              },
                              pos.df=positives.df[, c("Sub.Row", measure.net)])

    ## For each dilution step of positive controls, ...
    slopes <- NULL
    skews <- NULL
    cvs <- NULL
    for (positives in positive.locats) {
        ## Calculate slope for measure (good if near zero)
        y <- positives
        x <- seq_along(y)    # seq_len(nrows(positives.df)/ndilutions) = 1:96
        model <- lm(y~x)
        slope <- abs(coef(model)["x"])
        slopes <- c(slopes, slope)

        ## Calculate skewness for measure (good if near zero)
        ## If distribution is symmetric, then
        ## mean = median and there is zero skewness.
        skew <- if (all(positives >= 0.0)) {
                    abs(skewness(log(positives)))
                } else {
                    NaN  # Set (as would have been done) w/o warning
                }
        skews <- c(skews, skew)

        ## Calculate coefficient of variances for measure
        cv <- CV(positives)
        cvs <- c(cvs, cv)
    }
    names(cvs) <- strengths

    ## Calculate slope of step series for measure of positive controls
    slopediff <- slopeDiff(positives.df)

    ## Calculate dynamic range difference for measure of each dilution step
    ## of samples
    drdiffs <- NULL
    for (dilution in dilutions) {
        samp.tf <- with(samples.df,
                        (SpotType == "Sample" &
                         Dilution == dilution))
        samples.mean.net <- samples.df[samp.tf, measure.net]
        drdiff <- diff(range(samples.mean.net))
        drdiffs <- c(drdiffs, drdiff)
    }
    names(drdiffs) <- strengths

    ## Calculate percent good
    percentgood <- percentGood(samples.df)

    ## Create new class
    new("DS5RPPAPreFitQC",
        adjusted=useAdjusted,
        slopediff=slopediff,
        cvs=cvs,
        drdiffs=drdiffs,
        slopes=slopes,
        skews=skews,
        percentgood=percentgood)
}


##-----------------------------------------------------------------------------
setMethod("qcprob", "RPPAPreFitQC",
          function(object,
                   ...) {
    stop(sprintf("%s method must be implemented by any subclass of %s",
                 sQuote("qcprob"),
                 sQuote("RPPAPreFitQC")))
})


##-----------------------------------------------------------------------------
setMethod("qcprob", "DS5RPPAPreFitQC",
          function(object,
                   ...) {
    ##-------------------------------------------------------------------------
    pred.model <- function(x) {
        slopediff   <- x@slopediff
        cv1         <- x@cvs[1]
        cv2         <- x@cvs[2]
        cv8         <- x@cvs[4]
        step4slope  <- x@slopes[3]
        step4skew   <- x@skews[3]
        drdiff2     <- x@drdiffs[2]
        drdiff8     <- x@drdiffs[4]
        percentgood <- x@percentgood

        z <- 3.01 -
             (1.228      * slopediff) -
             (21.43      * cv1) -
             (35.46      * cv2) -
             (27.95      * cv8) -
             (0.01053    * step4slope) +
             (0.08624    * step4skew) +
             (0.0000462  * drdiff2) -
             (0.00004722 * drdiff8) +
             (0.01173    * percentgood)
        as.numeric(z)  ## Strip attributes
    }


    ## Begin processing
    z <- pred.model(object)
    exp(z) / (1 + exp(z)) # probability of good slide
})


##-----------------------------------------------------------------------------
setMethod("summary", "DS5RPPAPreFitQC",
          function(object,
                   ...) {
    cat(sprintf("An %s object", class(object)), "\n")
    cat("Measures spatially adjusted:", object@adjusted, "\n")
    cat("Difference from perfect slope:", object@slopediff, "\n")
    cat("Coefficients of variances:", "\n")
    cat("\t", paste(object@cvs, collapse=", "), "\n")
    cat("Difference in ranges:", "\n")
    cat("\t", paste(round(object@drdiffs, 2), collapse=", "), "\n")
    cat("Step slopes:", "\n")
    cat("\t", paste(round(object@slopes, 5), collapse=", "), "\n")
    cat("Step skews:", "\n")
    cat("\t", paste(object@skews, collapse=", "), "\n")
    cat("Percent good:", object@percentgood, "\n")
})

