###
### DILUTIONFIT.R
###

##############################################################
# This is the result of the model building in doSimulation.R
# Original version by Kevin R. Coombes and Jianhua Hu.
# Modified by Corwin Joy.
# Modified by: Kevin R. Coombes, 31 March 2006
# Modified by: Corwin Joy, 31 March 2006 to add more robust estimate for
#              alpha, beta & more formal regression tests.
# Last modified by: Kevin Coombes, 3 May 2006
#   Converted to S4 classes. Completely revised interface, giving more
#   sensible names to inputs and outputs. Added numerous useful methods
#   for assessing the quality of the fit.


##################################################################
# First pass estimator for concentrations

##-----------------------------------------------------------------------------
# compute a multiple of the logit transform.
# in practice, epsilon never changes.
.calcLogitz <- function(data, alpha, beta, gamma, epsilon=0.001) {
    z <- (data - alpha) / beta
    # Truncate points at min and max for dilution curve
    z[z < epsilon] <- epsilon
    z[z > (1-epsilon)] <- 1-epsilon
    log(z / (1-z)) / gamma
}


##-----------------------------------------------------------------------------
.firstPass <- function(yval, design, ignoreNegative, epsilon=1e-4) {
    # Computes crude estimates of the parameters (i.e., alpha, beta,
    # gamma, and the EC50s) so we can initialize the routine.

    # 'yval' is a vector of intensity values selected from the MicroVigene file
    # 'design' is a RPPADesign object. The length of 'yval' should match the
    # number of rows in design@layout, and be in the same order
    # in practice, 'epsilon' never changes

    ## Check arguments
    if (!is.numeric(yval)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("yval")))
    }

    if (!inherits(design, "RPPADesign")) {
        stop(sprintf("argument %s must be RPPADesign object",
                     sQuote("design")))
    }

    if (!is.logical(ignoreNegative)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("ignoreNegative")))
    } else if (!(length(ignoreNegative) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("ignoreNegative")))
    }

    if (!is.numeric(epsilon)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("epsilon")))
    } else if (!(length(epsilon) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("epsilon")))
    }

    ## Begin processing
    if (ignoreNegative) {
        yval[yval < 0] <- NA
    }
    temp <- yval[!.controlVector(design)]
    # compute a robust estimate for alpha and beta
    # Tried to use robust 'rnls' from 'sfsmisc' to fit alpha and beta below
    # but found it was still too sensitive to outliers.
    # This is a crude way to get alpha and beta but it seems to be robust.

    # XXXXXXXXXXXXXXXXXXXXXXXXXXX
    # lBot <- quantile(temp, probs=c(.05), na.rm=TRUE)
    # lTop <- quantile(temp, probs=c(.95), na.rm=TRUE)

    # HERE IS THE PROBLEM!
    # Quantiles forces a double trim in conjunction with calcLogitz
    # and epsilon above, artificially shrinking alpha and beta
    # Try to match 0.12
    lBot <- min(temp, na.rm=TRUE)
    lTop <- max(temp, na.rm=TRUE)


    lindata <- .calcLogitz(yval, lBot, lTop-lBot, 1)

    series <- seriesNames(design) # omits the control spots automagically

    .estimateLogisticSlope <- function(ser, dsn, ld) {
        layout <- dsn@layout
        items <- layout$Series == ser   # get the items in this diluton series
        steps <- layout$Steps[items]    # get the log2 steps for this series
        x <- ld[items]                  # get the transformed intensity values
        y <- x[!is.na(x) & !is.infinite(x)] # only use valid values
        (max(y) - min(y))/(max(steps) - min(steps)) # ychange / xchange
    }

    # initial estimate of the logistic slope across the dilution steps
    # there should be a more comprehensible way to write this:
    dat <- unlist(lapply(series, .estimateLogisticSlope, design, lindata))

    # force a nonzero slope for starting estimate, and
    # filter out zero slopes from blank sample
    minslope <- 0.001
    dat <- dat[dat > minslope]

    # use mean, not median. We may have many blank samples
    # on a slide in which case median slope will be 0
    gamma <- mean(dat, trim=0.01)
    gamma <- max(gamma, minslope) # force a nonzero slope for starting estimate

    # for each sample, estimate the offset
    passer <- rep(NA, length(series))
    names(passer) <- series
    for (this in series) {
        layout <- design@layout
        items <- layout$Series == this
        passer[this] <- median(lindata[items] / gamma - layout$Steps[items],
                               na.rm=TRUE)
    }

    list(lBot=lBot,
         lTop=lTop,
         gamma=gamma,
         passer=passer)
}


##############################################################
# RPPAFit


##-----------------------------------------------------------------------------
# We are actually interested in estimating the concentrations for each
# dilution series (which may be the same as a sample). However, when we do
# that we also get estimated concentrations at each spot, with corresponding
# predicted intensities. Default for 'fitted' is to return the per-spot
# fitted 'Y' intensities, with an option to return the per-spot fitted
# 'X' concentrations.
setMethod("fitted", "RPPAFit",
          function(object,
                   type=c("Y", "y", "X", "x"),
                   ...) {
    ## Check arguments
    type <- match.arg(type)

    ## Begin processing
    conc <- object@concentrations
    series <- as.character(object@design@layout$Series)
    fitX <- conc[series] + object@design@layout$Steps
    if (type == "x" || type == "X") {
        return(fitX)
    }

    fitted(object@model, fitX)
})


##-----------------------------------------------------------------------------
# raw residuals are defined so that
#    observed intensity = fitted Y + raw residuals
# standardized residuals are formed from the raw residuals in the obvious way
# linear residuals are the residuals after the logistic transformation
# r2 residuals are expressed as a kind of R^2 number.  specifically, if the entire fit had
# this residual value at each yi, show what would the R^2 for the fit would be. This allows us
# to express residuals on a uniform scale across multiple slides and quickly spot "bad" residuals
# on a uniform scale.
#
# Note that the model fitting is a bit of a hybrid between the linear and
# logistic intensity scales, so it's not completely clear which residuals are
# most meaningful
setMethod("residuals", "RPPAFit",
          function(object,
                   type=c("raw", "standardized", "r2"),
                   ...) {
    ## Check arguments
    type <- match.arg(type)

    ## Begin processing
    res <- object@rppa@data[, object@measure] - fitted(object)

    if (type == "standardized") {
        res <- scale(res)
    }
    if (type == "r2") {
        y    <- object@rppa@data[, object@measure]
        nobs <- length(y)
        sigmasq <- var(y) * (nobs-1)
        res  <- 1 - (nobs * res * res / sigmasq)
    }

    res
})


setMethod("resid", "RPPAFit",
          function(object, ...) {
    residuals(object, ...)
})


##-----------------------------------------------------------------------------
# Histogram of the residuals, with an option to see the standardized or linear residuals
setMethod("hist", "RPPAFit",
          function(x,
                   type=c("Residuals", "StdRes", "ResidualsR2"),
                   xlab=NULL,
                   main=NULL,
                   ...) {
    type <- match.arg(type)
    if (is.null(xlab)) {
        xlab <- type
    }
    if (is.null(main)) {
        main <- paste('Histogram of', type)
    }
    translate <- c("raw", "standardized", "r2")
    names(translate) <- c("Residuals", "StdRes", "ResidualsR2")
    res <- resid(x, type=translate[type])
    hist(res, xlab=xlab, main=main, ...)
})


##-----------------------------------------------------------------------------
.loess.line <- function(xval,
                        yval,
                        col="red",
                        span=(2 / 3),
                        xform=function(x) x) {
    aux <- loess(yval ~ xval, degree=1, span=span, family="gaussian")$fitted
    o <- order(xval)
    A <- xval[o]
    M <- xform(aux[o])
    o <- which(!duplicated(A))
    lines(approx(A[o], M[o]), col=col)
}


##-----------------------------------------------------------------------------
# Plot the results from dilutionFit above to see how well supercurve fits
setMethod("plot", "RPPAFit",
          function(x, y,
                   type=c("cloud", "series", "individual", "steps", "resid"),
                   xlab='Log Concentration',
                   ylab='Intensity',
                   colors=NULL,
                   xform=function(x) { x },
                   ...) {
    ## Check arguments
    type <- match.arg(type)

    if (!is.character(xlab)) {
        stop(sprintf("argument %s must be character",
                     sQuote("xlab")))
    } else if (!(length(xlab) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("xlab")))
    }

    if (!is.character(ylab)) {
        stop(sprintf("argument %s must be character",
                     sQuote("ylab")))
    } else if (!(length(ylab) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("ylab")))
    }

    ## :TODO: colors

    if (!is.function(xform)) {
        stop(sprintf("argument %s must be function",
                     sQuote("xform")))
    }

    ## Begin processing
    trimset <- as.list(x@trimset)
    max.step <- max(getSteps(x@design))
    min.step <- min(getSteps(x@design))

    xval <- fitted(x, 'x')
    yval <- fitted(x, 'y')
    yraw <- xform(x@rppa@data[, x@measure])
    series <- seriesNames(x@design)
    steps <- getSteps(x@design)
    model.color <- "green"
    if (type == "cloud" || type == "series") {
        min.conc <- trimset$lo.conc
        max.conc <- trimset$hi.conc
        if (!hasArg(sub)) {
            autosub <- paste('(Conc > -5) Trimmed Mean R^2 =',
                             format(mean(x@ss.ratio[x@concentrations > -5],
                                         trim=0.1),
                                    digits=3),
                             ', Min / Max Valid Conc. =',
                             round(min.conc, 2),
                             '/',
                             round(max.conc, 2))
            plot(xval, yraw, xlab="", ylab=ylab, sub=autosub, ...)
        } else {
            plot(xval, yraw, xlab=xlab, ylab=ylab, ...)
        }

        lines(sort(xval), sort(yval), col=model.color)
        abline(h=trimset$lo.intensity)
        abline(h=trimset$hi.intensity)

        if (type == "series") {
            if (is.null(colors)) {
                colors <- hcl(seq(0, 360, length=9)[1:8], l=65, c=65)
            }
            i <- 0
            ncol <- length(colors)
            for (this in series) {
                i <- (i %% ncol) + 1
                items <- x@design@layout$Series == this
                lines(xval[items], yraw[items], col=colors[i], type='b')
            }
            lines(sort(xval), sort(yval), lwd=2)
        }
    } else if (type == "individual") {
        xx <- seq(-10, 10, length=200)
        ymax <- max(yval)
        ymin <- min(yval)
        for (this in series) {
            plot(sort(xval), sort(yval), col=model.color, ylim=c(ymin, ymax))
            lines(sort(xval), sort(yval), col=model.color)
            text("topleft",
                 paste('SS Ratio =', format(x@ss.ratio[this], digits=4)), adj=0)
            points(x@concentrations[this],
                   x@intensities[this],
                   col='red',
                   pch=16)
            items <- x@design@layout$Series == this
            lines(xval[items], yraw[items], type='b')
        }
        lines(sort(xval), sort(yval), lwd=2)
    } else if (type == "steps") {
        xplot <- NA
        yplot <- NA
        xfit <- NA
        yfit <- NA
        for (this in series) {
            items <- x@design@layout$Series == this
            yval2 <- yraw[items]
            yfiti <- yval[items]
            l <- length(yval2)
            x.ser <-  yval2[1:l-1]
            y.ser <-  yval2[2:l]
            x.fit <-  yfiti[1:l-1]
            y.fit <-  yfiti[2:l]
            xplot <- c(xplot, (x.ser+y.ser) / 2)
            yplot <- c(yplot, y.ser-x.ser)
            xfit <- c(xfit, (x.fit+y.fit) / 2)
            yfit <- c(yfit, y.fit-x.fit)
        }
        xplot <- xplot[-1]
        yplot <- yplot[-1]
        xfit <- xfit[-1]
        yfit <- yfit[-1]
        plot(xplot, yplot,
             xlab="(Step[n+1]+Step[n])/2",
             ylab="Step[n+1] - Step[n]")
        .loess.line(xplot, yplot)
        abline(0, 0, col="blue")


        o <- order(xfit)
        lines(xfit[o], yfit[o], col=model.color)
        legend("bottomleft",
               c("loess", "model"),
               col=c("red", model.color),
               lty=1)
    } else if (type == "resid") {
        ## show a plot of the residuals v.s. estimated concentration
        ## to check for heteroscedasticity
        r <- resid(x)

        ## fit a model of abs(residual) v.s. estimated conc.
        ## to do a rough check for increasing heteroscedasticity
        l <- lm(abs(r) ~ xval)
        s <- summary(l)
        vals <- s$coefficients["xval", c("Estimate", "Pr(>|t|)")]
        subt <- paste("abs(Residual) Slope:",
                      round(vals[1], 2),
                      ", p-value = ",
                      round(vals[2], 3))
        plot(xval, r, sub=subt, xlab=xlab, ylab='Residual Instensity', ...)
        abline(h=0, col=model.color)
        abline(l, col="red")
        l <- lm(-abs(r) ~ xval)
        abline(l, col="red")

        # .loess.line(xval[r > 0], r[r > 0], span=0.75)
        # .loess.line(xval[r < 0], r[r < 0], span=0.75)
    }

    invisible(x)
})




##-----------------------------------------------------------------------------
# REQUIRED INPUTS:
# rppa    = an RPPA object
# design  = a RPPADesign object
# measure = name of column in rppa@data to view as intensity
#
# OPTIONAL INPUTS CONTROLLING THE ALGORITHM:
# method   = how to fit the alpha and beta values. simplest is quantiles, which simply
#     uses the 5th and 95th percentiles. Default is "nls", which just takes wahtever
#     the 'nls' method provides. Last is 'rlm', which performs a logistic transform
#     and tries a robust linear fit.
# ignoreNegative = if true, then alues below zero are converted to NA before estimating
#     alpha and beta. Only matters for the 'quantiles' method.
# ci       = if true, calculate a 90% confidence interval for the LC50 supercurve values
# bayesian = if true, use slide level logistic model as a bayesian prior for fitting
#     individual dilution series rather than an absolute set of constants. In particular
#     the baseline response 'alpha' is treated as a bayesian prior to account for
#     individual series having varying background
#
# OPTIONAL INPUTS CONTROLLING VERBOSITY:
# trace = passed to nls in the 'bayesian' portion
# verbose = logical value; should the function tell you what it is doing
# veryVerbose = logical value; should the function overwhelm you
#               to tell you what it is doing
# warnLevel = used to set the 'warn' option before calling 'rlm'.
#             since this is wrapped in a 'try', it won't cause
#             failure but will give us a chance to figure out
#             which dilution series fail. Setting warnLevel to
#             two or greater may change the computed results.
# balance = optional parameter specifying the spot concentration in each dilution step.
#           By default, a 2x dilution is assumed for each sample. If you want to override
#           this, you will need to provide a matrix(nrow=nsample, ncol=ndilutions)
#           that gives concentrations in each row on a log2 scale centered at 0.
#
# trim = optional parameter specifying whether to trim the concentrations.  If FALSE, concentrations will not be trimmed / bounded.
#
# OUTPUTS:
# An object of the RPPAFit class described above.
#
# N.B.!!!!
# If you change this routine please run dilutionFitRegessionTest() as at
# least a basic test that the routine still runs reasonably correctly
#

# KRC: Added yet another parameter, 'method', to control the ways to fit
# alpha and beta. Since we still don't know which method is best, we should
# make it an option so we (and others) can continue to evaluate it.

RPPAFitParams <- function(measure,
                          xform=function(x) x,
                          method=c("nls", "nlrob", "nlrq"),
                          ci=FALSE,
                          ignoreNegative=TRUE,
                          trace=FALSE,
                          verbose=FALSE,
                          veryVerbose=FALSE,
                          warnLevel=0,
                          trim=TRUE,
                          model=c("logistic", "loess", "cobs")) {
    ## Check arguments
    if (!is.character(measure)) {
        stop(sprintf("argument %s must be character",
                     sQuote("measure")))
    } else if (!(length(measure) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("measure")))
    }

    if (!is.function(xform)) {
        stop(sprintf("argument %s must be function",
                     sQuote("xform")))
    }

    method <- match.arg(method)

    if (!is.logical(ci)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("ci")))
    } else if (!(length(ci) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("ci")))
    }

    if (!is.logical(ignoreNegative)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("ignoreNegative")))
    } else if (!(length(ignoreNegative) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("ignoreNegative")))
    }

    if (!is.logical(trace)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("trace")))
    } else if (!(length(trace) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("trace")))
    }

    if (!is.logical(verbose)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("verbose")))
    } else if (!(length(verbose) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("verbose")))
    }

    if (!is.logical(veryVerbose)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("veryVerbose")))
    } else if (!(length(veryVerbose) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("veryVerbose")))
    }

    if (!is.numeric(warnLevel)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("warnLevel")))
    } else if (!(length(warnLevel) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("warnLevel")))
    }

    warnLevel <- as.integer(warnLevel)

    if (!is.logical(trim)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("trim")))
    } else if (!(length(trim) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("trim")))
    }

    model <- match.arg(model)

    ## Create new class
    new("RPPAFitParams",
        measure=measure,
        xform=xform,
        method=method,
        ci=ci,
        ignoreNegative=ignoreNegative,
        trace=trace,
        verbose=verbose,
        veryVerbose=veryVerbose,
        warnLevel=warnLevel,
        trim=trim,
        model=model)
}


##-----------------------------------------------------------------------------
RPPAFitFromParams <- function(rppa, design, fitparams) {
    ## Check arguments
    if (!inherits(rppa, "RPPA")) {
        stop(sprintf("argument %s must be RPPA object",
                     sQuote("rppa")))
    }

    if (!inherits(design, "RPPADesign")) {
        stop(sprintf("argument %s must be RPPADesign object",
                     sQuote("design")))
    }

    if (!inherits(fitparams, "RPPAFitParams")) {
        stop(sprintf("argument %s must be RPPAFitParams object",
                     sQuote("fitparams")))
    }

    ## Begin processing
    # .attachslot(fitparams)
    RPPAFit(rppa,
            design,
            fitparams@measure,
            fitparams@xform,
            fitparams@method,
            fitparams@ci,
            fitparams@ignoreNegative,
            fitparams@trace,
            fitparams@verbose,
            fitparams@veryVerbose,
            fitparams@warnLevel,
            fitparams@trim,
            fitparams@model)
}


##-----------------------------------------------------------------------------
RPPAFit <- function(rppa,
                    design,
                    measure,
                    xform=function(x) x,
                    method=c("nls", "nlrob", "nlrq"),
                    ci=FALSE,
                    ignoreNegative=TRUE,
                    trace=FALSE,
                    verbose=FALSE,
                    veryVerbose=FALSE,
                    warnLevel=0,
                    trim=TRUE,
                    model=c("logistic", "loess", "cobs")) {
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

    if (!is.function(xform)) {
        stop(sprintf("argument %s must be function",
                     sQuote("xform")))
    }

    method <- match.arg(method)

    if (!is.logical(ci)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("ci")))
    } else if (!(length(ci) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("ci")))
    }

    if (!is.logical(ignoreNegative)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("ignoreNegative")))
    } else if (!(length(ignoreNegative) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("ignoreNegative")))
    }

    if (!is.logical(trace)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("trace")))
    } else if (!(length(trace) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("trace")))
    }

    if (!is.logical(verbose)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("verbose")))
    } else if (!(length(verbose) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("verbose")))
    }

    if (!is.logical(veryVerbose)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("veryVerbose")))
    } else if (!(length(veryVerbose) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("veryVerbose")))
    }

    if (!is.numeric(warnLevel)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("warnLevel")))
    } else if (!(length(warnLevel) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("warnLevel")))
    }

    warnLevel <- as.integer(warnLevel)

    if (!is.logical(trim)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("trim")))
    } else if (!(length(trim) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("trim")))
    }

    model <- match.arg(model)

    ## Begin processing

    # make sure the input types are sensible

    if (missing("measure")) {
        stop("You must supply the name of the measurement column to fit")
    }
    dn <- dimnames(rppa@data)[[2]]
    temp <- pmatch(measure, dn)
    if (is.na(temp)) {
        stop("You must supply the name of a valid measurement column to fit")
    }
    if (length(temp) > 1) {
        stop("'measure' must identify a unique column of 'rppa'")
    }
    measure <- dn[temp]

    l1 <- levels(design@layout$Sample)
    l2 <- levels(rppa@data$Sample)
    if (!(length(l1) == length(l2)) || sum(l1 != l2) > 0) {
        warning("Number of sample labels in design does not match number of sample labels given in raw RPPA quantification file.")
    }

    call <- match.call()
    intensity <- xform(rppa@data[, measure])

    if (warnLevel < 0) {
        silent <- TRUE
    } else {
        silent <- FALSE
    }

    # perform the first pass to initialize the estimates
    first <- .firstPass(intensity, design, ignoreNegative)
    passer    <- first$passer
    gamma.est <- first$gamma
    if (verbose) {
        cat(paste('Completed first pass. Parameters:\n\tlBot =', first$lBot,
                  '\n\tlTop =', first$lTop, '\n\tG =', first$gamma, '\n\n'))
    }

    # put our current guess at the x and y values into vectors
    # here we must finally explicitly poke into the internals of the RPPADesign
    # class in order to omit the control spots.
    yval <- intensity[!.controlVector(design)]

    fc <- switch(model,
                 cobs = new("cobsFitClass"),
                 loess = new("loessFitClass"),
                 logistic = new("logisticFitClass"),
                 NULL)

    if (is.null(fc)) {
        stop("Unknown model type in RPPAfit")
    }



    ## do a two pass estimation. first using rough conc. estimates,
    ## then using better ones
    for (pass in 1:2) {
        if (pass == 1) {
            xval <- getSteps(design) + passer[names(design)]
        } else {
            xval <- getSteps(design) + pass2[names(design)]
        }

        # Fit a response curve for the slide of the form yval = f(xval)
        fc <- fitSlide(fc, conc=xval, intensity=yval, method=method)

        # Conditional on the response curve fit for the slide
        # perform a separate fit of the EC50 values for each dilution series.
        # If the option 'bayesian' is true, then we allow different alpha's
        # for each series to fine tune the baseline.  If 'bayesian' is false,
        # then we perform a logit transform and compute a robust linear model.
        steps <- getSteps(design)
        series <- seriesNames(design)
        pass2 <- rep(NA, length(series))

        names(pass2) <- series
        pval2  <- pass2
        ss.ratio <- pass2
        warn2  <- rep('', length(series))
        names(warn2) <- series
        for (this in series) {
            items <- names(design) == this

            fs <- fitSeries(fc,
                            diln=steps[items],
                            intensity=yval[items],
                            est.conc=passer[this],
                            method=method,
                            silent=silent,
                            trace=trace)
            pass2[this] <- fs$est.conc
            warn2[this] <- fs$warn
            resids <- fs$resids
            pval2[this] = 0 # pval is deprecated, doesn't work anyway, not given by .rnls
            # Compute R^2 as sum(r[i]^2) / sum((y[i]-mean(y))^2) = fraction of
            # variance explained for this series
            ratio <- 1 - (sum(resids*resids) /
                            (var(yval[items])*(length(yval[items])-1)))
            ss.ratio[this] <- ratio
        }

        if (verbose) {
            cat('Finished estimating EC50 values.  Coefficients:\n')
            print(summary(pass2))
            cat('SS Ratio:\n')
            print(summary(ss.ratio))
        }
    }

    ## Create new class
    result <- new("RPPAFit",
                  call=call,
                  rppa=rppa,
                  design=design,
                  measure=measure,
                  method=method,
                  trimset=c(lo.intensity = -100000,
                            hi.intensity =  100000,
                            lo.conc = -1000,
                            hi.conc = 1000),
                  model=fc,
                  concentrations=pass2,
                  lower=pass2,
                  upper=pass2,
                  intensities=fitted(fc, pass2),
                  ss.ratio=ss.ratio,
                  p.values=pval2,
                  conf.width=0,
                  warn=warn2)
                  #version=packageDescription("SuperCurve", fields="Version"))  ##### By Wenbin

    if (trim) {
        tc <- trimConc(fc,
                       conc = fitted(result, "X"),
                       intensity=intensity,
                       design=design)
        series.conc <- result@concentrations
        series.conc[series.conc < tc$lo.conc] <- tc$lo.conc
        series.conc[series.conc > tc$hi.conc] <- tc$hi.conc
        result@concentrations <- series.conc
        result@trimset <- unlist(tc)
    }

    if (ci) {
        # compute confidence intervals for the estimates
        if (verbose) {
            cat("Computing confidence intervals...", "\n")
        }
        result <- getConfidenceInterval(result)
    }

    result
}


##-----------------------------------------------------------------------------
getConfidenceInterval <- function(result, alpha=0.10, nSim=50) {
    ## Check arguments
    if (!inherits(result, "RPPAFit")) {
        stop(sprintf("argument %s must be RPPAFit object",
                     sQuote("result")))
    }

    if (!is.numeric(alpha)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("alpha")))
    } else if (!(length(alpha) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("alpha")))
    }

    if (!is.numeric(nSim)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("nSim")))
    } else if (!(length(nSim) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("nSim")))
    }

    nSim <- as.integer(nSim)

    ## Begin processing
    method <- result@method
    silent <- TRUE
    series <- seriesNames(result@design)
    steps <- getSteps(result@design)
    res <- resid(result)         # actual residuals on the intensity scale
    yval <- fitted(result, "Y")  # best fit of the intensities
    xval <- fitted(result, "X")  # best fit concentrations on the log2 scale
    # we assume the residuals vary smoothly with the concentration or intensity
    # so, we use loess to fit the absolute residuals as a function of the
    # fitted concentration
    lo <- loess(ares ~ xval, data.frame(ares=abs(res), xval=xval))
    # We assume the residuals locally satisfy R ~ N(0, sigma). Then the
    # expected value of |R| is sigma*sqrt(2/pi), so:
    sigma <- sqrt(pi/2) * fitted(lo)
    for (this in series) {
        items <- result@design@layout$Series == this
        xhat <- xval[items]
        yhat <- yval[items]

        sim <- rep(NA, nSim)
        for (j in seq(1, nSim)) {
            # sample the residuals
            resid.boot <- rnorm(sum(items), 0, sigma[items])
            # add resid.boot to y_hat;  refit;
            ysim <- yhat + resid.boot

            fs <- fitSeries(result@model,
                            diln=steps[items],
                            intensity=ysim,
                            est.conc=xhat[1],
                            method=method,
                            silent=silent,
                            trace=FALSE)
            sim[j] <- fs$est.conc
        }
        result@lower[this] <- quantile(sim, alpha/2, na.rm=TRUE)
        result@upper[this] <- quantile(sim, 1 - alpha/2, na.rm=TRUE)
    }
    result@conf.width <- 1-alpha

    result
}

