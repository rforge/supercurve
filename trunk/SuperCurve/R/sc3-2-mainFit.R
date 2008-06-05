##
## Constructors for RPPAFit class
##

##-----------------------------------------------------------------------------
## REQUIRED INPUTS:
## rppa    - an RPPA object
## design  - a RPPADesign object
## measure - name of column in rppa@data to view as intensity
##
##-----------------------------------------------------------------------------
## All optional inputs are handled through RPPAFitParams
##
## OPTIONAL INPUTS CONTROLLING THE ALGORITHM:
## model   - which statistical model to fit
## method  - how to fit the alpha and beta values.
## xform   - function used to transform the values before fitting
## trim    - if true, trim the concentrations; otherwise, concentrations will
##           not be trimmed / bounded.
## ci      - if true, calculate a 90% confidence interval for the LC50
##           supercurve values
## ignoreNegative - if true, then values below zero are converted to NA before
##           estimating alpha and beta. Only matters for the 'quantiles' method.
##
## OPTIONAL INPUTS CONTROLLING VERBOSITY:
## trace   - passed to nls in the 'bayesian' portion
## verbose - if true, have the function tell you what it is doing
## veryVerbose - if true, have the function overwhelm you telling you what it
##           is doing
## warnLevel - used to set the 'warn' option before calling 'rlm'.
##           since this is wrapped in a 'try', it won't cause failure but will
##           give us a chance to figure out which dilution series failed.
##           Setting warnLevel to two or greater may change computed results.


##-----------------------------------------------------------------------------
## Refactored this to improve maintainability; everything now happens once.
## Retain this constructor for backwards compatablity.
RPPAFit <- function(rppa,
                    design,
                    measure,
                    model='logistic',
                    xform=function(x) x,
                    method=c("nls", "nlrob", "nlrq"),
                    trim=TRUE,
                    ci=FALSE,
                    ignoreNegative=TRUE,
                    trace=FALSE,
                    verbose=FALSE,
                    veryVerbose=FALSE,
                    warnLevel=0) {
    ## Check arguments
    ## [1] 'rppa' and 'design' are checked in the call to RPPAFitFromParams
    ## [2] Everything else is checked in RPPAFitParams
    params <- RPPAFitParams(measure,
                            model,
                            xform,
                            method,
                            trim,
                            ci,
                            ignoreNegative,
                            trace,
                            verbose,
                            veryVerbose,
                            warnLevel)
    RPPAFitFromParams(rppa, design, params)
}

##-----------------------------------------------------------------------------
## Collect parameters in one place and make sure they are reasonably sensible
RPPAFitParams <- function(measure,
                          model="logistic",
                          xform=function(x) x,
                          method=c("nls", "nlrob", "nlrq"),
                          trim=TRUE,
                          ci=FALSE,
                          ignoreNegative=TRUE,
                          trace=FALSE,
                          verbose=FALSE,
                          veryVerbose=FALSE,
                          warnLevel=0) {
    ## Check arguments
  
    ## Start with the critical parameter, 'model', since it tells us which
    ## statistical model we are supposed to fit. At the time the curve is
    ## fit, 'model' has to be the name of a registered FitClass. You could
    ## conceivably define your own class and register it after setting up the
    ## fit parameters. Because of that fact, we defer some checking until
    ## later (in RPPAFitFromParams).
    if (!is.character(model)) {
        stop(sprintf("argument %s must be a character string",
                     sQuote("model")))
    }
    model <- model[1]

    ## This parameter is also critical, and we cannot complete checking it
    ## until later, when we know whether it refers to an actual column in
    ## the RPPA object being fit.
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

    ## Remaining parameters are less important, so we mostly just transform
    ## them into the correct type and length.
    method <- match.arg(method)
    ci <- as.logical(ci)[1]
    ignoreNegative <- as.logical(ignoreNegative)[1]
    trace <- as.logical(trace)[1]
    verbose <- as.logical(verbose)[1]
    veryVerbose <- as.logical(veryVerbose)[1]
    warnlevel <- as.integer(warnLevel)[1]
    trim <- as.logical(trim)[1]

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
## Compute a multiple of the logit transform.
## in practice, epsilon never changes.
.calcLogitz <- function(data, alpha, beta, gamma, epsilon=0.001) {
    z <- (data - alpha) / beta
    ## Truncate points at min and max for dilution curve
    z[z < epsilon] <- epsilon
    z[z > (1-epsilon)] <- 1-epsilon
    log(z / (1-z)) / gamma
}


##-----------------------------------------------------------------------------
## Computes crude estimates of the parameters (i.e., alpha, beta, gamma, and
## the EC50s) so routine can be initialized.
.firstPass <- function(yval, design, ignoreNegative, epsilon=1e-4) {

    ## 'yval' is a vector of intensity values selected from the MicroVigene
    ## file; its length of 'yval' should match the number of rows in
    ## design@layout, and be in the same order.
    ## in practice, 'epsilon' never changes

    ## Check arguments
    stopifnot(is.numeric(yval))
    stopifnot(inherits(design, "RPPADesign"))
    stopifnot(is.logical(ignoreNegative) && length(ignoreNegative) == 1)
    stopifnot(is.numeric(epsilon))

    ## Begin processing
    if (ignoreNegative) {
        yval[yval < 0] <- NA
    }
    temp <- yval[!.controlVector(design)]

    ## Compute a robust estimate for alpha and beta
    ## Tried to use robust 'rnls' from 'sfsmisc' to fit alpha and beta below
    ## but found it was still too sensitive to outliers.
    ## This is a crude way to get alpha and beta but it seems to be robust.

    ## HERE IS THE PROBLEM!
    ## Quantiles forces a double trim in conjunction with calcLogitz
    ## and epsilon above, artificially shrinking alpha and beta
    ## Try to match vresion 0.12
    # lBot <- quantile(temp, probs=c(.05), na.rm=TRUE)
    # lTop <- quantile(temp, probs=c(.95), na.rm=TRUE)

    lBot <- min(temp, na.rm=TRUE)
    lTop <- max(temp, na.rm=TRUE)

    lindata <- .calcLogitz(yval, lBot, lTop-lBot, 1)
    series <- seriesNames(design) # omits the control spots automagically

    .estimateLogisticSlope <- function(ser, dsn, ld) {
        ## Get the items in this dilution series
        items <- dsn@layout$Series == ser
        ## Get the log2 steps for this series
        steps <- dsn@layout$Steps[items]
        ## Get the transformed intensity values
        x <- ld[items]
        ## Only use valid values
        y <- x[!is.na(x) & !is.infinite(x)]

        if (all(is.na(y))) {
            NA
        } else {
            ## ychange / xchange
            (max(y) - min(y)) / (max(steps) - min(steps))
        }
    }

    ## Initial estimate of the logistic slope across the dilution steps.
    ## There should be a more comprehensible way to write this:
    dat <- unlist(lapply(series,
                         .estimateLogisticSlope,
                         design,
                         lindata))

    ## Force a nonzero slope for starting estimate
    minslope <- 0.001

    ## Filter out zero slopes from blank sample
    dat <- dat[dat > minslope]

    ## Use mean, not median. We may have many blank samples on a slide,
    ## in which case median slope will be 0
    gamma <- mean(dat, trim=0.01)
    gamma <- max(gamma, minslope) # force nonzero slope for starting estimate

    ## For each sample, estimate the offset
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


##-----------------------------------------------------------------------------
## :TODO: Migrate this to .onLoad since it's singleton-like
## :TBD: Shouldn't this be new.env() rather than new("environment")
.rppaModels <- new("environment")

## :TBD: Should this be extended such that names(classname) [if not NULL]
## would return string to be displayed on supercurveGUI dialog?
## For example, "Cobs" is displayed as "Monotone Increasing B-spline" in GUI
## :TBD: Shouldn't this method be exported in NAMESPACE?
##-----------------------------------------------------------------------------
## Register names of what code will consider "valid" models.
registerModel <- function(name, classname) {
    assign(name, classname, envir=.rppaModels)
}

## :TODO: Need routine to fetch all registered model names (for GUI)

## :TODO: Migrate following to .onLoad since registration should occur once
## :KRC: NO! The interface needs ot be exposed properly for other people
## to register models. Don't move anything until we figure that part out...
registerModel("logistic", "LogisticFitClass")
registerModel("cobs",     "CobsFitClass")
registerModel("loess",    "LoessFitClass")


##-----------------------------------------------------------------------------
RPPAFitFromParams <- function(rppa, design, fitparams) {
    ## Check arguments
    if (!inherits(rppa, "RPPA")) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("rppa"), "RPPA"))
    }

    if (!inherits(design, "RPPADesign")) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("design"), "RPPADesign"))
    }

    if (!inherits(fitparams, "RPPAFitParams")) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("fitparams"), "RPPAFitParams"))
    }

    ## Extract the already checked fit parameters
    measure <- fitparams@measure
    model <- fitparams@model
    xform <- fitparams@xform
    method <- fitparams@method
    trim <- fitparams@trim
    ci <- fitparams@ci
    ignoreNegative <- fitparams@ignoreNegative
    trace <- fitparams@trace
    verbose <- fitparams@verbose
    veryVerbose <- fitparams@veryVerbose
    warnLevel <- fitparams@warnLevel

    ## Need to make certain that the 'model' is a registered FitClass
    modelClass <- try(get(model, envir=.rppaModels))
    if (inherits(modelClass, "try-error")) {
        stop(sprintf("argument %s must be the name of a registed fit class",
                     sQuote("model")))
    }

    ## Need to make sure that 'measure' refers to an actual data column
    if (missing("measure")) {
        stop("missing name of the measurement column to fit")
    }
    dn <- dimnames(rppa@data)[[2]]
    temp <- pmatch(measure, dn)
    if (is.na(temp)) {
        stop("supply the name of a valid measurement column to fit")
    } else if (length(temp) > 1) {
        stop(sprintf("argument %s must identify unique column of argument %s",
                     sQuote("measure"),
                     sQuote("rppa")))
    }
    measure <- dn[temp]

    ## Begin processing
    l1 <- levels(design@layout$Sample)
    l2 <- levels(rppa@data$Sample)
    if (!(length(l1) == length(l2)) || sum(l1 != l2) > 0) {
        warning(sprintf("number of sample labels in design (%d) does not match number of sample labels given in raw RPPA quantification file (%d).",
                        length(l1),
                        length(l2)))
    }

    call <- match.call()
    intensity <- xform(rppa@data[, measure])

    silent <- warnLevel < 0

    ## Perform the first pass to initialize the estimates
    first <- .firstPass(intensity, design, ignoreNegative)
    passer    <- first$passer
    gamma.est <- first$gamma
    if (verbose) {
        cat(paste('Completed first pass. Parameters:\n\tlBot =',
                  first$lBot,
                  '\n\tlTop =',
                  first$lTop,
                  '\n\tG =',
                  first$gamma,
                  '\n\n'))
        flush.console()
    }

    ## Put our current guess at the x and y values into vectors
    ## Here we must finally explicitly poke into the internals of RPPADesign
    ## class in order to omit the control spots.
    yval <- intensity[!.controlVector(design)]

    ## Create new class
    fc <- new(modelClass)

    ## Do a two pass estimation, first using rough conc. estimates,
    ## then using better ones
    for (pass in 1:2) {
        xval <- if (pass == 1) {
                    getSteps(design) + passer[names(design)]
                } else {
                    getSteps(design) + pass2[names(design)]
                }

        ## Fit a response curve for the slide of the form yval = f(xval)
        fc <- fitSlide(fc, conc=xval, intensity=yval, method=method)

        ## Conditional on the response curve fit for the slide
        ## Perform a separate fit of EC50 values for each dilution series.
        steps <- getSteps(design)
        series <- seriesNames(design)
        pass2 <- rep(NA, length(series))

        names(pass2) <- series
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
            ## Compute R^2 as sum(r[i]^2) / sum((y[i]-mean(y))^2) = fraction of
            ## variance explained for this series
            ratio <- 1 - (sum(resids*resids) /
                            (var(yval[items])*(length(yval[items])-1)))
            ss.ratio[this] <- ratio
        }

        if (verbose) {
            cat('Finished estimating EC50 values. Coefficients:\n')
            print(summary(pass2))
            cat('SS Ratio:\n')
            print(summary(ss.ratio))
            flush.console()
        }
    }

    ## Create new class
    result <- new("RPPAFit",
                  call=call,
                  rppa=rppa,
                  design=design,
                  measure=measure,
                  method=method,
                  trimset=c(lo.intensity=-100000,
                            hi.intensity=100000,
                            lo.conc=-1000,
                            hi.conc=1000),
                  model=fc,
                  concentrations=pass2,
                  lower=pass2,
                  upper=pass2,
                  intensities=fitted(fc, pass2),
                  ss.ratio=ss.ratio,
                  conf.width=0,
                  warn=warn2,
                  version=packageDescription("SuperCurve", fields="Version"))

    if (trim) {
        tc <- trimConc(fc,
                       conc=fitted(result, "X"),
                       intensity=intensity,
                       design=design)
        series.conc <- result@concentrations
        series.conc[series.conc < tc$lo.conc] <- tc$lo.conc
        series.conc[series.conc > tc$hi.conc] <- tc$hi.conc
        result@concentrations <- series.conc
        result@trimset <- unlist(tc)
    }

    ## Should confidence intervals for the estimates be computed?
    if (ci) {
        if (verbose) {
            cat("Computing confidence intervals...", "\n")
            flush.console()
        }
        result <- getConfidenceInterval(result)
    }

    result
}


##-----------------------------------------------------------------------------
## Extracts model coefficients from objects returned by modeling functions.
setMethod("coef", "RPPAFit",
          function(object,
                   ...) {
    callGeneric(object@model)
})


