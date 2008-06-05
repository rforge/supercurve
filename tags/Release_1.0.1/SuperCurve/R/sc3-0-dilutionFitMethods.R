###
### DILUTIONFITMETHODS.R
###


##=============================================================================
setOldClass("cobs")
setOldClass("loess")

setClass("FitClass",
         representation("VIRTUAL"))

setClass("LogisticFitClass",
         representation("FitClass",
                        coefficients="numeric"),
         prototype=prototype(coefficients=c(alpha=0, beta=0, gamma=0)))

setClass("CobsFitClass",
         representation("FitClass",
                        model="cobs",
                        lambda="numeric"),
         prototype=prototype(lambda=0))

setClass("LoessFitClass",
         representation("FitClass",
                        model="loess"))


####################################################################
## GENERIC METHODS FOR FitClass: Typically throw an error since they
## must be implemented by derived classes.

##-----------------------------------------------------------------------------
## Finds the concentration for an individual dilution series given the
## curve fit for the slide
##
## Inputs
## dilutions and intensities for a single dilution series
## est.conc = starting estimated concentration for dilution = 0
##
## Outputs
## est.conc = estimated concentration for dilution = 0
##
setMethod("fitSeries", "FitClass",
          function(object,
                   diln,
                   intensity,
                   est.conc,
                   method="nls",
                   silent=TRUE,
                   trace=FALSE,
                   ...) {
    stop(sprintf("%s must be implemented by any subclass of %s",
                 sQuote("fitSeries"), sQuote("FitClass")))
})


##-----------------------------------------------------------------------------
## Use the conc and intensity series for an entire slide to
## fit a curve for the slide of intensity = f(conc)
setMethod("fitSlide", "FitClass",
          function(object,
                   conc,
                   intensity,
                   ...) {
    stop(sprintf("%s must be implemented by any subclass of %s",
                 sQuote("fitSlide"), sQuote("FitClass")))
})


##-----------------------------------------------------------------------------
## Returns concentration and intensity cutoffs for the model
setMethod("trimConc", "FitClass",
          function(object,
                   conc,
                   intensity,
                   design,
                   ...) {
    stop(sprintf("%s must be implemented by any subclass of %s",
                 sQuote("trimConc"), sQuote("FitClass")))
})


##-----------------------------------------------------------------------------
## Extracts model coefficients from objects returned by modeling functions.
## N.B.: Should be overridden by classes that have coefficients!
setMethod("coef", "FitClass",
          function(object,
                   ...) {
    NULL
})


###################################################################
## Utility fuctions used to implement methods for derived classes
## :KRC: Should these be used for the FitClass method so we can
## both document them and use them if we decide to develop new
## and improved fitting algorithms in the future?

##-----------------------------------------------------------------------------
.slide.model <- function(conc) {
    obj <- get(".RPPA.fit.model", env=.GlobalEnv)
    fitted(obj, conc)
}


##-----------------------------------------------------------------------------
## Fit the dilution series to the model for the slide
.series.fit <- function(object,
                        diln,
                        intensity,
                        est.conc,
                        method=c("nls", "nlrob", "nlrq"),
                        silent=TRUE,
                        trace=FALSE,
                        ...) {
    method <- match.arg(method)

    dum <- list(Y=intensity,
                Steps=diln)

    ## Ensure necessary packages available
    if (method == "nlrob" && !require(robustbase)) {
        stop(sprintf("%s package required for %s method",
                     sQuote("robustbase"), sQuote(method)))
    } else if (method == "nlrq" && !require(quantreg)) {
        stop(sprintf("%s package required for %s method",
                     sQuote("quantreg"), sQuote(method)))
    }

    ## Define regression method
    nlsmeth <- switch(EXPR=method,
                      nls=nls,
                      nlrob=nlrob,
                      nlrq=function(...) {
                               nlrq(...,
                                    control=nlrq.control(maxiter=10, eps=1e-02))
                           },
                      stop(sprintf("unrecognized regression method %s",
                                   sQuote(method))))

    ## Function .slide.model references object back here for curve model
    ## :TBD: Isn't writing into global environment considered rude?
    assign(".RPPA.fit.model", object, env=.GlobalEnv)

    tmp <- try(nlsmeth(Y ~ SuperCurve:::.slide.model(Steps+X),
                       data=data.frame(Y=intensity,
                                       Steps=diln),
                       start=list(X=est.conc),
                       trace=trace),
               silent)

    if (is(tmp, 'try-error')) {
        if (!silent) {
            warning('unavoidable nls/rlm error')
        }
        warn <- "unavoidable nls-error"
        resids <- 0
    } else {
        ## Model fitting succeded, so we can continue
        warn <- ""
        est.conc <- coef(tmp)
        resids <- residuals(tmp)
    }

    list(warn=warn,
         est.conc=est.conc,
         resids=resids)
}


##-----------------------------------------------------------------------------
.generic.trim <- function(object,
                          conc,
                          intensity,
                          design,
                          ...) {
    ## Trim the concentration estimates to bound lower and upper
    ## concentration estimates at the limits of what can be detected
    ## given our background noise.

    max.step <- max(getSteps(design))
    min.step <- min(getSteps(design))
    r <- fitted(object, conc) - intensity  # residuals
    s <- mad(r, na.rm=TRUE)
    lBot <- quantile(intensity, probs=c(.01), na.rm=TRUE)
    lTop <- quantile(intensity, probs=c(.99), na.rm=TRUE)

    ## Use trim * (median absolute deviation of residuals)
    ## as estimate for background noise

    ## By default, trim is set to 2.
    ## Trim is arbitrary, and was based on trying
    ## multiple cutoff levels across multiple slides.

    trim <- 2 * s
    lo.intensity <- lBot + trim

    ## In practice, we rarely see the response "top out".
    ## Do not trim at the top end.
    # hi.intensity <- lTop - trim
    hi.intensity <- max(intensity)

    ## Search fitted model to find conc corresponding to lo.intensity
    lo.conc <- bisection.search(min(conc, na.rm=TRUE),
                                max(conc, na.rm=TRUE),
                                function(x, object) {
                                    fitted(object, x) - lo.intensity
                                },
                                f.extra=object,
                                tol=0.1)$x
    ## Adjust min allowable conc to point at undiluted spot
    lo.conc <- lo.conc - max.step

    hi.conc <- bisection.search(min(conc, na.rm=TRUE),
                                max(conc, na.rm=TRUE),
                                function(x, object) {
                                    fitted(object, x) - hi.intensity
                                },
                                f.extra=object,
                                tol=0.1)$x
    ## :TBD: comment right?
    ## Adjust min allowable conc to point at most dilute spot
    hi.conc <- hi.conc - min.step

    list(lo.intensity=lo.intensity,
         hi.intensity=hi.intensity,
         lo.conc=lo.conc,
         hi.conc=hi.conc)
}


###################################################################
## Loess model class
##

##-----------------------------------------------------------------------------
setMethod("fitSlide", "LoessFitClass",
          function(object,
                   conc,
                   intensity,
                   ...) {
    fit.lo <- loess(intensity ~ conc)

    ## Create new class
    new("LoessFitClass",
        model=fit.lo)
})


##-----------------------------------------------------------------------------
setMethod("fitted", "LoessFitClass",
          function(object,
                   conc,
                   ...) {
    fit.lo <- object@model

    ## loess will not interpolate beyond the initial fitted conc. range
    lo <- min(fit.lo$x)
    conc <- pmax(min(fit.lo$x), conc)
    conc <- pmin(max(fit.lo$x), conc)
    conc.pred <- conc
    conc.pred[is.na(conc)] <- lo

    intensity <- predict(fit.lo, data.frame(conc=conc.pred))
    intensity[is.na(conc)] <- NA

    intensity
})


##-----------------------------------------------------------------------------
setMethod("fitSeries", "LoessFitClass",
          function(object,
                   diln,
                   intensity,
                   est.conc,
                   method="nls",
                   silent=TRUE,
                   trace=FALSE,
                   ...) {
    .series.fit(object, diln, intensity, est.conc, method, silent, trace)
})


##-----------------------------------------------------------------------------
setMethod("trimConc", "LoessFitClass",
          function(object,
                   conc,
                   intensity,
                   design,
                   ...) {
    .generic.trim(object, conc, intensity, design, ...)
})


###################################################################
## Cobs model class
##

##-----------------------------------------------------------------------------
setMethod("fitSlide", "CobsFitClass",
          function(object,
                   conc,
                   intensity,
                   ...) {
    if (!require(cobs)) {
        stop(sprintf("%s package required for %s method",
                     sQuote("cobs"), sQuote("fitSlide")))
    }

    fit.lo <- cobs(conc,
                   intensity,
                   constraint="increase",
                   nknots=20,
                   lambda=object@lambda,
                   degree=2,
                   tau=0.5,
                   print.warn=FALSE,
                   print.mesg=FALSE)

    ## Create new class
    new("CobsFitClass",
        model=fit.lo,
        lambda=fit.lo$lambda)
})


##-----------------------------------------------------------------------------
.predict.spline <- function(xvec, aknot, acoef) {
    stopifnot(is.numeric(xvec))
    stopifnot(is.numeric(aknot))
    stopifnot(is.numeric(acoef))

    nknot <- length(aknot)
    aknotnew <- c(aknot[1], aknot[1], aknot, aknot[nknot], aknot[nknot])
    ncoef <- length(acoef)

    xvec[xvec < (aknot[1] + (1e-8))] <- aknot[1] + (1e-8)
    xvec[xvec > (aknot[nknot] - (1e-8))] <- aknot[nknot] - (1e-8)

    a <- spline.des(aknotnew, xvec, ord=3)
    fvalvec <- (a$design) %*% acoef

    return(as.vector(fvalvec))
}


##-----------------------------------------------------------------------------
setMethod("fitted", "CobsFitClass",
          function(object,
                   conc,
                   ...) {
    fit <- object@model

    ## Predict missing values at min intensity
    lo <- min(fit$x)
    conc.pred <- conc
    conc.pred[is.na(conc)] <- lo

    ## :TODO: Add argument to enable Jianhua's code, or remove it
    intensity <- if (TRUE) {
                     ## predict.cobs is irritating
                     ## It returns predicted values after sorting on the input
                     ## vector. So we get intensity ~ sort(fit)
                     ## We need to undo this sort to find predictions using the
                     ## original concentration ordering

                     n <- length(conc.pred)
                     if (n > 1) {
                         ## Undo sort on fit
                         o <- sort.list(conc.pred,
                                        method="quick",
                                        na.last=NA)
                         u <- rep(NA, n)
                         u[o] <- 1:n
                         cobs.intensity <- predict(fit, conc.pred[o])[, "fit"]
                         cobs.intensity[u]
                     } else {
                         ## Only one data point
                         predict(fit, conc.pred)[, "fit"]
                     }
                 } else {
                     if (!require(splines)) {
                         stop(sprintf("%s package required for %s method",
                                      sQuote("splines"), sQuote("fitted")))
                     }

                     ## The above sort and unsort process is yucky and a bit
                     ## slow. Jianhua did not use the cobs predict method and
                     ## instead evaluates the spline directly. Unfortunately,
                     ## there seems to be a bug in .predict.spline where it does
                     ## not have the correct number of coefficients sometimes.
                     .predict.spline(conc.pred,
                                     fit$knots,
                                     fit$coef)
                 }

    intensity[is.na(conc)] <- NA

    intensity
})


##-----------------------------------------------------------------------------
setMethod("fitSeries", "CobsFitClass",
          function(object,
                   diln,
                   intensity,
                   est.conc,
                   method="nls",
                   silent=TRUE,
                   trace=FALSE,
                   ...) {
    .series.fit(object, diln, intensity, est.conc, method, silent, trace)
})


##-----------------------------------------------------------------------------
setMethod("trimConc", "CobsFitClass",
          function(object,
                   conc,
                   intensity,
                   design,
                   ...) {
    .generic.trim(object, conc, intensity, design, ...)
})


###################################################################
## Logistic model class
##

##-----------------------------------------------------------------------------
## N.B.: rnls does not work with local functions
.ea <- function(x) {
    exp(x) / (1 + exp(x))
}


##-----------------------------------------------------------------------------
.lp <- function(p) {
    log(p/(1-p))
}


##-----------------------------------------------------------------------------
.coef.quantile.est <- function(intensity) {
    stopifnot(is.numeric(intensity))

    lBot <- quantile(intensity, probs=c(.05), na.rm=TRUE)
    lTop <- quantile(intensity, probs=c(.95), na.rm=TRUE)
    p.alpha <- lBot
    p.beta  <- lTop-lBot
    p.gamma <- log(2)  # Assume linear response on log2 scale as first guess
    list(alpha=p.alpha,
         beta=p.beta,
         gamma=p.gamma)
}


##-----------------------------------------------------------------------------
setMethod("fitSlide", "LogisticFitClass",
          function(object,
                   conc,
                   intensity,
                   ...) {
    cf <- as.list(coef(object))

    if (cf$gamma == 0) {
        ## Initialize coefficients
        cf <- .coef.quantile.est(intensity)
    }
    dum <- data.frame(xval=conc,
                      yval=log(intensity + 5000))
    nls.model <- try(nls(yval ~ log(alpha + beta*.ea(gamma*xval) + 5000),
                         data=dum,
                         start=list(alpha=cf$alpha,
                                    beta=cf$beta,
                                    gamma=cf$gamma),
                         control=nls.control(maxiter=100),
                         na.action='na.omit'))
    if (is(nls.model, 'try-error')) {
        warning("unable to perform first pass overall slide fit. trying quantiles.")
        ## Crude (but robust) way to get alpha and beta
        cf <- .coef.quantile.est(intensity)
    } else {
        cf <- coef(nls.model)
        p.alpha <- cf['alpha']
        p.beta  <- cf['beta']
        p.gamma <- cf['gamma']
        cf <- list(alpha=p.alpha,
                   beta=p.beta,
                   gamma=p.gamma)
    }

    ## Create new class
    new("LogisticFitClass",
        coefficients=unlist(cf))
})


##-----------------------------------------------------------------------------
setMethod("fitted", "LogisticFitClass",
          function(object,
                   conc,
                   ...) {
    cf <- as.list(object@coefficients)
    cf$alpha + cf$beta * .ea(cf$gamma * conc)
})


##-----------------------------------------------------------------------------
setMethod("fitSeries", "LogisticFitClass",
          function(object,
                   diln,
                   intensity,
                   est.conc,
                   method="nls",
                   silent=TRUE,
                   trace=FALSE,
                   ...) {
    .series.fit(object, diln, intensity, est.conc, method, silent, trace)
})


##-----------------------------------------------------------------------------
setMethod("trimConc", "LogisticFitClass",
          function(object,
                   conc,
                   intensity,
                   design,
                   ...) {
    ## Trim the concentration estimates to bound lower and upper
    ## concentration estimates at the limits of what can be detected
    ## given our background noise.
    cf <- as.list(object@coefficients)
    max.step <- max(getSteps(design))
    min.step <- min(getSteps(design))
    r <- fitted(object, conc) - intensity  # residuals
    s <- mad(r, na.rm=TRUE)

    ## Use trim * (median absolute deviation of residuals)
    ## as estimate for background noise

    ## By default, trim is set to 2.
    ## Trim is arbitrary, and was based on trying
    ## multiple cutoff levels across multiple slides.
    trim <- 2 * s / cf$beta
    lo.conc <- .lp(trim) / cf$gamma - max.step
    hi.conc <- .lp(1-trim) / cf$gamma - min.step

    lo.intensity <- cf$alpha + cf$beta * trim
    hi.intensity <- cf$alpha + cf$beta - cf$beta * trim

    list(lo.intensity=lo.intensity,
         hi.intensity=hi.intensity,
         lo.conc=lo.conc,
         hi.conc=hi.conc)
})


##-----------------------------------------------------------------------------
## Extracts model coefficients from LogisticFitClass.
setMethod("coef", "LogisticFitClass",
          function(object,
                   ...) {
    object@coefficients
})

