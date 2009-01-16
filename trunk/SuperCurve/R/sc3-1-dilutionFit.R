###
### DILUTIONFIT.R
###


##=============================================================================
setClass("RPPAFit",
         representation=list(call="call",              # function call used to create the model
                             rppa="RPPA",              # required parameter
                             design="RPPADesign",      # required parameter
                             measure="character",      # required parameter
                             method="character",       # optional parameter
                             trimset="numeric",        # list(lo.intensity, hi.intensity, lo.conc, hi.conc, level)
                             model="FitClass",         # curve model
                             concentrations="numeric", # main output
                             lower="numeric",          # confidence interval
                             upper="numeric",          # confidence interval
                             conf.width="numeric",     # width of confidence interval
                             intensities="numeric",    # intensities related to series concentrations
                             ss.ratio="numeric",
                             warn="character",
                             version="character"))


##=============================================================================
setClassUnion("OptionalFunction", c("function", "NULL"))
setClass("RPPAFitParams",
         representation=list(measure="character",
                             xform="OptionalFunction",
                             method="character",
                             ci="logical",
                             ignoreNegative="logical",
                             trace="logical",
                             verbose="logical",
                             veryVerbose="logical",
                             warnLevel="numeric",
                             trim="numeric",
                             model="character"))


##-----------------------------------------------------------------------------
is.RPPAFit <- function(x) {
    inherits(x, "RPPAFit")
}


##-----------------------------------------------------------------------------
is.RPPAFitParams <- function(x) {
    inherits(x, "RPPAFitParams")
}


##-----------------------------------------------------------------------------
setMethod("summary", "RPPAFit",
          function(object,
                   ...) {
    cat(sprintf("An %s object constructed via the function call:",
                class(object)), "\n")
    cat(" ", as.character(list(object@call)), "\n")
    cat("with fitting parameters:", "\n")
    cat(" ", sprintf("measure:   %s", object@measure), "\n")
    cat(" ", sprintf("method:    %s", object@method), "\n")
    cat(" ", sprintf("model:     %s", class(object@model)), "\n")
    trimlevel <- object@trimset["level"]
    if (trimlevel) {
        cat(" ", sprintf("trimlevel: %s", trimlevel), "\n")
    }
    invisible(NULL)
})


##-----------------------------------------------------------------------------
## Provides a geographic plot of some measure computed from the fit.
## Default is to image the (raw) residuals, with options for other forms
## of the residuals or for the fitted concentrations (X) or intensities (Y).
setMethod("image", signature(x="RPPAFit"),
          function(x,
                   measure=c("Residuals",
                             "ResidualsR2",
                             "StdRes",
                             "X",
                             "Y"),
                   main=.mkPlotTitle(measure, x@rppa@antibody),
                   ...) {
    ## Check arguments
    measure <- match.arg(measure)

    ## Begin processing
    rppa <- x@rppa
    rppa@data[[measure]] <- switch(EXPR=measure,
                                   Residuals=resid(x),
                                   StdRes=resid(x, "standardized"),
                                   ResidualsR2=resid(x, "r2"),
                                   X=fitted(x, "X"),
                                   Y=fitted(x, "Y"),
                                   stop(sprintf("unrecognized measure %s",
                                                sQuote(measure))))

    ## Image the residuals
    imageRPPA <- getMethod("image", class(rppa))
    imageRPPA(rppa,
              measure=measure,
              main=main,
              ...)

    invisible(x)
})


##-----------------------------------------------------------------------------
## We are actually interested in estimating the concentrations for each
## dilution series (which may be the same as a sample). However, when we do
## that we also get estimated concentrations at each spot, with corresponding
## predicted intensities. Default for 'fitted' is to return the per-spot
## fitted 'Y' intensities, with an option to return the per-spot fitted
## 'X' concentrations.
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

    ## Define type of fitted values
    switch(EXPR=type,
           x=, X=fitX,
           y=, Y=fitted(object@model, fitX),
           stop(sprintf("unrecognized fitted values type %s",
                        sQuote(type))))
})


##-----------------------------------------------------------------------------
## Raw residuals are defined so that
##    observed intensity = fitted Y + raw residuals
## standardized residuals are formed from the raw residuals in the obvious way
## linear residuals are the residuals after the logistic transformation
## r2 residuals are expressed as a kind of R^2 number. Specifically, if the
## entire fit had this residual value at each yi, show what would the R^2 for
## the fit would be. This allows us to express residuals on a uniform scale
## across multiple slides and quickly spot "bad" residuals on a uniform scale.
##
## Note that the model fitting is a bit of a hybrid between the linear and
## logistic intensity scales, so it's not completely clear which residuals are
## most meaningful
setMethod("residuals", "RPPAFit",
          function(object,
                   type=c("raw", "standardized", "r2"),
                   ...) {
    ## Check arguments
    type <- match.arg(type)

    ## Begin processing
    res <- object@rppa@data[, object@measure] - fitted(object)

    ## Define residuals type
    switch(EXPR=type,
           raw          = res,
           standardized = scale(res),
           r2           = {
                              y <- object@rppa@data[, object@measure]
                              nobs <- length(y)
                              sigmasq <- var(y) * (nobs-1)
                              1 - (nobs * res * res / sigmasq)
                          },
           stop(sprintf("unrecognized residuals type %s",
                        sQuote(type))))
})


setMethod("resid", "RPPAFit",
          function(object, ...) {
    residuals(object, ...)
})


##-----------------------------------------------------------------------------
## Histogram of the (raw) residuals, with an option to see the standardized
## or linear residuals
setMethod("hist", "RPPAFit",
          function(x,
                   type=c("Residuals", "StdRes", "ResidualsR2"),
                   xlab=NULL,
                   main=NULL,
                   ...) {
    type <- match.arg(type)

    if (is.null(xlab)) {
        xlab <- switch(EXPR=type,
                       Residuals="Residuals",  # unchanged
                       ResidualsR2="Residuals R^2",
                       StdRes="Standardized Residuals")
    }
    if (is.null(main)) {
        main <- .mkPlotTitle(paste("Histogram of", type),
                             x@rppa@antibody)
    }

    translate <- c("raw", "standardized", "r2")
    names(translate) <- c("Residuals", "StdRes", "ResidualsR2")
    res <- resid(x, type=translate[type])
    hist(res,
         main=main,
         sub=paste("File:", x@rppa@file),
         xlab=xlab,
         ...)
})


##-----------------------------------------------------------------------------
.loess.line <- function(xval,
                        yval,
                        col="red",
                        span=(2 / 3),
                        xform=NULL) {
    aux <- loess(yval ~ xval, degree=1, span=span, family="gaussian")$fitted
    o <- order(xval)
    A <- xval[o]
    M <- if (!is.null(xform)) {
             xform(aux[o])
         } else {
             aux[o]
         }
    o <- which(!duplicated(A))
    lines(approx(A[o], M[o]), col=col)
}


##-----------------------------------------------------------------------------
## Plot the results from dilutionFit above to see how well supercurve fits
setMethod("plot", signature(x="RPPAFit", y="missing"),
          function(x, y,
                   type=c("cloud", "series", "individual", "steps", "resid"),
                   col=NULL,
                   main=.mkPlotTitle(paste(.capwords(type), "Fit Plot"),
                                     x@rppa@antibody),
                   xform=NULL,
                   xlab="Log Concentration",
                   ylab="Intensity",
                   ...) {
    ## Check arguments
    type <- match.arg(type)

    ## Begin processing
    trimset <- as.list(x@trimset)
    series <- seriesNames(x@design)
    steps <- getSteps(x@design)
    max.step <- max(steps)
    min.step <- min(steps)

    xval <- fitted(x, "x")
    yval <- fitted(x, "y")
    yraw <- if (!is.null(xform)) {
                if (!is.function(xform)) {
                    stop(sprintf("argument %s must be function, if specified",
                                 sQuote("xform")))
                }

                xform(x@rppa@data[, x@measure])
            } else {
                x@rppa@data[, x@measure]
            }
    model.color <- "green" # :KRC: why is the color hard-coded?
    if (type == "cloud" || type == "series") {
        if (!hasArg(sub)) {
            autosub <- paste("(Conc > -5) Trimmed Mean R^2 =",
                             format(mean(x@ss.ratio[x@concentrations > -5],
                                         trim=0.1),
                                    digits=3),
                             ", Min / Max Valid Conc. =",
                             round(trimset$lo.conc, 2),
                             "/",
                             round(trimset$hi.conc, 2))
            ## :TODO: add 'autosub' to dots and remove one of these plot calls
            plot(xval, yraw,
                 main=main,
                 sub=autosub,
                 xlab="",
                 ylab=ylab,
                 ...)
        } else {
            ## :TBD: Above the 'xlab' argument is "", yet here it is preserved?
            plot(xval, yraw,
                 main=main,
                 xlab=xlab,
                 ylab=ylab,
                 ...)
        }

        lines(sort(xval), sort(yval), col=model.color)
        abline(h=trimset$lo.intensity)
        abline(h=trimset$hi.intensity)

        if (type == "series") {
            if (is.null(col)) {
                col <- hcl(seq(0, 360, length=9)[1:8], c=65, l=65)
            }
            i <- 0
            ncol <- length(col)
            for (this in series) {
                i <- (i %% ncol) + 1
                items <- x@design@layout$Series == this
                lines(xval[items], yraw[items], col=col[i], type="b")
            }
            lines(sort(xval), sort(yval), lwd=2)
        }
    } else if (type == "individual") {
        ymax <- max(yval, na.rm=TRUE)
        ymin <- min(yval, na.rm=TRUE)
        for (this in series) {
            ## :TBD: why aren't {x,y}labs passed to plot()?
            plot(sort(xval), sort(yval),
                 col=model.color,
                 main=main,
                 ylim=c(ymin, ymax))
            lines(sort(xval), sort(yval), col=model.color)
            title(sub=paste("SS Ratio =", format(x@ss.ratio[this], digits=4)))
            points(x@concentrations[this],
                   x@intensities[this],
                   col="red",
                   pch=16)
            items <- x@design@layout$Series == this
            lines(xval[items], yraw[items], type="b")
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
            x.ser <- yval2[1:l-1]
            y.ser <- yval2[2:l]
            x.fit <- yfiti[1:l-1]
            y.fit <- yfiti[2:l]
            xplot <- c(xplot, (x.ser + y.ser) / 2)
            yplot <- c(yplot, y.ser - x.ser)
            xfit <- c(xfit, (x.fit + y.fit) / 2)
            yfit <- c(yfit, y.fit - x.fit)
        }
        xplot <- xplot[-1]
        yplot <- yplot[-1]
        xfit <- xfit[-1]
        yfit <- yfit[-1]
        plot(xplot, yplot,
             main=main,
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
        ## Show a plot of the residuals vs. estimated concentration
        ## to check for heteroscedasticity
        r <- resid(x)

        ## Fit a model of abs(residual) vs. estimated concentration
        ## to do a rough check for increasing heteroscedasticity
        l <- lm(abs(r) ~ xval)
        s <- summary(l)
        vals <- s$coefficients["xval", c("Estimate", "Pr(>|t|)")]
        subt <- paste("abs(Residual) Slope:",
                      round(vals[1], 2),
                      ", p-value =",
                      round(vals[2], 3))
        plot(xval, r,
             main=main,
             sub=subt,
             xlab=xlab,
             ylab="Residual Intensity",
             ...)
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
getConfidenceInterval <- function(result, alpha=0.10, nSim=50) {
    ## Check arguments
    if (!is.RPPAFit(result)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("result"), "RPPAFit"))
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

    ## We assume the residuals vary smoothly with concentration or intensity
    ## so, we use loess to fit the absolute residuals as a function of the
    ## fitted concentration
    lo <- loess(ares ~ xval, data.frame(ares=abs(res), xval=xval))

    ## We assume the residuals locally satisfy R ~ N(0, sigma).
    ## Then the expected value of |R| is sigma*sqrt(2/pi), so:
    sigma <- sqrt(pi / 2) * fitted(lo)

    for (this in series) {
        items <- result@design@layout$Series == this
        xhat <- xval[items]
        yhat <- yval[items]

        sim <- rep(NA, nSim)
        for (j in seq_len(nSim)) {
            ## Sample the residuals
            resid.boot <- rnorm(sum(items), 0, sigma[items])
            ## Add resid.boot to y_hat;  refit;
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
        result@lower[this] <- quantile(sim, probs=alpha/2, na.rm=TRUE)
        result@upper[this] <- quantile(sim, probs=1 - alpha/2, na.rm=TRUE)
    }
    result@conf.width <- 1-alpha

    result
}

