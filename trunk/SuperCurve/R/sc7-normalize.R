###
### NORMALIZE.R - Used after RPPAFit to remove sample effect
###


##
## Module Variables
##
## :TODO: Migrate this to .onLoad since it's singleton-like
.NormEnv <- new.env(hash=TRUE)                   # Private environment
attr(.NormEnv, "name") <- "SuperCurveNormalizationMethods"


##
## Private Methods
##

##-----------------------------------------------------------------------------
## Returns private environment for storing registered normalization methods
normenv <- function() {
    return(.NormEnv)
}


##-----------------------------------------------------------------------------
## Normalization method. Sample median is subtracted from each sample.
normalize.median <- function(concs,
                             ...) {
    stopifnot(is.matrix(concs) || is.data.frame(concs))
    stopifnot(is.numeric(rowMedian))

    return(normconcs <- sweep(concs, 1, rowMedian, FUN="-"))
}


##-----------------------------------------------------------------------------
## Normalization method. Median of set of housekeeping antibodies is subtracted
## from each sample.
normalize.house <- function(concs,
                            antibodies,
                            ...) {
    stopifnot(is.matrix(concs) || is.data.frame(concs))

## Alternate setup for nonformal argument
#    dots <- list(...)
#    antibodies <- if ("antibodies" %in% names(dots)) {
#                      dots["antibodies"]
#                  } else {
#                      stop(sprintf("argument %s not provided",
#                                   sQuote("antibodies")))
#                  }
    stopifnot(is.character(antibodies) && length(antibodies) >= 1)

    if (!all(antibodies %in% colnames(concs))) {
        missingNames <- antibodies[!antibodies %in% colnames(concs)]
        stop(sprintf(ngettext(length(missingNames),
                              "argument %s specifies invalid %s column name: %s",
                              "argument %s specifies invalid %s column names: %s"),
                     sQuote("antibodies"),
                     sQuote("concs"),
                     paste(missingNames, collapse=", ")))
    }

    houseMedian <- apply(as.matrix(concs[, antibodies]),
                         1,
                         median,
                         na.rm=TRUE)
    normconcs <- sweep(concs, 1, houseMedian, FUN="-")
    ## Store method-specific info in "normalization" attribute
    attr(normconcs, "normalization") <- list(antibodies=antibodies,
                                             houseMedian=houseMedian)

    return(normconcs)
}


##-----------------------------------------------------------------------------
## Normalization method (variable slope). Sample median is subtracted from
## each sample after applying multiplicative gamma.
normalize.vs <- function(concs,
                         ...) {
    stopifnot(is.matrix(concs) || is.data.frame(concs))
    stopifnot(is.numeric(rowMedian))

    ##-------------------------------------------------------------------------
    ## Estimates the multiplicative gamma terms from variable slope
    ## normalization. It takes as input the data matrix (with samples in
    ## the rows and antibodies in the columns). It is assumed that this matrix
    ## has already had the column median swept out from its columns.
    ## It outputs estimates of the gammas (multiplicative protein effects).
    estimateGamma <- function(Xhat) {
        stopifnot(is.matrix(Xhat) || is.data.frame(Xhat))

        nCol <- ncol(Xhat)
        gamma <- matrix(0, nrow=nCol, ncol=nCol)
        means <- apply(Xhat, 2, mean)

        for (i in seq(1, nCol-1)) {
            for (j in seq(i+1, nCol)) {
                r <- cor(Xhat[, i], Xhat[, j], use="complete.obs")
                a <- Xhat[, i]
                n <- length(a)
                tt <- r * sqrt((n-2) / (1-r^2))
                chk <- pt(tt, n-2, lower.tail=FALSE)
                if (chk < 0.05) {
                    eig <- eigen(var(cbind(Xhat[, i], Xhat[, j]), na.rm=TRUE))
                    tmp <- (-1) * eig$vectors[1, 2] / eig$vectors[2, 2]
                    gamma[i, j] <- tmp
                }
            }
        }
        gamma[gamma <= 0] <- 1
        upper <- upper.tri(gamma)
        ind.upper <- which(upper, arr.ind=TRUE)

        design <- matrix(0, ncol=nCol, nrow=nrow(ind.upper))
        for (i in seq_len(nrow(ind.upper))) {
            design[i, ind.upper[i, 1]] <- -1
            design[i, ind.upper[i, 2]] <- 1
        }

        loggamma <- log(gamma[upper])

        newrow <- rep((1 / nCol), nCol)
        nonsingular <- rbind(newrow, design)
        lestimateMean <- qr.solve(nonsingular, c(0, loggamma))

        estimate1 <- exp(lestimateMean)
    }


    gamma <- estimateGamma(concs)
    temp <- sweep(concs, 2, gamma, FUN="/")

    normconcs <- sweep(temp, 1, rowMedian, FUN="-")
    ## Store method-specific info in "normalization" attribute
    attr(normconcs, "normalization") <- list(gamma=gamma)

    return(normconcs)
}


##
## Public Methods
##

##-----------------------------------------------------------------------------
## Returns normalization method associated with key for invocation.
getRegisteredNormalizationMethod <- function(key) {
    return(method <- getRegisteredMethod(key, envir=normenv())$method)
}


##-----------------------------------------------------------------------------
## Returns label associated with key for display by user interface.
getRegisteredNormalizationMethodLabel <- function(key) {
    return(ui.label <- getRegisteredMethod(key, envir=normenv())$ui.label)
}


##-----------------------------------------------------------------------------
## Returns vector containing "keys" for all registered normalization methods.
getRegisteredNormalizationMethodKeys <- function() {
    keys <- getRegisteredMethodKeys(envir=normenv())
    if (length(keys) == 0) {
        stop("no registered normalization methods exist")
    }

    return(keys)
}


##-----------------------------------------------------------------------------
## Registers specific normalization method for use by normalize() method.
registerNormalizationMethod <- function(key,
                                        method,
                                        ui.label=names(key)) {
    if (is.null(ui.label)) {
        ui.label <- key
    }
    ui.label <- as.character(ui.label)[1]

    registerMethod(key, method, ui.label=ui.label, envir=normenv())
}


##-----------------------------------------------------------------------------
## Performs normalization for sample loading after quantification.
## It has two required input values:
##   1) the data matrix with samples in the rows and antibodies in the columns.
##   2) the name of the method of sample loading normalization. This argument
##      may be augmented with user-provided normalization methods.
##      Package-provided values are:
##
##      median - the sample median (row median) is subtracted from each sample
##      house  - housekeeping normalization. The median of a housekeeping
##               antibody or set of housekeeping antibodies are used. The
##               names of the antibodies to be used must be supplied as
##               a named argument to this method.
##      vs     - variable slope normalization. Here the sample median
##               is used along with a multiplicative gamma.
##

## :KRC: Should be called something different; there are "normalize"
## functions for every technology in the universe. The name should at
## least include "rppa" somewhere.

normalize <- function(concs,
                      method=getRegisteredNormalizationMethodKeys(),
                      ...) {
    ## Check arguments
    if (is.RPPASet(concs)) {
        ## Assemble matrix of concentrations from all fits in object
        concs <- .fitSlot(concs, "concentrations")
    }

    if (!(is.matrix(concs) || is.data.frame(concs))) {
        stop(sprintf("argument %s must be matrix-like",
                     sQuote("concs")))
    }

    methodLabel <- method <- match.arg(method)

    ## Begin processsing
    rowMedian <- apply(concs, 1, median, na.rm=TRUE)
    colMedian <- apply(concs, 2, median, na.rm=TRUE)

    ## Create environment for method to use
    env <- new.env(hash=TRUE)
    env$rowMedian <- rowMedian
    env$colMedian <- colMedian

    ## Apply environment to method
    method <- getRegisteredNormalizationMethod(method)
    environment(method) <- env

    ## Invoke method
    normconcs <- method(concs=sweep(concs, 2, colMedian, FUN="-"),
                        ...)

    ## Store processing info in "normalization" attribute
    attr(normconcs, "normalization") <- c(list(method=methodLabel,
                                               rowMedian=rowMedian,
                                               colMedian=colMedian),
                                          attr(normconcs, "normalization"))

    return(normconcs)
}


##
## Initialization
##
## :TODO: Migrate following to .onLoad since registration should occur once
registerNormalizationMethod("median", normalize.median)
registerNormalizationMethod("house", normalize.house, "Housekeeping")
registerNormalizationMethod("vs", normalize.vs, "Variable Slope")

