###
### VSN.R
###


##-----------------------------------------------------------------------------
## Performs normalization for sample loading after quantification.
## It has two required input values: 1) the data matrix with samples
## in the rows and proteins in the columns. 2) the method of sample
## loading normalization:
##   median - the sample median (row median) is subtracted from
##            each sample
##   house  - housekeeping normalization. The median of a housekeeping
##            protein or set of housekeeping proteins are used. The
##            name of the protein(s) to be used must also be supplied.
##   vs     - variable slope normalization. Here the sample median
##            is used along with a multiplicate gamma
normalize <- function(concs,
                      method=c("median", "house", "vs"),
                      protein=NULL) {
    ## Check arguments
    if (is.RPPASet(concs)) {
        ## :TODO: assemble matrix of concentrations from all fits in object
    }
    if (!(is.matrix(concs) || is.data.frame(concs))) {
        stop(sprintf("argument %s must be matrix-like",
                     sQuote("concs")))
    }

    if (!(is.character(protein))) {
        stop(sprintf("argument %s must be character",
                     sQuote("protein")))
    } else {
        ## :TODO: verify any protein named in argument is column of 'concs'
    }

    method <- match.arg(method)

    ## Estimates the multiplicative gamma terms from variable slope
    ## normalization. It takes as input the data matrix (with samples in
    ## the rows and proteins in the columns). It is assumed that this matrix
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
                if (chk < .05) {
                    eig <- eigen(var(cbind(Xhat[, i], Xhat[, j]), na.rm=TRUE))
                    tmp <- (-1) * eig$vectors[1, 2] / eig$vectors[2, 2]
                    gamma[i, j] <- tmp
                }
            }
        }
        gamma[gamma<=0] <- 1
        upper <- upper.tri(gamma)
        ind <- which(upper, arr.ind=TRUE)

        design <- matrix(0, ncol=nCol, nrow=nrow(ind))
        for (i in seq(1, nrow(ind))) {
            design[i, ind[i, 1]] <- -1
            design[i, ind[i, 2]] <- 1
        }

        loggamma <- log(gamma[upper])

        newrow <- rep((1/nCol), nCol)
        nonsingular <- rbind(newrow, design)
        lestimateMean <- qr.solve(nonsingular, c(0, loggamma))

        estimate1 <- exp(lestimateMean)
        val <- estimate1
    }


    ## Begin processsing
    rowMedian <- apply(concs, 1, median, na.rm=TRUE)
    colMedian <- apply(concs, 2, median, na.rm=TRUE)

    concs <- sweep(concs, 2, colMedian)
    gamma <- NULL
    houseMedian <- NULL

    dataNorm <- switch(EXPR=method,
                       median = sweep(concs, 1, rowMedian),
                       house  = {
                                    ## housekeeping normalization
                                    houseMedian <- apply(concs[, protein],
                                                         1,
                                                         median,
                                                         na.rm=TRUE)
                                    sweep(concs, 1, houseMedian, "-")
                                },
                       vs     = {
                                    ## variable slope normalization
                                    gamma <- estimateGamma(concs)
                                    temp <- sweep(concs, 2, gamma, "/")
                                    sweep(temp, 1, rowMedian, "-")
                                },
                       stop(sprintf("unrecognized normalization method %s",
                                    sQuote(method))))

    list(dataNorm=dataNorm,
         rowMedian=rowMedian,
         colMedian=colMedian,
         gamma=gamma,
         houseMedian=houseMedian)
}

