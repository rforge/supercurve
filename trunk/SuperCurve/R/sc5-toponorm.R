###
### TOPONORM.R
###


##-----------------------------------------------------------------------------
spatialNorm <- function(rppa,
                        design,
                        measure="Mean.Net",
                        cutoff=0.8,
                        k=100,
                        gamma=0.1,
                        plot.surface=FALSE) {
    ## Check arguments
    if (!is.RPPA(rppa)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("rppa"), "RPPA"))
    }

    if (!is.RPPADesign(design)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("design"), "RPPADesign"))
    }

    if (!is.character(measure)) {
        stop(sprintf("argument %s must be character",
                     sQuote("measure")))
    } else if (!(length(measure) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("measure")))
    } else if (!(measure %in% colnames(rppa@data))) {
        stop(sprintf("invalid measure %s",
                     sQuote(measure)))
    }

    if (!is.numeric(cutoff)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("cutoff")))
    } else if (!(length(cutoff) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("cutoff")))
    } else if (!(cutoff >= 0 && cutoff <= 1)) {
        stop(sprintf("argument %s must be in interval [%d, %d]",
                     sQuote("cutoff"), 0, 1))
    }

    ## :TBD: What is valid range of values? [passed directly to mgcv::s()]
    ## Just pass through - should be >=2, -1 (calculated), or large
    if (!is.numeric(k)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("k")))
    } else if (!(length(k) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("k")))
    }

    ## Valid range is probably [0..2] [passed directly to mgcv::gam()]
    if (!is.numeric(gamma)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("gamma")))
    } else if (!(length(gamma) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("gamma")))
    } else if (!(gamma > 0)) {
        stop(sprintf("argument %s must be a positive quantity",
                     sQuote("gamma")))
    }

    if (!is.logical(plot.surface)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("plot.surface")))
    } else if (!(length(plot.surface) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("plot.surface")))
    }

    ## Begin processing

    ## :TBD: Add required 'mgcv' package to Suggests or Depends?
    ## :TBD: What exactly do we use from this package?
    if (!require(mgcv)) {
        stop(sprintf("%s package required for fitting the GAM in the %s method",
                     sQuote("mgcv"), sQuote("spatialNorm")))
    }

    ## Set up the row and column variables
    mydata <- rppa@data
    mydata$Row <- (mydata$Main.Row-1)*max(mydata$Sub.Row) + mydata$Sub.Row
    mydata$Col <- (mydata$Main.Col-1)*max(mydata$Sub.Col) + mydata$Sub.Col

    ## Create data frame with row/column indices used for predicting surface
    pd <- data.frame(Row=mydata$Row, Col=mydata$Col)

    ## Borrow SpotType and Dilution information from design
    layout <- design@layout
    newdata <- merge(mydata, layout, by=c(.locationColnames(), "Sample"))

    ## Find all negative controls
    negcon <- with(newdata, SpotType == "Blank" |
                            SpotType == "Buffer" |
                            SpotType == "NegCtrl")

    ## Identify noise region
    bg <- if (sum(negcon) == 0) {
              ## Use background to compute noise region
              mydata$Mean.Total - mydata$Mean.Net
          } else {
              newdata$Mean.Net[negcon]
          }

    ## Compute background cutoff using the quantile of 'cutoff' argument
    bgCut <- quantile(bg, cutoff)

    ## If computed background cutoff too low, use a larger quartile
    if (bgCut <= 100) {
        bgCut <- quantile(bg, 0.99)
        if (bgCut <= 100) {
            bgCut <- max(bg[-which.max(bg)])
        }
    }

    ## Remove positive controls less than computed background cutoff
    poscon <- newdata$SpotType == "PosCtrl"
    is.na(newdata[poscon, measure]) <- newdata[poscon, measure] < bgCut
    positives <- newdata[poscon, ]

    ## Determine positive control dilutions
    dilutions <- sort(unique(positives$Dilution), decreasing=TRUE)
    ndilut <- length(dilutions)

    ## Fits a generalized additive model to estimate a surface
    ## from positive controls
    for (i in dilutions) {
        pcsub <- positives
        pcsub <- pcsub[pcsub$Dilution == i, ]
        ## Make choice of k robust in case that the number of
        ## available spots is less than k (YH).
        adjK <- k
        spotCount <- sum(!is.na(pcsub[, measure]))
        if (spotCount < k) {
            adjK <- round(spotCount / 3)  ## :TODO: Magic # (3)
        }

        b1 <- gam(Mean.Net ~ s(Row, Col, bs="ts", k=adjK),
                  data=pcsub,
                  gamma=gamma)
        surface <- paste("surface", i, sep="")
        ## :TBD: Is assignment visible in global namespace?
        assign(surface, predict.gam(b1, newdata=pd))
    }

    ## Plot the different surfaces
    ## :TBD: What's the best way to organize various levels of surface plots?
    if (plot.surface) {
        temprppa <- rppa
        par(ask=TRUE)
        for (i in dilutions) {
            surface <- paste("surface", i, sep="")
            temprppa@data[, surface] <- eval(as.name(surface))
            ## :TODO: Annotate plot axes
            image(temprppa, surface, colorbar=TRUE)
        }
        par(ask=FALSE)
    }

    ## Constrain the surfaces so they do not cross
    if (ndilut > 1) {
        for (i in seq(2, ndilut)) {
            surface <- paste("surface", dilutions[i], sep="")
            prev.surface <- paste("surface", dilutions[i-1], sep="")
            s2 <- eval(as.name(surface))
            s1 <- eval(as.name(prev.surface))
            s2[s1 < s2] <- s1[s1 < s2] - 0.5
            ## :TBD: Is assignment visible in global namespace?
            assign(surface, s2)
        }
    }


    ##-------------------------------------------------------------------------
    ## :TODO: But what does method "do"?
    ## The "which.bin" function takes as input a vector
    ## whose first value is the measure to be corrected
    ## (either Mean.Net or Mean.Total) and the others
    ## values at each level of predicted surface.
    ## Returns the surface to which each spot is closest in intensity
    which.bin <- function(value) {
        stopifnot(is.numeric(value) && length(value) > 1)

        x <- value[1]
        vec <- value[-1]
        n <- length(vec)
        place <- NA
        for (i in seq_len(n-1)) {
            if (vec[i] > x && x > vec[i+1]) {
                place <- i
                break
            }

            if (is.na(place)) {
                if (x >= vec[1]) {
                    place <- 0
                } else if (x <= vec[n]) {
                    place <- n
                }
            }
        }

        return(place)
    }


    ##-------------------------------------------------------------------------
    ## :TODO: But what does method "do"?
    ## The "getp" function takes as input a vector
    ## whose first value is the output from the "which.bin" function,
    ## the second is the measure to be corrected
    ## (either Mean.Net or Mean.Total) and the others
    ## values at each level of predicted surface.
    ## :TODO: Reword the following...
    ## If the spot to be corrected falls between the two surfaces, the value
    ## returned is the fraction it falls between the two surfaces;
    ## otherwise, a 0 is returned.

    getp <- function(value) {
        stopifnot(is.numeric(value) && length(value) >= 3)

        item <- value[1]
        mn <- value[2]
        vec <- value[-(1:2)]
        n <- length(vec)

        p <- if (item != 0 && item != n) {
                (vec[item] - mn) / (vec[item] - vec[item+1])
             } else {
                 0
             }
    }


    ##-------------------------------------------------------------------------
    ## :TODO: But what does method "do"?
    ## The "getadj" function takes as input a vector
    ## whose first value is the output from the "which.bin" function,
    ## the second is output from the "getp" function and the third
    ## is the measure to be corrected
    ## (either Mean.Net or Mean.Total). The other values are the corrected
    ## measure values. If we want to correct the Mean.Net values of each
    ## spot, then the fourth column of the input matrix is the Mean.Net value
    ## scaled to the first positive control surface, the fifth is the
    ## Mean.Net values scaled to the half strength positive controls and so on.
    ## Returns the overall adjustment.
    getadj <- function(value) {
        stopifnot(is.numeric(value) && length(value) >= 3)

        item <- value[1]
        p <- value[2]
        vec <- value[-(1:2)]
        n <- length(vec)

        adj <- if (item == 0) {
                   vec[1]
               } else if (item == n) {
                   vec[n]
               } else {
                   vec[item]*(1-p) + vec[item+1]*p
               }
    }


    if (ndilut > 1) {
        ## Organize matrix for input into "which.bin" function
        mn <- rppa@data[, measure]
        surf <- mn
        for (i in dilutions) {
            surface <- paste("surface", i, sep="")
            x <- eval(as.name(surface))
            surf <- cbind(surf, x)
        }

        ## Find closest positive control surface and fraction
        ## between two surfaces
        place <- apply(surf, 1, which.bin)
        myvalue <- cbind(place, surf)
        p <- apply(myvalue, 1, getp)

        ## Perform scaling on each positive control surface
        adj <- matrix(NA, nrow=nrow(mydata), ncol=ndilut)
        for (i in seq_len(ndilut)) {
            x <- rppa@data[, measure]
            surface <- paste("surface", dilutions[i], sep="")
            s1 <- eval(as.name(surface))
            adj[, i] <- (x / s1) * median(s1)
        }

        ## Now retrieve appropriate adjustment based on the
        ## closest positive control surface and the fraction
        ## between two surfaces as computed by "getadj" function.
        myadjust <- cbind(place, p, adj)
        adjustment <- apply(myadjust, 1, getadj)
    } else {
        x <- rppa@data[, measure]
        surface <- paste("surface", dilutions, sep="")
        s1 <- eval(as.name(surface))
        adjustment <- (x / s1) * median(s1)
    }

    rppa@data$Spatial.Norm <- adjustment

    return(rppa)
}

