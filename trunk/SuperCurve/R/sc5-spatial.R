###
### SPATIAL.R
###


##-----------------------------------------------------------------------------
.computeBackgroundCutoff <- function(mydata, measure, cutoff) {
    ## Check arguments
    stopifnot(is.data.frame(mydata))
    stopifnot(is.character(measure) && length(measure) == 1)
    stopifnot(is.numeric(cutoff) && length(cutoff) == 1)

    ## Begin processing

    ## Find all negative controls
    negcon <- with(mydata, SpotType == "Blank" |
                           SpotType == "Buffer" |
                           SpotType == "NegCtrl")

    ## Identify noise region
    bg <- if (any(negcon)) {
              mydata[[measure]][negcon]
          } else {
              ## Use background to compute noise region
              mydata$Mean.Total - mydata$Mean.Net
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

    return(bgCut)
}


##-----------------------------------------------------------------------------
.getFitMeasure <- function(rppa) {
    ## Check arguments
    stopifnot(is.RPPA(rppa))

    ## Begin processing
    fitMeasures <- c("Mean.Net", "Mean.Total", "Median.Net", "Median.Total")

    return(fitMeasures[fitMeasures %in% colnames(rppa@data)])
}


##-----------------------------------------------------------------------------
spatialCorrection <- function(rppa,
                              design,
                              measure=c("Mean.Net", "Mean.Total"),
                              cutoff=0.8,
                              k=100,
                              gamma=0.1,
                              plotSurface=FALSE) {
    ## Check arguments
    if (!is.RPPA(rppa)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("rppa"), "RPPA"))
    } else if ("Spatial.Norm" %in% colnames(rppa@data)) {
        stop(sprintf("argument %s has already been spatially corrected",
                     sQuote("rppa")))
    }

    if (!is.RPPADesign(design)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("design"), "RPPADesign"))
    } else {
        reqdNames <- c("SpotType", "Dilution")
        if (!(all(reqdNames %in% colnames(design@layout)))) {
            message(sprintf("package %s can be used to generate the missing information in the %s object required to use this method",
                            sQuote("slideDesignerGUI"),
                            class(design)))
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
        stop(sprintf("dim of argument %s (%s) must match that of argument %s (%s)",
                     sQuote("rppa"),
                     paste(dim.rppa, collapse="x"),
                     sQuote("design"),
                     paste(dim.design, collapse="x")))
    }

    ## :TBD: Technically, "Median.Net" and "Median.Total" are legal for this
    ## routine if the RPPA contains these columns. Allow them?
    ## If so, measure argument would become function passing rppa argument
    ## returning potential fit column measures.

    measure <- match.arg(measure)

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

    if (!is.logical(plotSurface)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("plotSurface")))
    } else if (!(length(plotSurface) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("plotSurface")))
    }


    ## Begin processing
    if (!require(mgcv)) {
        stop(sprintf("%s package required for fitting the GAM in the %s method",
                     sQuote("mgcv"), sQuote("spatialNorm")))
    }

    ## Set up the row and column variables
    mydata <- rppa@data
    mydata$Row <- with(mydata, (Main.Row-1)*max(Sub.Row) + Sub.Row)
    mydata$Col <- with(mydata, (Main.Col-1)*max(Sub.Col) + Sub.Col)

    ## Create data frame with row/column indices used for predicting surface
    pd <- data.frame(Row=mydata$Row, Col=mydata$Col)

    ## Borrow SpotType and Dilution information from design
    layout <- design@layout
    mydata <- merge(mydata, layout, by=c(.locationColnames(), "Sample"))

    ## :NOTE: This code seems to assume that only one positive control
    ## series exists on a slide. If multiple exist, they would seem to
    ## be blended together by this method, which may not be desirable.

    ## Compute background cutoff
    bgCut <- .computeBackgroundCutoff(mydata, measure, cutoff)

    ## Remove positive controls less than computed background cutoff
    poscon <- mydata$SpotType == "PosCtrl"
    is.na(mydata[poscon, measure]) <- mydata[poscon, measure] < bgCut
    positives <- mydata[poscon, ]

    ## Determine positive control dilutions
    dilutions <- sort(unique(positives$Dilution), decreasing=TRUE)
    ndilutions <- length(dilutions)

    ## Create surface names
    surfaces <- sapply(dilutions,
                       function(dilution) {
                           paste("surface", dilution, sep="")
                       })

    ## Fits a generalized additive model to estimate a surface
    ## from positive controls
    for (dilution in dilutions) {
        #pcsub <- positives[positives$Dilution == dilution, ]
        pcsub <- subset(positives, Dilution == dilution, drop=FALSE)

        ## Make choice of k robust in case that the number of
        ## available spots is less than k (YH).
        adjK <- k
        spotCount <- sum(!is.na(pcsub[, measure]))
        if (spotCount < k) {
            adjK <- round(spotCount / 3)  ## arbitrary magic number
        }

        b1 <- gam(Mean.Net ~ s(Row, Col, bs="ts", k=adjK),
                  data=pcsub,
                  gamma=gamma)
        surface <- paste("surface", dilution, sep="")
        assign(surface, predict.gam(b1, newdata=pd))
    }

    ## Plot the different surfaces
    ## :TBD: What's the best way to organize various levels of surface plots?
    if (plotSurface) {
        temprppa <- rppa
        par(ask=TRUE)
        for (surface in surfaces) {
            temprppa@data[, surface] <- eval(as.name(surface))
            image(temprppa,
                  colorbar=TRUE,
                  measure=surface,
                  xlab="",
                  ylab="")
        }
        par(ask=FALSE)
    }

    ## Constrain the surfaces so they do not cross
    if (ndilutions > 1) {
        for (i in seq(2, ndilutions)) {
            surface <- surfaces[i]
            prev.surface <- surfaces[i-1]
            s2 <- eval(as.name(surface))
            s1 <- eval(as.name(prev.surface))
            s2[s1 < s2] <- s1[s1 < s2] - 0.5
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


    ##-------------------------------------------------------------------------
    ## Scales measurement using values of surface
    scaleBySurface <- function(x, surface, n=1) {
        stopifnot(is.numeric(x))
        stopifnot(is.character(surface))
        stopifnot(is.numeric(n))

        s1 <- eval.parent(as.name(surface), n=n)
        stopifnot(is.numeric(s1))
        (x / s1) * median(s1)
    }


    adjustment <- if (ndilutions > 1) {
                      ## Organize matrix for input into "which.bin" function
                      input.mat <- as.matrix(rppa@data[, measure])
                      for (surface in surfaces) {
                          s1 <- eval(as.name(surface))
                          input.mat <- cbind(input.mat, s1)
                      }
                      dimnames(input.mat) <- list(NULL,
                                                  c("measure", surfaces))

                      ## Find closest positive control surface and fraction
                      ## between two surfaces
                      place <- apply(input.mat, 1, which.bin)
                      values.mat <- cbind(place, input.mat)
                      rownames(values.mat) <- NULL
                      p <- apply(values.mat, 1, getp)

                      ## Perform scaling on each positive control surface
                      x <- rppa@data[, measure]
                      adj <- sapply(surfaces, scaleBySurface, x=x, n=3)
                      dimnames(adj) <- list(NULL, 
                                            paste("adj", dilutions, sep=""))

                      ## Now retrieve appropriate adjustment based on the
                      ## closest positive control surface and the fraction
                      ## between two surfaces as computed by "getadj" function.
                      adjust.mat <- cbind(place, p, adj)
                      rownames(adjust.mat) <- NULL
                      apply(adjust.mat, 1, getadj)
                  } else {
                      x <- rppa@data[, measure]
                      as.numeric(scaleBySurface(x, surfaces[1]))
                  }

## :TODO: Rename new measurement column as?
    rppa@data$Spatial.Norm <- adjustment

    return(rppa)
}

