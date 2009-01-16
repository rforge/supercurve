###
### RPPA.R
###


##=============================================================================
setClass("RPPA",
         representation=list(data="data.frame",
                             file="character"))


##-----------------------------------------------------------------------------
is.RPPA <- function(x) {
    inherits(x, "RPPA")
}


##-----------------------------------------------------------------------------
## Generates an RPPA object from a quantification file.
RPPA <- function(file,
                 path=".",
                 software="microvigene") {
    ## Check arguments
    if (is.character(file)) {
        if (!(length(file) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("file")))
        } else if (!nzchar(file)) {
            stop(sprintf("argument %s must not be empty string",
                         sQuote("file")))
        }

        pathname <- if (.isAbsolutePathname(file)) {
                        file
                    } else {
                        if (!is.character(path)) {
                            stop(sprintf("argument %s must be character",
                             sQuote("path")))
                        } else if (!(length(path) == 1)) {
                            stop(sprintf("argument %s must be of length 1",
                             sQuote("path")))
                        }

                        file.path(path, file)
                    }

        if (!file.exists(pathname)) {
            stop(sprintf("file %s does not exist",
                         dQuote(pathname)))
        }

        ## Convert to connection object
        file <- file(pathname, "r")
        on.exit(close(file))
    }
    filename <- basename(summary(file)$description)

    ## Read quantification file
    quant.df <- readQuantification(file, software)

    ## Create new class
    new("RPPA",
        data=quant.df,
        file=filename))
}


##-----------------------------------------------------------------------------
setMethod("dim", signature="RPPA",
          function(x) {
    .dimOfLayout(x@data)
})


##-----------------------------------------------------------------------------
setMethod("summary", "RPPA",
          function(object,
                   ...) {
    cat(sprintf("An %s object loaded from file %s",
                class(object), dQuote(object@file)), "\n")
    cat("\n")
    print(dim(object))
    cat("\n")
    unneededColnames <- c(.locationColnames(), "Sample")
    summarizable <- !colnames(object@data) %in% unneededColnames
    print(summary(object@data[summarizable]))
})


##-----------------------------------------------------------------------------
setMethod("image", signature(x="RPPA"),
          function(x,
                   measure="Mean.Net",
                   main=measure,
                   colorbar=FALSE,
                   col=terrain.colors(256),
                   ...) {
    ## Check arguments
    if (!is.character(measure)) {
        stop(sprintf("argument %s must be character",
                     sQuote("measure")))
    } else if (!(length(measure) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("measure")))
    } else if (!(measure %in% colnames(x@data))) {
        stop(sprintf("invalid measure %s",
                     sQuote(measure)))
    }

    if (!is.character(main)) {
        stop(sprintf("argument %s must be character",
                     sQuote("main")))
    } else if (!(length(main) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("main")))
    }

    if (is.numeric(colorbar)) {
        colorbar <- as.logical(colorbar)
    }

    if (!is.logical(colorbar)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("colorbar")))
    } else if (!(length(colorbar) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("colorbar")))
    }

    ## Begin processing
    data.df <- x@data
    dim.rppa <- dim(x)
    my <- dim.rppa["Main.Row"] * dim.rppa["Sub.Row"]
    mx <- dim.rppa["Main.Col"] * dim.rppa["Sub.Col"]
    yspot <- 1+my-(max(data.df$Sub.Row)*(data.df$Main.Row-1) + data.df$Sub.Row)
    xspot <- max(data.df$Sub.Col)*(data.df$Main.Col-1) + data.df$Sub.Col
    geo <- tapply(data.df[, measure],
                  list(xspot, yspot),
                  mean)
    if (colorbar) {
        ## Get the size of the plotting region in relative units
        startPlt <- par()$plt

        ## We're only going to partition things on the x-axis, so only
        ## the first 2 coordinates are of interest. Define the boundaries
        ## for the two panels so that a 10/1 width ratio is attained.
        imagePlt    <- startPlt
        colorbarPlt <- startPlt
        startWidth  <- startPlt[2] - startPlt[1]

        imagePlt[2]    <- startPlt[1] + (10/12)*startWidth
        colorbarPlt[1] <- startPlt[2] - ( 1/12)*startWidth

        ## Draw the colorbar
        ## :TODO: Figure out how to set margins so it works in small windows...
        par(plt=colorbarPlt)
        image(1,
              seq(min(geo, na.rm=TRUE),
                  max(geo, na.rm=TRUE),
                  length=256),
              matrix(seq_len(256), nrow=1),
              col=col,
              xaxt="n",
              xlab="",
              yaxt="n",
              ylab="")
        axis(4) # Put labeling at right
        box()

        ## Set things up to draw main image and revert back for next figure
        par(plt=imagePlt, new=TRUE)
        on.exit(par(plt=startPlt))
    }
    image(seq_len(mx),
          seq_len(my),
          geo,
          col=col,
          main=main,
          ...)
    abline(h=(0.5 + seq(0, my, length=1+max(data.df$Main.Row))))
    abline(v=(0.5 + seq(0, mx, length=1+max(data.df$Main.Col))))
    invisible(x)
})

