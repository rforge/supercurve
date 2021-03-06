###
### $Id$
###


##=============================================================================
setClass("RPPA",
         representation(data="data.frame",
                        file="character",
                        antibody="character"))


##-----------------------------------------------------------------------------
is.RPPA <- function(x) {
    is(x, "RPPA")
}


##-----------------------------------------------------------------------------
## Generates an RPPA object from a quantification file.
RPPA <- function(file,
                 path=".",
                 antibody=NULL,
                 software="microvigene",
                 alt.layout=NULL) {
    ## Check arguments
    if (is.character(file)) {
      #:KRC: user-hostile.
        if (!(length(file) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("file")))
        } else if (!nzchar(file)) {
            stop(sprintf("argument %s must not be empty string",
                         sQuote("file")))
        }

        path_or_url <- if (.isAbsolutePathname(file) || .hasScheme(file)) {
                           file
                       } else {
                           if (!is.character(path)) {
                               stop(sprintf("argument %s must be character",
                                            sQuote("path")))
                           } else if (!(length(path) == 1)) {
                               stop(sprintf("argument %s must be of length 1",
                                            sQuote("path")))
                           }

                           if (.hasScheme(path)) {
                               paste(path, file, sep="/")
                           } else {
                               file.path(path, file)
                           }
                       }

        is_url <- .hasScheme(path_or_url)
        if (!is_url) {
            #:KRC: Useless error checking that does not save you
            ## :PLR: But has a more meaningful error message...
            if (!file.exists(path_or_url)) {
                stop(sprintf("file %s does not exist",
                             dQuote(path_or_url)))
            }
        }

        ## Convert to connection object
        file <- if (is_url) {
                    url(path_or_url, "r")
                } else {
                    file(path_or_url, "r")
                }
        on.exit(close(file))
    }
    filename <- basename(summary(file)$description)

    if (!is.null(antibody)) {
      #:KRC: usual user-hostile error checking...
        if (!is.character(antibody)) {
            stop(sprintf("argument %s must be character",
                         sQuote("antibody")))
        } else if (!(length(antibody) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("antibody")))
        } else if (!nzchar(antibody)) {
            stop(sprintf("argument %s must not be empty string",
                         sQuote("antibody")))
        }
    } else {
        ## Use filename without extension as default value
        txt.re <- "\\.[tT][xX][tT]$"
        basename <- sub(txt.re, "", filename)
        antibody <- sub("[[:space:]]+$", "", basename)
    }

    ## Read quantification file
    quant.df <- readQuantification(file, software, alt.layout)

    ## Create new class
    new("RPPA",
        data=quant.df,
        file=filename,
        antibody=antibody)
}


##-----------------------------------------------------------------------------
setMethod("dim", signature(x="RPPA"),
          function(x) {
    .dimOfLayout(x@data)
})


##-----------------------------------------------------------------------------
setMethod("summary", signature(object="RPPA"),
          function(object,
                   ...) {
    cat(sprintf("An %s object loaded from file %s",
                class(object), dQuote(object@file)), "\n")
    cat("antibody:", object@antibody, "\n")
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
                   main=.mkPlotTitle(measure, x@antibody),
                   colorbar=FALSE,
                   col=terrain.colors(256),
                   ...) {
    ## Check arguments
    if (!is.character(measure)) {
      #:KRC: sigh. user-hostile.
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
      #:KRC: guess...
        stop(sprintf("argument %s must be character",
                     sQuote("main")))
    } else if (!(length(main) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("main")))
    }

    # the whole thing should just use "try(as.logical())" and, if it
    # works, live with the result....
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
          z=geo,
          col=col,
          main=main,
          sub=paste("File:", x@file),
          xaxt="n",
          xlab="",
          yaxt="n",
          ylab="",
          ...)

    at.x <- seq(from=dim.rppa["Sub.Col"],
                to=dim.rppa["Sub.Col"]*dim.rppa["Main.Col"],
                by=dim.rppa["Sub.Col"])
    at.y <- seq(from=dim.rppa["Sub.Row"],
                to=dim.rppa["Sub.Row"]*dim.rppa["Main.Row"],
                by=dim.rppa["Sub.Row"])
    axis(1, at=at.x)
    axis(2, at=at.y)

    abline(h=(0.5 + seq(0, my, length=1+dim.rppa["Main.Row"])))
    abline(v=(0.5 + seq(0, mx, length=1+dim.rppa["Main.Col"])))
    invisible(x)
})


##-----------------------------------------------------------------------------
## Retrieve the value for 'software' used to create this object.
software <- function(rppa) {
    stopifnot(is.RPPA(rppa))

    attr(rppa@data, "software", exact=TRUE)
}


##-----------------------------------------------------------------------------
## Retrieve the layout used to create this object. In this case, a non-NULL
## value indicates an alternative layout was used.
layout <- function(rppa) {
    stopifnot(is.RPPA(rppa))

    attr(rppa@data, "layout", exact=TRUE)
}

