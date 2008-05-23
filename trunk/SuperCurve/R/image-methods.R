###
### IMAGE-METHODS.R
###


##-----------------------------------------------------------------------------
image.RPPA <- function(x,
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
    }

    if (!is.character(main)) {
        stop(sprintf("argument %s must be character",
                     sQuote("main")))
    } else if (!(length(main) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("main")))
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
    my <- max(data.df$Main.Row) * max(data.df$Sub.Row)
    mx <- max(data.df$Main.Col) * max(data.df$Sub.Col)
    yspot <- 1+my-(max(data.df$Sub.Row)*(data.df$Main.Row-1) + data.df$Sub.Row)
    xspot <- max(data.df$Sub.Col)*(data.df$Main.Col-1) + data.df$Sub.Col
    geo <- tapply(data.df[, measure],
                  list(xspot, yspot),
                  mean)
    if (colorbar) {
        ## get the size of the plotting region in relative units
        startPlt <- par()$plt

        ## We're only going to partition things on the x-axis, so only
        ## the first 2 coordinates are of interest. Define the boundaries
        ## for the two panels so that a 10/1 width ratio is attained.
        imagePlt    <- startPlt
        colorbarPlt <- startPlt
        startWidth  <- startPlt[2] - startPlt[1]

        imagePlt[2]    <- startPlt[1] + (10/12)*startWidth
        colorbarPlt[1] <- startPlt[2] - ( 1/12)*startWidth

        ## draw the colorbar
        par(plt=colorbarPlt)
        image(1,
              seq(min(geo, na.rm=TRUE),
                  max(geo, na.rm=TRUE),
                  length=256),
              matrix(1:256, nrow=1),
              col=col,
              xaxt="n",
              xlab="",
              yaxt="n",
              ylab="")
        axis(4) # put labeling at right
        box()

        ## set things up to draw the main image and
        ## revert back for the next figure
        par(plt=imagePlt, new=TRUE)
        on.exit(par(plt=startPlt))
    }
    image(1:mx,
          1:my,
          geo,
          col=col,
          main=main,
          ...)
    abline(h=(0.5 + seq(0, my, length=1+max(data.df$Main.Row))))
    abline(v=(0.5 + seq(0, mx, length=1+max(data.df$Main.Col))))
    invisible(x)
}

## :TODO: Figure out how to combine with above and remove S3 definition
setMethod("image", signature(x="RPPA"),
          image.RPPA)


##-----------------------------------------------------------------------------
setMethod("image", signature(x="RPPADesign"),
          function(x,
                   ...) {
    ## figure out how to make "geographic" pictures
    data.df <- x@layout
    my <- max(data.df$Main.Row) * max(data.df$Sub.Row)
    mx <- max(data.df$Main.Col) * max(data.df$Sub.Col)
    yspot <- 1+my-(max(data.df$Sub.Row)*(data.df$Main.Row-1) + data.df$Sub.Row)
    xspot <- max(data.df$Sub.Col)*(data.df$Main.Col-1) + data.df$Sub.Col
    geo.steps <- tapply(data.df$Steps,
                        list(xspot, yspot),
                        mean)
    image(1:mx,
          1:my,
          geo.steps,
          ...)
    abline(h=(0.5 + seq(0, my, length=1+max(data.df$Main.Row))))
    abline(v=(0.5 + seq(0, mx, length=1+max(data.df$Main.Col))))
    invisible(geo.steps)
})


##-----------------------------------------------------------------------------
## Provides a geographic plot of some measure computed from the fit.
## Default is to image the (raw) residuals, with options for other forms
## of the residuals or for the fitted concentrations (X) or intensities (Y).
image.RPPAFit <- function(x,
                          measure=c("Residuals",
                                    "ResidualsR2",
                                    "StdRes",
                                    "X",
                                    "Y"),
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

    ## :TBD: Should this invoke callNextMethod instead (when S4 method)?
    ## :TBD: What should axis labels be?
    ## :TBD: Shouldn't main reference file as well?
    image(rppa,
          measure=measure,
          ...)

    invisible(x)
}

## :TODO: Figure out how to combine with above and remove S3 definition
setMethod("image", signature(x="RPPAFit"),
          image.RPPAFit)


