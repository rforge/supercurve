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
    geo <- tapply(data.df[,measure], list(xspot, yspot), mean)
    if (colorbar) {
        layout(matrix(c(2, 1), nrow=1), widths=c(10,1))
        opar <- par(mai=c(1, 0.5, 1, 0.1))
        on.exit(par(opar))
        on.exit(layout(1))
        image(1,
              seq(min(geo, na.rm=TRUE),
                  max(geo, na.rm=TRUE),
                  length=256),
              matrix(1:256, nrow=1),
              col=col,
              xaxt='n',
              xlab='',
              ylab='')
    }
    image(1:mx, 1:my, geo, main=main, col=col, ...)
    abline(h = 0.5 + seq(0, my, length=1+max(data.df$Main.Row)))
    abline(v = 0.5 + seq(0, mx, length=1+max(data.df$Main.Col)))
    invisible(x)
}

setMethod("image", "RPPA", image.RPPA)


##-----------------------------------------------------------------------------
setMethod("image", "RPPADesign",
          function(x, ...) {
    # figure out how to make "geographic" pictures
    data.df <- x@layout
    my <- max(data.df$Main.Row) * max(data.df$Sub.Row)
    mx <- max(data.df$Main.Col) * max(data.df$Sub.Col)
    yspot <- 1+my-(max(data.df$Sub.Row)*(data.df$Main.Row-1) + data.df$Sub.Row)
    xspot <- max(data.df$Sub.Col)*(data.df$Main.Col-1) + data.df$Sub.Col
    geo.steps <- tapply(data.df$Steps, list(xspot, yspot), mean)
    image(1:mx, 1:my, geo.steps, ...)
    abline(h = 0.5 + seq(0, my, length=1+max(data.df$Main.Row)))
    abline(v = 0.5 + seq(0, mx, length=1+max(data.df$Main.Col)))
    invisible(geo.steps)
})


##-----------------------------------------------------------------------------
# The image function gives a geographic plot of some measure computed from
# the fit. Default is to image the (raw) residuals, with options for other
# forms of the residuals or for the fitted concentrations (X) or intensities (Y).

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
    rppa@data$Residuals <- resid(x)
    rppa@data$StdRes <- resid(x, "standardized")
    rppa@data$ResidualsR2 <- resid(x, "r2")
    rppa@data$X = fitted(x, "X")
    rppa@data$Y = fitted(x, "Y")
    image.RPPA(rppa, measure=measure, ...)

    invisible(x)
}

setMethod("image", "RPPAFit",
          image.RPPAFit)

