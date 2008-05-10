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
    temp <- x@data
    my <- max(temp$Main.Row) * max(temp$Sub.Row)
    mx <- max(temp$Main.Col) * max(temp$Sub.Col)
    yspot <- 1+my-(max(temp$Sub.Row)*(temp$Main.Row-1) + temp$Sub.Row)
    xspot <- max(temp$Sub.Col)*(temp$Main.Col-1) + temp$Sub.Col
    geo <- tapply(temp[,measure], list(xspot, yspot), mean)
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
    abline(h = 0.5 + seq(0, my, length=1+max(temp$Main.Row)))
    abline(v = 0.5 + seq(0, mx, length=1+max(temp$Main.Col)))
    invisible(x)
}

setMethod("image", "RPPA", image.RPPA)


##-----------------------------------------------------------------------------
setMethod("image", "RPPADesign",
          function(x, ...) {
    # figure out how to make "geographic" pictures
    temp <- x@layout
    my <- max(temp$Main.Row) * max(temp$Sub.Row)
    mx <- max(temp$Main.Col) * max(temp$Sub.Col)
    yspot <- 1+my-(max(temp$Sub.Row)*(temp$Main.Row-1) + temp$Sub.Row)
    xspot <- max(temp$Sub.Col)*(temp$Main.Col-1) + temp$Sub.Col
    geo.steps <- tapply(temp$Steps, list(xspot, yspot), mean)
    image(1:mx, 1:my, geo.steps, ...)
    abline(h = 0.5 + seq(0, my, length=1+max(temp$Main.Row)))
    abline(v = 0.5 + seq(0, mx, length=1+max(temp$Main.Col)))
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
    measure <- match.arg(measure)
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

