###
### FITCURVE.R
###


##-----------------------------------------------------------------------------
fitCurveAndSummarizeFromSettings <- function(settings) {
    ## Check arguments
    if (!is.SuperCurveSettings(settings)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("settings"), "SuperCurveSettings"))
    }

###
### :TODO: Find out how best to do this. Meantime, die loudly...
###
    stopifnot(validObject(settings))


    ## Begin processing
    txtdir <- as(settings@txtdir, "character")
    imgdir <- as(settings@imgdir, "character")
    outdir <- as(settings@outdir, "character")

    rppasetArgs <- list(path=txtdir,
                        designparams=settings@designparams,
                        fitparams=settings@fitparams)
    ## :NOTE: Handle following after list construction so NULL values dropped
    rppasetArgs$antibodyfile <- settings@antibodyfile
    rppasetArgs$software <- settings@software

    ## Perform analysis
    fitset <- do.call(RPPASet, rppasetArgs)

    write.summary(fitset,
                  path=outdir,
                  graphs=TRUE,
                  tiffdir=imgdir)
}

