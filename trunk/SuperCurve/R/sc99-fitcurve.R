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
    designparams <- settings@designparams
    fitparams <- settings@fitparams
                              
    ## Perform analysis
    fitset <- RPPASet(txtdir, designparams, fitparams)

    write.summary(fitset,
                  path=outdir,
                  graphs=TRUE,
                  tiffdir=imgdir)
}

