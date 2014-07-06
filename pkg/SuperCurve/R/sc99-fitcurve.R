###
### $Id$
###


##-----------------------------------------------------------------------------
fitCurveAndSummarizeFromSettings <- function(settings,
                                             monitor=NULL) {
    ## Check arguments
    if (!is.SuperCurveSettings(settings)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("settings"), "SuperCurveSettings"))
    }
    validObject(settings, complete=TRUE)  ## Invokes stop() if invalid

    if (!is.null(monitor)) {
        if (!is.SCProgressMonitor(monitor)) {
            stop(sprintf("argument %s must be object of class %s",
                         sQuote("settings"), "SCProgressMonitor"))
        }
    } else {
        ## Create one, if necessary
        monitor <- SCProgressMonitor()
    }

    ## Begin processing
    txtdir <- as(settings@txtdir, "character")
    outdir <- as(settings@outdir, "character")
    imgdir <- if (is.Directory(settings@imgdir)) {
                  as(settings@imgdir, "character")
              } else {
                  NULL
              }

    rppasetArgs <- list(path=txtdir,
                        designparams=settings@designparams,
                        fitparams=settings@fitparams,
                        spatialparams=settings@spatialparams,
                        normparams=settings@normparams,
                        doprefitqc=settings@doprefitqc,
                        monitor=monitor)
    ## :NOTE: Handle following after list construction so NULL values dropped
    rppasetArgs$antibodyfile <- settings@antibodyfile
    rppasetArgs$software <- settings@software
    rppasetArgs$alt.layout <- settings@alt.layout

    ## Perform analysis
    rppaset <- do.call(RPPASet, rppasetArgs)

    ## Save results (as rppaset takes forever to generate)
    rda.filename <- "sc-rppaset.RData"
    save(rppaset, file=file.path(outdir, rda.filename))

    ## Summarize the results
    progressStage(monitor) <- "Graphing"
    write.summary(rppaset,
                  path=outdir,
                  graphs=TRUE,
                  tiffdir=imgdir,
                  onlynormqcgood=settings@onlynormqcgood,
                  monitor=monitor)
}

