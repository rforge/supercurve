###
### RPPASET.R
###

##############################################################
## Fit a set of slides with a common layout
## Initial version: Corwin Joy


##-----------------------------------------------------------------------------
## Provide a generic convenience function to view a slot in the array of fits
## as a simple matrix view (e.g., fitslot(fitset, 'concentrations'))
setMethod("fitslot", "RPPASet",
          function(object,
                   sl,
                   ...) {
    ## Check arguments
    if (!is.character(sl)) {
        stop(sprintf("argument %s must be character",
                     sQuote("sl")))
    } else if (!(length(sl) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("sl")))
    }

    ## Begin processing
    expr <- paste("object@fits[[1]]@", sl, sep='')
    mat <- matrix(NA,
                  nrow=length(eval(parse(text=expr))),
                  ncol=length(rownames(object@fits)))
    rownames(mat) <- colnames(t(eval(parse(text=expr))))

    for (j in seq(1, ncol(mat))) {
        expr <- paste("object@fits[[j]]@", sl, sep='')
        mat[, j] <- eval(parse(text=expr))
    }
    colnames(mat) <- rownames(object@fits)
    mat
})


##-----------------------------------------------------------------------------
## Merge output graphs with source tiff files
.mergeGraphAndImage <- function(protein, namebase, outputdir, tiffdir) {
    stopifnot(is.character(protein)   && length(protein) == 1)
    stopifnot(is.character(namebase)  && length(namebase) == 1)
    stopifnot(is.character(outputdir) && length(outputdir) == 1)
    stopifnot(is.character(tiffdir)   && length(tiffdir) == 1)

    base <- sub(".txt", "", protein, fixed=TRUE)
    tiff <- file.path(tiffdir, paste(base, "tif", sep='.'))

    ## convert *4EBP1.tif "Y:\Private\LysateArray\DorisSiwak\Feiller\Feiler results\*4EBP1.txt.png" -append -quality 100 4EBP1.jpg

    pg1 <- file.path(outputdir, paste(namebase, protein, 'png', sep='.'))
    pg2 <- file.path(outputdir, paste(namebase, protein, '2', 'png', sep='.'))
    output <- file.path(outputdir, paste(base, "jpg", sep='.'))

    # convert  $pg1 $pg2 +append $tiff -append -quality 100 $output
    message(paste('merging tiff for', protein))
    command <- paste('convert ',
                     shQuote(pg1),
                     ' ',
                     shQuote(pg2),
                     ' +append ',
                     shQuote(tiff),
                     ' -append -quality 100 ',
                     shQuote(output),
                     sep='')
    return(rc <- system(command))
}


##-----------------------------------------------------------------------------
## Provide a convenience function to save fit results to file
setMethod("write.summary", "RPPASet",
          function(object,
                   namebase,
                   path,
                   graphs=TRUE,
                   tiffdir=NULL,
                   ...) {
    ## Check arguments
    if (!is.character(namebase)) {
        stop(sprintf("argument %s must be character",
                     sQuote("namebase")))
    } else if (!(length(namebase) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("namebase")))
    }

    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    } else if (!file.exists(path)) {
        ## :TODO: Add code to verify directory exists
        stop(sprintf("directory %s does not exist",
                     dQuote(path)))
    }

    if (!is.logical(graphs)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("graphs")))
    } else if (!(length(graphs) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("graphs")))
    }

    if (is.null(tiffdir)) {
        ## assume the tif images are in a sibling directory named "tif"
        tiffdir <- file.path(path, "..", "tif")
    }

    if (!is.character(tiffdir)) {
        stop(sprintf("argument %s must be character",
                     sQuote("tiffdir")))
    } else if (!(length(tiffdir) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("tiffdir")))
    } else if (!file.exists(tiffdir)) {
        ## :TODO: Add code to verify directory exists
        stop(sprintf("directory %s does not exist",
                     dQuote(tiffdir)))
    }

    ## Begin processing
    conc <- fitslot(object, 'concentrations')
    conc.ss <- fitslot(object, 'ss.ratio')
    if (sum(as.character(object@design@alias$Alias) ==
            as.character(object@design@alias$Sample)) < nrow(conc)) {
        ## We have non-trivial alias names.
        ## Use sample aliases to write out data
        rno <- rownames(conc)
        sn <- object@design@sampleMap[rno]
        lookup.sn <- match(sn, object@design@alias$Sample)
        alias.name <- as.character(object@design@alias$Alias)[lookup.sn]
        rownames(conc) <- alias.name
        rownames(conc.ss) <- alias.name
    }
    write.csv(conc,
              file=file.path(path,
                             paste(namebase, '_conc_raw.csv', sep='')))

    ## median polish to normalize sample, slide effects
    pol <- medpolish(conc, trace.iter=FALSE)
    conc <- pol$residuals
    sample.correction <- pol$row
    conc <- cbind(sample.correction, conc)
    write.csv(conc,
              file=file.path(path,
                             paste(namebase, '_conc_med_polish.csv', sep='')))

    write.csv(conc.ss,
              file=file.path(path,
                             paste(namebase, '_ss.csv', sep='')))

    ## Use red/yellow/green palette for residual plots.
    ## From RColorBrewer palette RdYlGn
    RYG <- c("#A50026",
             "#D73027",
             "#F46D43",
             "#FDAE61",
             "#FEE08B",
             "#FFFFBF",
             "#D9EF8B",
             "#A6D96A",
             "#66BD63",
             "#1A9850",
             "#006837")

    if (graphs) {
        ## save fit graphs
        op <- par(no.readonly=TRUE)
        par(mfrow=c(2, 1))
        proteins <- rownames(object@fits)
        for (i in seq(1, length(proteins))) {
            ## first pair of plots
            ptitle <- paste(object@fits[[i]]@measure,
                            ":  ",
                            proteins[i],
                            sep="")
            try(plot(object@fits[[i]],
                     main=ptitle,
                     xform=object@fitparams@xform,
                     xlim=c(-15, 15)))

            ## Mark R^2 = 0.4 and below as red.
            try(image(object@fits[[i]],
                      col=RYG,
                      main="",
                      measure="ResidualsR2",
                      xlab="Residuals R^2",
                      zlim=c(0.4, 1)))
            dev.copy(png,
                     file.path(path,
                               paste(namebase, proteins[i], 'png', sep='.')),
                     width=640,
                     height=640)
            dev.off()

            try(plot(object@fits[[i]],
                     main=ptitle,
                     type="resid",
                     xform=object@fitparams@xform,
                     xlim=c(-15, 15)))
            try(plot(object@fits[[i]],
                     main=ptitle,
                     type="steps",
                     xform=object@fitparams@xform,
                     xlim=c(-15, 15)))
            dev.copy(png,
                     file.path(path,
                               paste(namebase, proteins[i], '2', 'png', sep='.')),
                     width=640,
                     height=640)
            dev.off()
        }
        par(op)

        if (TRUE) {
            ## Use ImageMagick to merge output graphs with source tiff files
            for (i in seq(1, length(proteins))) {
                rc <- .mergeGraphAndImage(proteins[i], namebase, path, tiffdir)
                if (rc == 32512) {
                    warning(sprintf("ImageMagick executable %s not installed or unavailable via PATH", sQuote("convert")))
                    message("some output files may be missing")
                    break
                }
            }
        }
    }
})


##-----------------------------------------------------------------------------
# Create an RPPA set from a directory of slides.
# path = directory to analyze
# designparams = common slide design specification of class RPPADesignParams
# fitparams = common fit specification of class RPPAFitParams
#
# example usage:
# see tests/testRPPASet.R

RPPAFitDir <- function(path,
                       designparams,
                       fitparams,
                       blanks=NULL) {
    ## Check arguments
    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    } else if (!file.exists(path)) {
        ## :TODO: Add code to verify directory exists
        stop(sprintf("directory %s does not exist",
                     dQuote(path)))
    }
 
    if (!inherits(designparams, "RPPADesignParams")) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("designparams"), "RPPADesignParams"))
    }

    if (!inherits(fitparams, "RPPAFitParams")) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("fitparams"), "RPPAFitParams"))
    }
    ## :TODO: Add checks for 'blanks' argument

    ## Begin processing
    call <- match.call()

    ## assume all .txt files in the directory are slides
    slideFilenames <- {
                          txt.re <- ".*[tT][xX][tT]$"
                          list.files(path=path, pattern=txt.re)
                      }

    ## Load alias information in directory
    if (length(designparams@alias) < 1) {
        layoutInfoPathname <- file.path(path, 'layoutInfo.tsv')
        if (file.exists(layoutInfoPathname)) {
            sampleLayout <- try(read.delim(layoutInfoPathname,
                                           quote='',
                                           row.names=NULL))
            ## :TBD: If the above fails, what should happen?
            ## Would appear this would crash and burn here...
            al <- list(Alias=sampleLayout$Alias,
                       Sample=sampleLayout$Sample)
            designparams@alias <- al
        }
    }

    message(paste("reading", slideFilenames[1]))
    firstslide <- RPPA(slideFilenames[1],
                       path=path,
                       blanks)
    design <- RPPADesignFromParams(firstslide,
                                   designparams)

    ## plot the first slide as a quick design check
    plotDesign(firstslide,
               design,
               'Mean.Total',
               main=slideFilenames[1])

    ## :TBD: Why was this construct used and not 'vector("list", numslides)'
    ## Is the dimension attribute used?
    rppas <- array(list(), c(length(slideFilenames)), slideFilenames)
    rppas[[1]] <- firstslide
    if (length(slideFilenames) > 1) {
        for (i in seq(2, length(slideFilenames))) {
            message(paste("reading", slideFilenames[i]))
            rppas[[i]] <- RPPA(slideFilenames[i],
                               path=path,
                               blanks)
        }
    }

    fits <- array(list(), c(length(slideFilenames)), slideFilenames)
    for (i in seq(1, length(slideFilenames))) {
        message(paste("fitting", slideFilenames[i], "-", "Please wait."))
        fits[[i]] <- RPPAFitFromParams(rppas[[i]],
                                       design=design,
                                       fitparams=fitparams)
    }

    rownames(fits) <- slideFilenames
    rownames(rppas) <- slideFilenames

    ## Create new class
    new("RPPASet",
        call=call,
        design=design,
        fitparams=fitparams,
        fits=fits,
        rppas=rppas,
        version=packageDescription("SuperCurve", fields="Version"))
}

