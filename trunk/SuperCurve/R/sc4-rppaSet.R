###
### RPPASET.R - Fit a set of slides with a common layout
###


##=============================================================================
setClass("RPPASet",
         representation=list(call="call",               # function call used to create the model
                             version="character",       # package version
                             design="RPPADesign",       # common design for all slides
                             rppas="array",             # vector of RPPAs
                             fitparams="RPPAFitParams", # parameters used for fitting
                             fits="array"))             # set of fits
## :KRC: Why is "rppas" an array or vector instead of a list (or environment)?
## :PLR: Because Corwin? wrote it this way...


##-----------------------------------------------------------------------------
is.RPPASet <- function(x) {
    inherits(x, "RPPASet")
}


##-----------------------------------------------------------------------------
## Returns a slot in the array of fits as a simple matrix view.
.fitSlot <- function(rppaset, sl) {
    ## Check arguments
    stopifnot(is.RPPASet(rppaset))
    stopifnot(is.character(sl) && length(sl) == 1)

    if (!(sl %in% slotNames(rppaset@fits[[1]]))) {
        stop(sprintf("invalid slotname %s",
                     sQuote(sl)))
    }

    ## Begin processing
    ## :TODO: simplify processing using slot method instead of eval/parse
    expr <- paste("rppaset@fits[[1]]@", sl, sep="")
    mat <- matrix(NA,
                  nrow=length(eval(parse(text=expr))),
                  ncol=length(rownames(rppaset@fits)))
    rownames(mat) <- colnames(t(eval(parse(text=expr))))

    for (j in seq_len(ncol(mat))) {
        expr <- paste("rppaset@fits[[j]]@", sl, sep="")
        mat[, j] <- eval(parse(text=expr))
    }
    colnames(mat) <- rownames(rppaset@fits)
    mat
}


##-----------------------------------------------------------------------------
## Get antibody names
.getAntibodyNames <- function(rppaset) {
    ## Check arguments
    stopifnot(is.RPPASet(rppaset))

    ## Begin processing
    slideFilenames <- rownames(rppaset@fits)
    ## Remove filename extensions
    sub(".[Tt][Xx][Tt]$", "", slideFilenames)
}


##-----------------------------------------------------------------------------
## Create the fit graphs and save them as PNG files
.createFitGraphs <- function(rppaset, path, prefix) {
    ## Check arguments
    stopifnot(is.RPPASet(rppaset))
    stopifnot(is.character(path)   && length(path) == 1)
    stopifnot(is.character(prefix) && length(prefix) == 1)

    ## Begin processing
    saved.par <- par(no.readonly=TRUE)
    on.exit(par(saved.par))

    par(bg="white",
        mfrow=c(2, 1))

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

    fitxform <- rppaset@fitparams@xform
    antibodies <- .getAntibodyNames(rppaset)
    for (i in seq_along(antibodies)) {
        antibody <- antibodies[i]
        rppafit <- rppaset@fits[[i]]

        ptitle <- paste(rppafit@measure,
                        ":  ",
                        antibody,
                        sep="")

        ## :TBD: Any good reason why these are produced to screen
        ## first anyway? Why not just create plots as disk files
        ## and avoid the device copy since only the last image
        ## is still visible at the end of the run?

        ## First pair of plots
        try(plot(rppafit,
                 main=ptitle,
                 xform=fitxform,
                 xlim=c(-15, 15)))

        ## Mark R^2 = 0.4 and below as red.
        imageRPPAFit <- getMethod("image", class(rppafit))            
        imageRPPAFit(rppafit,
                     col=RYG,
                     main="",
                     measure="ResidualsR2",
                     xlab="Residuals R^2",
                     zlim=c(0.4, 1))

        filename <- paste(paste(prefix, antibody, sep="_"),
                          "png",
                          sep=".")
        dev.copy(png,
                 file.path(path, filename),
                 width=640,
                 height=640)
        dev.off()

        ## Second pair of plots
        try(plot(rppafit,
                 main=ptitle,
                 type="resid",
                 xform=fitxform,
                 xlim=c(-15, 15)))
        try(plot(rppafit,
                 main=ptitle,
                 type="steps",
                 xform=fitxform,
                 xlim=c(-15, 15)))

        filename <- paste(paste(prefix, antibody, "2", sep="_"),
                          "png",
                          sep=".")
        dev.copy(png,
                 file.path(path, filename),
                 width=640,
                 height=640)
        dev.off()
    }
}


##-----------------------------------------------------------------------------
## Merge output graphs with source tiff file, save it as JPG file
.mergeGraphsAndImage <- function(antibody, prefix, outputdir, tiffdir) {
    ## Check arguments
    stopifnot(is.character(antibody)  && length(antibody) == 1)
    stopifnot(is.character(prefix)    && length(prefix) == 1)
    stopifnot(is.character(outputdir) && length(outputdir) == 1)
    stopifnot(is.character(tiffdir)   && length(tiffdir) == 1)

    ## Begin processing
    filename <- paste(antibody, "tif", sep=".")
    tiff <- file.path(tiffdir, filename)

    filename <- paste(paste(prefix, antibody, sep="_"),
                      "png",
                      sep=".")
    pg1 <- file.path(outputdir, filename)

    filename <- paste(paste(prefix, antibody, "2", sep="_"),
                      "png",
                      sep=".")
    pg2 <- file.path(outputdir, filename)

    filename <- paste(antibody, "jpg", sep=".")
    output <- file.path(outputdir, filename)

    ## Use ImageMagick 'convert' binary to perform merge
    message(paste("merging tiff for", antibody))
    command <- paste('convert',
                     shQuote(pg1),
                     shQuote(pg2),
                     '+append',
                     shQuote(tiff),
                     '-append',
                     '-quality 100',
                     shQuote(output))
    return(rc <- switch(EXPR=.Platform$OS.type,
                        unix=system(command),
                        windows=shell(command),
                        stop(sprintf("unrecognized operating system %s",
                                     sQuote(.Platform$OS.type)))))
}


##-----------------------------------------------------------------------------
## Provide a convenience function to save fit results to file
## :TODO: Rename this method to something more appropriate
write.summary <- function(rppaset,
                          path,
                          prefix="supercurve",
                          graphs=TRUE,
                          tiffdir=NULL) {
    ## Check arguments
  
    ## would not need this is it were a method....
    if (!is.RPPASet(rppaset)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("rppaset"), "RPPASet"))
    }

    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    } else if (!(file.exists(path) & file.info(path)$isdir)) {
        stop(sprintf("directory %s does not exist",
                     dQuote(path)))
    } else if (!(file.access(path, mode=2) == 0)) {
        stop(sprintf("directory %s is not writable",
                     dQuote(path)))
    }

    if (!is.character(prefix)) {
        stop(sprintf("argument %s must be character",
                     sQuote("prefix")))
    } else if (!(length(prefix) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("prefix")))
    }

    if (is.numeric(graphs)) {
        graphs <- as.logical(graphs)
    }

    if (!is.logical(graphs)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("graphs")))
    } else if (!(length(graphs) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("graphs")))
    }

    ## Begin processing
    conc <- .fitSlot(rppaset, "concentrations")
    conc.ss <- .fitSlot(rppaset, "ss.ratio")
    if (sum(as.character(rppaset@design@alias$Alias) ==
            as.character(rppaset@design@alias$Sample)) < nrow(conc)) {
        ## We have non-trivial alias names.
        ## Use sample aliases to write out data
        rno <- rownames(conc)
        sn <- rppaset@design@sampleMap[rno]
        lookup.sn <- match(sn, rppaset@design@alias$Sample)
        alias.name <- as.character(rppaset@design@alias$Alias)[lookup.sn]
        rownames(conc) <- alias.name
        rownames(conc.ss) <- alias.name
    }

    ## Write file for raw concentrations
    filename <- paste(paste(prefix, "conc_raw", sep="_"),
                      "csv",
                      sep=".")
    write.csv(conc, file=file.path(path, filename))

    ## Write file for R^2 statistics
    filename <- paste(paste(prefix, "ss_ratio", sep="_"),
                      "csv",
                      sep=".")
    write.csv(conc.ss, file=file.path(path, filename))

    ## Median polish to normalize sample, slide effects
    pol <- medpolish(conc, trace.iter=FALSE)
    conc <- pol$residuals
    sample.correction <- pol$row
    conc <- cbind(sample.correction, conc)

    ## Write file for polished concentration
    filename <- paste(paste(prefix, "conc_med_polish", sep="_"),
                      "csv",
                      sep=".")
    write.csv(conc, file=file.path(path, filename))

    if (graphs) {
        if (is.null(tiffdir)) {
            ## Assume the tif images are in a sibling directory named "tif"
            tiffdir <- normalizePath(file.path(path, "..", "tif"))
        }

        if (!is.character(tiffdir)) {
            stop(sprintf("argument %s must be character",
                         sQuote("tiffdir")))
        } else if (!(length(tiffdir) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("tiffdir")))
        } else if (!(file.exists(tiffdir) & file.info(tiffdir)$isdir)) {
            stop(sprintf("directory %s does not exist",
                         dQuote(tiffdir)))
        }

        ## Save fit graphs
        .createFitGraphs(rppaset, path, prefix)

        ## Merge output graphs with source tiff file for each antibody
        antibodies <- .getAntibodyNames(rppaset)
        for (i in seq_along(antibodies)) {
            rc <- .mergeGraphsAndImage(antibodies[i], prefix, path, tiffdir)
            if (rc == 32512) {
                warning(sprintf("ImageMagick executable %s not installed or unavailable via PATH",
                                sQuote("convert")))
                message("some output files may be missing")
                break
            }
        }
    }
}


setMethod("summary", "RPPASet",
          function(object,
                   path,
                   prefix="supercurve",
                   graphs=TRUE,
                   tiffdir=NULL,
                   ...) {
    ## :PLR: A call to summary on any other kind of object won't write to disk
    ## This seems at odds with below...
    write.summary(object, path, prefix, graphs, tiffdir)
})


##-----------------------------------------------------------------------------
## Create an RPPA set from a directory of slides.
RPPASet <- function(path,
                    designparams,
                    fitparams,
                    software="microvigene") {
    ## Check arguments
    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    } else if (!(file.exists(path) & file.info(path)$isdir)) {
        stop(sprintf("directory %s does not exist",
                     dQuote(path)))
    }

    if (!is.RPPADesignParams(designparams)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("designparams"), "RPPADesignParams"))
    }

    if (!is.RPPAFitParams(fitparams)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("fitparams"), "RPPAFitParams"))
    }

    ## :TBD: Should this get the list of slides from a file ('proteinAssay.tsv'
    ## or 'targets.txt') instead of assuming all .txt files are slides?
    getQuantificationFilenames <- function(path) {
        ## Assumes all .txt files in the directory are slides
        txt.re <- ".*[tT][xX][tT]$"
        list.files(path=path, pattern=txt.re)
    }

    ## Begin processing
    call <- match.call()

    ## Get filenames of slides to process
    slideFilenames <- getQuantificationFilenames(path)
    if (length(slideFilenames) == 0) {
        stop(sprintf("no quantification files found in directory %s",
                     dQuote(path)))
    }

    ## Load alias information in directory
    if (length(designparams@alias) < 1) {
        pathname <- file.path(path, "layoutInfo.tsv")
        if (file.exists(pathname)) {
            tryCatch({
                         sampleLayout <- read.delim(pathname,
                                                    quote="",
                                                    row.names=NULL)
                         designparams@alias <- list(Alias=sampleLayout$Alias,
                                                    Sample=sampleLayout$Sample)
                         rm(sampleLayout)
                     },
                     error=function(e) {
                         warning(sprintf("cannot import alias information from file %s",
                                         dQuote(pathname)))
                     })
        }
        rm(pathname)
    }

    ## Load slides to process
    ## :TBD: Why was this construct used and not 'vector("list", numslides)'
    ## Is the dimension attribute used?
    rppas <- array(list(), length(slideFilenames), slideFilenames)
    for (i in seq_along(slideFilenames)) {
        slideFilename <- slideFilenames[i]

        message(paste("reading", slideFilename))
        rppas[[i]] <- RPPA(slideFilename, path=path, software=software)

        ## If this is first slide read...
        if (i == 1) {
            firstslide <- rppas[[1]]

            ## Create design
            design <- RPPADesignFromParams(firstslide, designparams)

            ## Plot the first slide as a quick design check
            ## :TBD: Should this be plotting the requested measure instead?
            plot(firstslide,
                 design,
                 "Mean.Total",
                 main=slideFilename)
            rm(firstslide)
        }
        rm(slideFilename)
    }

    ## Create fits
    fits <- array(list(), length(slideFilenames), slideFilenames)
    for (i in seq_along(slideFilenames)) {
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

