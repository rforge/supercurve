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


##=============================================================================
setClass("RPPASetSummary",
         representation=list(raw="matrix",
                             ss="matrix",
                             medpol="matrix"))


##-----------------------------------------------------------------------------
is.RPPASet <- function(x) {
    inherits(x, "RPPASet")
}


##-----------------------------------------------------------------------------
is.RPPASetSummary <- function(x) {
    inherits(x, "RPPASetSummary")
}


##-----------------------------------------------------------------------------
## Returns a slot in the array of fits as a simple matrix view.
.fitSlot <- function(rppaset,
                     slotname) {
    ## Check arguments
    stopifnot(is.RPPASet(rppaset))
    stopifnot(is.character(slotname) && length(slotname) == 1)

    if (!(slotname %in% slotNames(rppaset@fits[[1]]))) {
        stop(sprintf("invalid slotname %s",
                     sQuote(slotname)))
    }

    ## Begin processing
    sapply(rppaset@fits,
           slot,
           name=slotname)
}


##-----------------------------------------------------------------------------
## Create the fit graphs and save them as PNG files
.createFitGraphs <- function(rppaset,
                             path,
                             prefix) {
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

    antibodies <- rownames(rppaset@fits)
    for (i in seq_along(antibodies)) {
        antibody <- antibodies[i]
        rppafit <- rppaset@fits[[i]]

        ptitle <- .mkPlotTitle(rppafit@measure, antibody)

        ## First pair of plots
        try(plot(rppafit,
                 main=ptitle,
                 xform=fitxform,
                 xlim=c(-15, 15)))

        ## Mark R^2 = 0.4 and below as red.
        imageRPPAFit <- getMethod("image", class(rppafit))
        imageRPPAFit(rppafit,
                     col=RYG,
                     measure="ResidualsR2",
                     zlim=c(0.4, 1))

        filename <- paste(paste(prefix, antibody, "1",  sep="_"),
                          "png",
                          sep=".")
        dev.copy(png,
                 file.path(path, .portableFilename(filename)),
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
                 file.path(path, .portableFilename(filename)),
                 width=640,
                 height=640)
        dev.off()
    }
}


##-----------------------------------------------------------------------------
## Merge output graphs with source tiff file, save it as JPG file
.mergeGraphsAndImage <- function(antibody,
                                 prefix,
                                 outputdir,
                                 tiff) {
    ## Check arguments
    stopifnot(is.character(antibody)  && length(antibody) == 1)
    stopifnot(is.character(prefix)    && length(prefix) == 1)
    stopifnot(is.character(outputdir) && length(outputdir) == 1)
    stopifnot(is.character(tiff)      && length(tiff) == 1)

    ## Begin processing
    filename <- paste(paste(prefix, antibody, "1", sep="_"),
                      "png",
                      sep=".")
    pg1 <- file.path(outputdir, .portableFilename(filename))

    filename <- paste(paste(prefix, antibody, "2", sep="_"),
                      "png",
                      sep=".")
    pg2 <- file.path(outputdir, .portableFilename(filename))

    filename <- paste(antibody, "jpg", sep=".")
    output <- file.path(outputdir, .portableFilename(filename))

    ## Use ImageMagick 'convert' binary to perform merge
    command <- paste("convert",
                     shQuote(pg1),
                     shQuote(pg2),
                     "+append",
                     shQuote(tiff),
                     "-append",
                     "-quality 100",
                     shQuote(output))
    rc <- switch(EXPR=.Platform$OS.type,
                 unix=system(command),
                 windows=shell(command),
                 stop(sprintf("unrecognized operating system family %s",
                              sQuote(.Platform$OS.type))))
    #cat("rc =", rc, ", command:", command, "\n")

    return(rc)
}


##-----------------------------------------------------------------------------
## Create an RPPASetSummary object
RPPASetSummary <- function(rppaset) {
    ## Check arguments
    if (!is.RPPASet(rppaset)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("rppaset"), "RPPASet"))
    }

    ## Begin processing
    conc.raw <- .fitSlot(rppaset, "concentrations")
    conc.ss <- .fitSlot(rppaset, "ss.ratio")
    if (sum(as.character(rppaset@design@alias$Alias) ==
            as.character(rppaset@design@alias$Sample)) < nrow(conc.raw)) {
        ## We have non-trivial alias names.
        ## Use sample aliases to write out data
        rno <- rownames(conc.raw)
        sn <- rppaset@design@sampleMap[rno]
        lookup.sn <- match(sn, rppaset@design@alias$Sample)
        alias.name <- as.character(rppaset@design@alias$Alias)[lookup.sn]
        rownames(conc.raw) <- alias.name
        rownames(conc.ss) <- alias.name
    }

    ## Median polish to normalize sample, slide effects
    ##   where:
    ##     row       - sample correction
    ##     residuals - polished concentrations
    ##
    pol <- medpolish(conc.raw, trace.iter=FALSE)
    conc.medpol <- cbind(pol$row, pol$residuals)
    colnames(conc.medpol)[1] <- "Correction"

    ## Create new class
    new("RPPASetSummary",
        raw=conc.raw,
        ss=conc.ss,
        medpol=conc.medpol)
}


##-----------------------------------------------------------------------------
## See R FAQ (8.1 How should I write summary methods?)
setMethod("summary", "RPPASet",
          function(object,
                   ...) {
    RPPASetSummary(object)
})


##-----------------------------------------------------------------------------
setMethod("write.summary", "RPPASetSummary",
          function(object,
                   path,
                   prefix="supercurve",
                   ...) {
    ## Check arguments
    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    } else if (!dir.exists(path)) {
        stop(sprintf("directory %s does not exist",
                     dQuote(path)))
    } else if (!dir.writable(path)) {
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

    ## Begin processing

    ## Write file for raw concentrations
    filename <- paste(paste(prefix, "conc_raw", sep="_"),
                      "csv",
                      sep=".")
    write.csv(object@raw, file=file.path(path, .portableFilename(filename)))

    ## Write file for R^2 statistics
    filename <- paste(paste(prefix, "ss_ratio", sep="_"),
                      "csv",
                      sep=".")
    write.csv(object@ss, file=file.path(path, .portableFilename(filename)))

    ## Write file for polished concentration
    filename <- paste(paste(prefix, "conc_med_polish", sep="_"),
                      "csv",
                      sep=".")
    write.csv(object@medpol, file=file.path(path, .portableFilename(filename)))
})


##-----------------------------------------------------------------------------
## Provide a convenience function to save fit results to file
setMethod("write.summary", "RPPASet",
          function(object,
                   path,
                   prefix="supercurve",
                   graphs=TRUE,
                   tiffdir=NULL) {
    ## Check arguments
    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    } else if (!dir.exists(path)) {
        stop(sprintf("directory %s does not exist",
                     dQuote(path)))
    } else if (!dir.writable(path)) {
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

    ## Merge output, if requested
    if (graphs) {
        pkgimgdir <- system.file("images", package="SuperCurve")

        if (is.null(tiffdir)) {
            ## Assume the tif images are in a sibling directory named "tif"
            tiffdir <- normalizePath(file.path(path, "..", "tif"))
            if (!dir.exists(tiffdir)) {
                ## As last resort, use package directory for missing image
                message(sprintf("image directory unspecified and sibling directory %s does not exist",
                                dQuote(tiffdir)))
                tiffdir <- pkgimgdir
            }
        }

        if (!is.character(tiffdir)) {
            stop(sprintf("argument %s must be character",
                         sQuote("tiffdir")))
        } else if (!(length(tiffdir) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("tiffdir")))
        } else if (!dir.exists(tiffdir)) {
            stop(sprintf("directory %s does not exist",
                         dQuote(tiffdir)))
        }

        ## Save fit graphs
        .createFitGraphs(object, path, prefix)

        ## Merge output graphs with source tiff file for each antibody
        imgfiles <- {
                        txtfiles <- sapply(object@fits,
                                           function(fit) {
                                               fit@rppa@file
                                           })
                        txt.re <- "\\.[tT][xX][tT]$"
                        sub(txt.re, ".tif", txtfiles)
                    }

        ## For each antibody...
        antibodies <- names(object@fits)
        for (antibody in antibodies) {

            message(paste("merging graphs and image for", antibody))
            flush.console()

            ## If no corresponding image exists, substitute "missing" image
            imgfile <- file.path(tiffdir, imgfiles[antibody])
            if (!file.exists(imgfile)) {
                imgfile <- file.path(pkgimgdir, "missing_slide.tif")
            }

            ## Create merged image
            rc <- .mergeGraphsAndImage(antibody,
                                       prefix,
                                       path,
                                       imgfile)
            if (rc == 32512) {
                warning(sprintf("ImageMagick executable %s not installed or unavailable via PATH",
                                sQuote("convert")))
                message("some output files may be missing")
                flush.console()
                break
            }
        }
    }

    ## Write CSV files
    callGeneric(summary(object),
                path,
                prefix)
})


##-----------------------------------------------------------------------------
.loadAntibodyInfo <- function(antibodyfile,
                              slidefiles) {
    ## Check arguments
    stopifnot(is.character(antibodyfile) && length(antibodyfile) == 1)
    stopifnot(is.character(slidefiles) && length(slidefiles) >= 1)

    ## Begin processing
    tryCatch({
            stopifnot(file.exists(antibodyfile))

            ## Read datafile
            proteinassay.df <- read.delim(antibodyfile,
                                          as.is=TRUE,
                                          quote="",
                                          row.names=NULL)

            reqdColnames <- c("Antibody",
                              "Filename")

            ## Ensure minimum number of columns
            if (!(ncol(proteinassay.df) >= length(reqdColnames))) {
                stop("not enough columns")
            }

            ## Ensure required columns exist
            found <- reqdColnames %in% colnames(proteinassay.df)
            if (!(all(found))) {
                missingColumns <- reqdColnames[!found]
                stop(sprintf(ngettext(length(missingColumns),
                                      "missing required column: %s",
                                      "missing required columns: %s"),
                             paste(dQuote(missingColumns), collapse=", ")))
            }
        },
        error=function(cond) {
            stop(sprintf("cannot load antibody data from file %s - %s",
                         dQuote(antibodyfile),
                         conditionMessage(cond)))
        })

    ## Extract information from data.frame
    antibodies <- vector("list", length(slidefiles))
    names(antibodies) <- slidefiles

    for (filename in slidefiles) {
        x.antibody <- match(filename, proteinassay.df$Filename)[1]
        antibody <- proteinassay.df$Antibody[x.antibody]
        if (!is.na(antibody)) {
            x.slidefiles <- match(filename, slidefiles)
            antibodies[[x.slidefiles]] <- antibody
        }
    }

    return(antibodies)
}


##-----------------------------------------------------------------------------
## Create an RPPA set from a directory of slides.
RPPASet <- function(path,
                    designparams,
                    fitparams,
                    antibodyfile=NULL,
                    software="microvigene") {
    ## Check arguments
    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    } else if (!dir.exists(path)) {
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

    if (!is.null(antibodyfile)) {
        if (!is.character(antibodyfile)) {
            stop(sprintf("argument %s must be character",
                         sQuote("antibodyfile")))
        } else if (!(length(antibodyfile) == 1)) {
            stop(sprintf("argument %s must be of length 1",
                         sQuote("antibodyfile")))
        } else if (!nzchar(antibodyfile)) {
            stop(sprintf("argument %s must not be empty string",
                         sQuote("antibodyfile")))
        }

        if (!.isAbsolutePathname(antibodyfile)) {
            antibodyfile <- file.path(path, antibodyfile)
        }
    }


    ##-------------------------------------------------------------------------
    ## Returns the names of all TXT files in directory argument.
    ## :TBD: Should this get the list of slides from a file ('proteinAssay.tsv'
    ## or 'targets.txt') instead of assuming all .txt files are slides?
    getQuantificationFilenames <- function(path) {
        stopifnot(is.character(path) && length(path) == 1)

        ## Assumes all .txt files in the directory are slides
        txt.re <- "\\.[tT][xX][tT]$"
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

    ## Load antibody information, if provided
    ab.list <- if (!is.null(antibodyfile)) {
                   .loadAntibodyInfo(antibodyfile, slideFilenames)
               } else {
                   vector("list", length(slideFilenames))
               }

    ## Fill in missing values with generated defaults
    x.which <- which(sapply(ab.list, is.null))
    txt.re <- "\\.[tT][xX][tT]$"
    for (x in x.which) {
        ab.list[[x]] <- sub(txt.re, "", slideFilenames[x])
    }

    ## Ensure antibody names are unique
    antibodies <- make.unique(abnames <- unlist(ab.list, use.names=FALSE))
    if (!identical(antibodies, abnames)) {
        warning("adjusting antibody names to be unique")
    }
    remove(abnames)

    ## Load slides to process
    rppas <- array(list(), length(slideFilenames))
    for (i in seq_along(slideFilenames)) {
        slideFilename <- slideFilenames[i]
        antibody <- antibodies[i]

        message(paste("reading", slideFilename))
        flush.console()

        rppas[[i]] <- RPPA(slideFilename,
                           path=path,
                           antibody=antibody,
                           software=software)

        ## If this is first slide read...
        if (i == 1) {
            firstslide <- rppas[[1]]

            ## Create design
            design <- RPPADesignFromParams(firstslide, designparams)

            ## Plot the first slide as a quick design check
            plot(firstslide,
                 design,
                 fitparams@measure)

            remove(firstslide)
        }
    }
    rownames(rppas) <- antibodies

    ## Create fits
    fits <- array(list(), length(slideFilenames), slideFilenames)
    for (i in seq_along(slideFilenames)) {
        message(paste("fitting", antibodies[i], "-", "please wait."))
        flush.console()

        fits[[i]] <- RPPAFitFromParams(rppas[[i]],
                                       design=design,
                                       fitparams=fitparams)
    }
    rownames(fits) <- antibodies

    ## Create new class
    new("RPPASet",
        call=call,
        design=design,
        fitparams=fitparams,
        fits=fits,
        rppas=rppas,
        version=packageDescription("SuperCurve", fields="Version"))
}

