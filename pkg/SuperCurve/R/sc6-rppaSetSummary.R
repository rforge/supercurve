###
### $Id$
### Summarize fit processing results
###


##=============================================================================
setClass("RPPASetSummary",
         representation(raw="matrix",              ## raw concentrations
                        ss="matrix",               ## sum squares ratio
                        norm="matrix",             ## normalized concentrations
                        probs="numeric",           ## probability good slide
                        completed="matrix",        ## what worked/failed
                        design="RPPADesign",       ## design for all slides
                        onlynormqcgood="logical",  ## filter norm'd by qc score
                        version="character"))      ## package version


##-----------------------------------------------------------------------------
is.RPPASetSummary <- function(x) {
    is(x, "RPPASetSummary")
}


##-----------------------------------------------------------------------------
## Returns a slot in the array of fits as a simple matrix view.
.fitSlot <- function(rppaset,
                     slotname) {
    ## Check arguments
    stopifnot(is.RPPASet(rppaset))
    stopifnot(is.character(slotname) && length(slotname) == 1)

    rppafits.tf <- rppaset@completed[, 'fit']
    rppafits <- rppaset@fits[rppafits.tf]

    if (!(slotname %in% slotNames(rppafits[[1]]))) {
        stop(sprintf("invalid slotname %s",
                     sQuote(slotname)))
    }

    ## Begin processing
    sapply(rppafits,
           slot,
           name=slotname)
}


##-----------------------------------------------------------------------------
## Create an RPPASetSummary object
RPPASetSummary <- function(rppaset,
                           onlynormqcgood=ran.prefitqc(rppaset),
                           monitor=NULL) {
    ## Check arguments
    if (!is.RPPASet(rppaset)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("rppaset"), "RPPASet"))
    }
    if (!is.logical(onlynormqcgood)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("onlynormqcgood")))
    } else if (!(length(onlynormqcgood) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("onlynormqcgood")))
    }
    if (!is.null(monitor)) {
        if (!is.SCProgressMonitor(monitor)) {
            stop(sprintf("argument %s must be object of class %s",
                         sQuote("monitor"), "SCProgressMonitor"))
        }
    } else {
        ## Create one, if necessary
        monitor <- SCProgressMonitor()
    }

    ## Begin processing
    design <- rppaset@design
    conc.raw <- .fitSlot(rppaset, "concentrations")
    conc.ss  <- .fitSlot(rppaset, "ss.ratio")
    if (sum(as.character(design@alias$Alias) ==
            as.character(design@alias$Sample)) < nrow(conc.raw)) {
        ## We have non-trivial alias names.
        ## Use sample aliases to write out data
        rno <- rownames(conc.raw)
        sn <- design@sampleMap[rno]
        lookup.sn <- match(sn, design@alias$Sample)
        alias.name <- as.character(design@alias$Alias)[lookup.sn]
        rownames(conc.raw) <- alias.name
        rownames(conc.ss) <- alias.name
    }

    ## Generate probabilities (goodness) for each processed slide (if any)
    prefitqcs.tf <- rppaset@completed[, "prefitqc"]
    probs <- if (!all(is.na(prefitqcs.tf))) {
                 progressMarquee(monitor) <- "Generating QC probabilities"
                 prefitqcs <- rppaset@prefitqcs[prefitqcs.tf]
                 sapply(prefitqcs, qcprob)
             } else {
                 as.numeric(NaN)
             }

    ## Normalize the concentrations
    progressMarquee(monitor) <- "Normalizing concentrations"
    norm.tf <- if (onlynormqcgood) {
                   ## Remove "bad" slides...
                   local({
                       probs.tmp <- probs
                       probs.tmp[is.na(probs.tmp)] <- 0
                       good.cutoff <- 0.8
                       probs.tmp >= good.cutoff
                   })
               } else {
                   rep(TRUE, ncol(conc.raw))
               }
    normparams <- rppaset@normparams
    normalizeArgs <- c(list(), normparams@arglist)
    normalizeArgs$object <- conc.raw[, norm.tf]
    normalizeArgs$method <- normparams@method

    conc.norm <- do.call(normalize, normalizeArgs)

    ## Magically delicious hack to allow reorder on write
    rppafits.tf <- rppaset@completed[, "fit"]
    rppa <- rppaset@rppas[rppafits.tf][[1]]
    alt.layout <- SuperCurve:::layout(rppa)
    ## :TBD: Should this really be for SuperSlide only or more generic?
    if (!is.null(alt.layout) && alt.layout == "superslide") {
        layout <- design@layout
        firstsample.tf <- layout$SpotType == "Sample" & !duplicated(layout$Sample)
        locations <- as.integer(rownames(rppa@data)[firstsample.tf])

        attr(conc.raw,  "locations") <- locations
        attr(conc.ss,   "locations") <- locations
        attr(conc.norm, "locations") <- locations
    }

    ## Create new class
    new("RPPASetSummary",
        raw=conc.raw,
        ss=conc.ss,
        norm=conc.norm,
        probs=probs,
        completed=rppaset@completed,
        design=rppaset@design,
        onlynormqcgood=onlynormqcgood,
        version=packageDescription("SuperCurve", fields="Version"))
}


##-----------------------------------------------------------------------------
## Provide a convenience function to save fit summary results as CSV/TSV files
setMethod("write.summary", signature(object="RPPASetSummary"),
          function(object,
                   path,
                   prefix="supercurve",
                   monitor=NULL,
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

    if (!is.null(monitor)) {
        if (!is.SCProgressMonitor(monitor)) {
            stop(sprintf("argument %s must be object of class %s",
                         sQuote("monitor"), "SCProgressMonitor"))
        }
    } else {
        ## Create one, if necessary
        monitor <- SCProgressMonitor()
    }

    if (!is.character(prefix)) {
        stop(sprintf("argument %s must be character",
                     sQuote("prefix")))
    } else if (!(length(prefix) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("prefix")))
    }


    ##-------------------------------------------------------------------------
    ## Allows concentrations to be written back in different order.
    get_concs_ordered_for_write <- function(object,
                                            slotname) {
        stopifnot(is.RPPASetSummary(object))
        stopifnot(is.character(slotname) && length(slotname) == 1)

        if (!(slotname %in% slotNames(object))) {
            stop(sprintf("invalid slotname %s",
                         sQuote(slotname)))
        }

        concs <- slot(object, slotname)
        locations <- attr(concs, "locations", exact=TRUE)
        if (!is.null(locations)) {
            concs[order(locations), ]
        } else {
            concs
        }
    }


    ##-------------------------------------------------------------------------
    ## Create informative information to the filename
    mknormtag <- function(object) {
        stopifnot(is.RPPASetSummary(object))

        attrs <- attr(object@norm, "normalization", exact=TRUE)
        normMethod <- attrs$method
        qctag <- if (object@onlynormqcgood) {
                     "qc"
                 }
        if (normMethod == "medpolish") {
            if (is.null(qctag)) {
                normMethod
            } else {
                paste(normMethod, qctag, sep="-")
            }
        } else {
            sweeptag <- if (attrs$sweep.cols) "rowcol" else "col"
            if (is.null(qctag)) {
                paste(normMethod, sweeptag, sep="-")
            } else {
                paste(normMethod, sweeptag, qctag, sep="-")
            }
        }
    }


    ## Begin processing
    progressMarquee(monitor) <- "Writing Fit Summary Files"

    ## Write file for raw concentrations
    filename <- sprintf("%s_conc_raw.csv", prefix)
    conc.raw <- get_concs_ordered_for_write(object, "raw")
    write.csv(conc.raw, file=file.path(path, .portableFilename(filename)))

    ## Write file for R^2 statistics
    filename <- sprintf("%s_ss_ratio.csv", prefix)
    conc.ss <- get_concs_ordered_for_write(object, "ss")
    write.csv(conc.ss, file=file.path(path, .portableFilename(filename)))

    ## Write file for normalized concentrations
    filename <- sprintf("%s_conc_norm_%s.csv", prefix, mknormtag(object))
    conc.norm <- get_concs_ordered_for_write(object, "norm")
    write.csv(conc.norm, file=file.path(path, .portableFilename(filename)))

    ## If QC processing was performed...
    if (!(length(object@probs) == 1 && is.na(object@probs))) {
        ## Write file for QC probabilities
        filename <- sprintf("%s_prefit_qc.csv", prefix)
        probs.df <- data.frame("Filename"=names(object@probs),
                               "Probabilities"=object@probs,
                               row.names=seq_along(object@probs))
        write.csv(probs.df, file=file.path(path, .portableFilename(filename)))
    }

    ## Write file for stage completion summary
    filename <- sprintf("%s_summary.tsv", prefix)
    write.table(object@completed,
                file=file.path(path, .portableFilename(filename)),
                sep='\t',
                col.names=NA)

    invisible(NULL)
})

