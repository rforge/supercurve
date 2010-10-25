###
### RPPASETSUMMARY.R - Summarize fit processing results
###


##=============================================================================
setClass("RPPASetSummary",
         representation(raw="matrix",
                        ss="matrix",
                        medpol="matrix",
                        probs="numeric",
                        completed="matrix"))


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
RPPASetSummary <- function(rppaset) {
    ## Check arguments
    if (!is.RPPASet(rppaset)) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("rppaset"), "RPPASet"))
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

    ## Median polish to normalize sample, slide effects
    ##   where:
    ##     row       - sample correction
    ##     residuals - polished concentrations
    ##
    pol <- medpolish(conc.raw, trace.iter=FALSE)
    conc.medpol <- cbind(pol$row, pol$residuals)
    colnames(conc.medpol)[1] <- "Correction"

    ## Generate probabilities (goodness) for each processed slide (if any)
    prefitqcs.tf <- rppaset@completed[, 'prefitqc']
    probs <- if (!all(is.na(prefitqcs.tf))) {
                 prefitqcs <- rppaset@prefitqcs[prefitqcs.tf]
                 names(prefitqcs) <- names(prefitqcs.tf[prefitqcs.tf])
                 sapply(prefitqcs, qcprob)
             } else {
                 as.numeric(NaN)
             }

    ## Create new class
    new("RPPASetSummary",
        raw=conc.raw,
        ss=conc.ss,
        medpol=conc.medpol,
        probs=probs,
        completed=rppaset@completed)
}


##-----------------------------------------------------------------------------
## Provide a convenience function to save fit summary results as CSV/TSV files
setMethod("write.summary", "RPPASetSummary",
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

    ## Begin processing

    ## Write file for raw concentrations
    filename <- sprintf("%s_conc_raw.csv", prefix)
    write.csv(object@raw, file=file.path(path, .portableFilename(filename)))

    ## Write file for R^2 statistics
    filename <- sprintf("%s_ss_ratio.csv", prefix)
    write.csv(object@ss, file=file.path(path, .portableFilename(filename)))

    ## Write file for polished concentration
    filename <- sprintf("%s_conc_med_polish.csv", prefix)
    write.csv(object@medpol, file=file.path(path, .portableFilename(filename)))

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
})

