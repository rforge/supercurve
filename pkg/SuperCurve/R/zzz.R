###
### ZZZ.R
###


##
## Package/Namespace Hooks
##

##-----------------------------------------------------------------------------
.onAttach <- function(libname, pkgname) {
    verbose <- getOption("verbose")
    if (verbose) {
        local({
            libraryPkgName <- function(pkgname, sep="_") {
                unlist(strsplit(pkgname, sep, fixed=TRUE))[1]
            }
            packageDescription <- function(pkgname) {
                fieldnames <- c("Title", "Version")
                metafile <- file.path(libname, pkgname, "DESCRIPTION")
                meta <- as.list(read.dcf(metafile, fieldnames))
                names(meta) <- fieldnames
                return(meta)
            }

            meta <- packageDescription(pkgname)
            msg <- sprintf("%s, version %s",
                           meta$Title, meta$Version)
            packageStartupMessage(msg)
            msg <- sprintf("Type library(help=%s) to see package documentation",
                           libraryPkgName(pkgname))
            packageStartupMessage(msg)
        })
    }
}


##-----------------------------------------------------------------------------
.onLoad <- function(libname, pkgname) {

    ##-------------------------------------------------------------------------
    ## Preflight check use of ImageMagick 'convert' binary
    preflightCheck <- function() {
        command <- "convert --version"
        tryCatch({
                output <- switch(EXPR=.Platform$OS.type,
                                 unix=system(command,
                                             intern=TRUE,
                                             ignore.stderr=TRUE),
                                 windows=shell(command,
                                               intern=TRUE,
                                               ignore.stderr=TRUE),
                                 "")
                grepl("ImageMagick", output[1], fixed=TRUE)
            },
            error=function(e) {
                FALSE
            })
    }


    if (!preflightCheck()) {
        warning(sprintf("ImageMagick executable %s not installed or unavailable via PATH",
                        sQuote("convert")),
                call.=FALSE)
    }
}

