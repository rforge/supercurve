###
### APPDEFAULTS.R
###

library(tclish)


##-----------------------------------------------------------------------------
## Loads application defaults into resource database, merging from multiple
## sources.
loadAppDefaults <- function(classNames,
                            priority="startupFile") {
    ## Check arguments
    stopifnot(is.character(classNames) && all(nzchar(classNames)))
    stopifnot((is.character(priority) || is.numeric(priority)) &&
              length(priority) == 1)

    ##-------------------------------------------------------------------------
    splitPath <- function(path,
                          sep=.Platform$path.sep) {
        stopifnot(is.character(path) && length(path) == 1)
        as.character(unlist(strsplit(path, sep)))
    }


    ##-------------------------------------------------------------------------
    getCustomization <- function() {
        rsrcClass <- "Customization"
        rsrcName <- tolower(rsrcClass)
        as.character(tclvalue(optiondb_get(rsrcName=rsrcName,
                                           rsrcClass=rsrcClass)))
    }


    ##-------------------------------------------------------------------------
    getLanguageLocaleCodeset <- function() {
        Sys.getenv("LANG")
    }


    ##-------------------------------------------------------------------------
    readAppDefaultsFile <- function(pathname,
                                    priority,
                                    verbose=FALSE) {
        stopifnot(is.character(pathname) && length(pathname) == 1)
        # skip priority as it was already evaluated
        stopifnot(is.logical(verbose) && length(verbose) == 1)

        tryCatch(optiondb_readfile(pathname, priority, verbose),
                 error=function(cond) {
                     warning(sprintf("error loading %s: %s",
                                     dQuote(pathname),
                                     conditionMessage(cond)),
                             call.=FALSE)
                 })
    }


    ## Begin processing
    verbose <- getOption("verbose")
    customization <- getCustomization()
    llc <- getLanguageLocaleCodeset()
    lang <- substr(llc, 1, 2)
    directories <- c("/usr/lib/X11/%L/%T",
                     "/usr/lib/X11/%l/%T",
                     "/usr/lib/X11/%T",
                     "/usr/X11R6/lib/X11/%L/%T",
                     "/usr/X11R6/lib/X11/%l/%T",
                     "/usr/X11R6/lib/X11/%T",
                     splitPath(Sys.getenv("XFILESEARCHPATH")),
                     Sys.getenv("XAPPLRESDIR"),
                     splitPath(Sys.getenv("XUSERFILESEARCHPATH")))
    directories <- directories[nzchar(directories)]

    for (className in classNames) {
        for (directory in directories) {
            pathname <- if (grepl("%", directory)) {
                            sub("%C", customization, fixed=TRUE,
                                sub("%N", className, fixed=TRUE,
                                    sub("%T", "app-defaults", fixed=TRUE,
                                        sub("%L", llc, fixed=TRUE,
                                            sub("%l", lang, fixed=TRUE,
                                                directory)))))
                        } else {
                            file.path(directory, className)
                        }
            if (verbose) {
                message(pathname)
            }
            if (file.exists(pathname) && !file.info(pathname)$isdir) {
                readAppDefaultsFile(pathname, priority, verbose)
            }
        }
    }
}

