###
### $Id$
###


##-----------------------------------------------------------------------------
## Used to enable providing consistent naming for measures.
.capwords <- function(s, strict=FALSE) {
    ## Check arguments
    stopifnot(is.character(s))

    ##-------------------------------------------------------------------------
    cap <- function(s) {
        paste(toupper(substring(s, 1, 1)),
              {
                  if (strict) {
                      tolower(substring(s, 2))
                  } else {
                      substring(s, 2)
                  }
              },
              sep="",
              collapse=".")
    }


    ## Begin processing
    return(sapply(strsplit(s, split="\\."),
                  cap,
                  USE.NAMES=!is.null(names(s))))
}


##-----------------------------------------------------------------------------
## Returns dimensions of slide layout as numeric vector.
.dimOfLayout <- function(layout) {
    ## Check arguments
    stopifnot(is.data.frame(layout))

    ## Begin processing
    return(sapply(.locationColnames(),
                  function(df, column) {
                      max(df[[column]])
                  },
                  df=layout))
}


##-----------------------------------------------------------------------------
## Specifies measures capable of being used for fits.
## N.B.: use intersection of this with what is actually available in data.frame
.fitColnames <- function() {
    return(c("Mean.Net",
             "Mean.Total",
             "Median.Net",
             "Median.Total"))
}


##-----------------------------------------------------------------------------
## Tests whether pathname is absolute, with system-dependent results.
## On UNIX systems, a pathname is absolute if its prefix is "/".
## On Windows systems, a pathname is absolute if its prefix is a drive letter
## followed by "\\", or if its prefix is "\\\\".
## Based on original work by Henrik Bengtsson.
.isAbsolutePathname <- function(pathname) {
    ## Check arguments
    stopifnot(is.character(pathname) && length(pathname) == 1)

    ## Begin processing
    if (!nzchar(pathname)) {
        return(FALSE)
    }

    absolute <- switch(EXPR=.Platform$OS.type,
                       unix={
                           ## Tilde expansion
                           regexpr("^~", pathname) != -1
                       },
                       windows={
                           ## Drive paths
                           regexpr("^[A-Za-z]:(/|\\\\)", pathname) != -1 ||
                           ## Network paths
                           regexpr("^\\\\", pathname) != -1
                       },
                       stop(sprintf("unrecognized operating system family %s",
                                    sQuote(.Platform$OS.type))))

    if (absolute) {
        return(TRUE)
    }

    ## Split pathname into components
    components <- strsplit(pathname, split="[/\\]")[[1]]
    if (length(components) == 0) {
        return(FALSE)
    }

    absolute <- components[1] == ""
}


##-----------------------------------------------------------------------------
.isProbability <- function(x) {
    isTRUE(is.numeric(x) && (x >= 0 && x <= 1))
}


##-----------------------------------------------------------------------------
.isPackageInstalled <- function(pkgname) {
    stopifnot(is.character(pkgname) && length(pkgname) == 1)
    nzchar(system.file(package=pkgname))
}


##-----------------------------------------------------------------------------
## Specifies measures used for determining location on lysate array.
.locationColnames <- function() {
    return(c("Main.Row",
             "Main.Col",
             "Sub.Row",
             "Sub.Col"))
}


##-----------------------------------------------------------------------------
.mkPlotTitle <- function(maintext,
                         antibody) {
    ## Check arguments
    stopifnot(is.character(maintext) && length(maintext) == 1)
    stopifnot(is.character(antibody) && length(antibody) == 1)

    ## Begin processing
    main <- sprintf("%s:  %s", maintext, antibody)
}


##-----------------------------------------------------------------------------
## Returns logical value indicating whether code is running as a package.
packaged <- function() {
    return(getPackageName() != ".GlobalEnv")
}


##-----------------------------------------------------------------------------
## Get version of R for which the package was built
.pkgRversion <- function(pkgname) {
    ## Check arguments
    stopifnot(is.character(pkgname) && length(pkgname) == 1)

    ## Begin processing
    meta <- packageDescription(pkgname)
    sub("\\.[[:digit:]];.*$", "", substring(meta$Built, 3))
}


##-----------------------------------------------------------------------------
## Returns a POSIX portable filename from its input. Filenames should be
## constructed from the portable filename character set because the use of
## other characters can be confusing or ambiguous in certain contexts. The
## hyphen character shall not be used as the first character of a portable
## filename. Uppercase and lowercase letters shall retain their unique
## identities between conforming implementations. See reference URLs:
## http://opengroup.org/onlinepubs/000095399/basedefs/xbd_chap04.html#tag_04_06
## http://opengroup.org/onlinepubs/000095399/basedefs/xbd_chap03.html#tag_03_276
.portableFilename <- function(filename) {
    ## Check arguments
    stopifnot(is.character(filename) && length(filename) == 1)
    stopifnot(nzchar(filename))

    ## Begin processing

    ## Substitute hyphen for delimiters and underscore for anything else
    openclose.re <- "[][(){}]"             ## brackets, parentheses, brackets
    nonportable.re <- "[^0-9A-Za-z._-]"    ## nonportable characters
    hyphenfirstchar.re <- "^-"
    sub(hyphenfirstchar.re, "_",
        gsub(nonportable.re, "_",
             gsub(openclose.re, "-", filename)))
}


##-----------------------------------------------------------------------------
## Capitalizes string by replacing first character with upper case, and the
## rest with lowercase. Arguments first and last indicate range of string on
## which to operate.
.totitle <- function(s, first=1, last=nchar(s)) {
    ## Check arguments
    if (!is.character(s)) {
        s <- as.character(s)
    }
    stopifnot(is.numeric(first) && length(first) == 1)
    stopifnot(is.numeric(last) && length(last) == 1)

    ## Begin processing
    begin <- if (first > 1) {
                 substring(s, 1, first-1)
             } else {
                 ""
             }
    subst <- substring(s, first, last)
    end <- if (last < nchar(s)) {
               substring(s, last+1, nchar(s))
           } else {
               ""
           }

    paste(begin,
          toupper(substring(subst, 1, 1)),
          tolower(substring(subst, 2)),
          end,
          sep="")
}


##-----------------------------------------------------------------------------
## Returns TRUE if path represents a directory; otherwise, FALSE.
dir.exists <- function(path) {
    ## Check arguments
    stopifnot(is.character(path) && length(path) == 1)

    ##-------------------------------------------------------------------------
    dirTest <- function(x) {
        !is.na(isdir <- file.info(x)$isdir) & isdir
    }

    ## Begin processing
    file.exists(path) && dirTest(path)
}


##-----------------------------------------------------------------------------
## Returns TRUE if directory is writable; otherwise, FALSE.
dir.writable <- function(path) {
    ## Check arguments
    stopifnot(is.character(path) && length(path) == 1)

    ##-------------------------------------------------------------------------
    ## Had issues with file.access() reporting no write access on directories
    ## served from our network servers (Likewise-enabled) mounted by PCs.
    ## Returns TRUE if throwaway file can be created; otherwise, FALSE.
    canwrite <- function(path) {
        tryCatch({
                fn <- tempfile("sctest", path)
                fh <- suppressWarnings(file(fn, open="w"))

                TRUE
            },
            error = function(ex) {
                FALSE
            },
            finally = {
                if (exists("fh")) {
                    if (isOpen(fh)) {
                        close(fh)
                    }
                    rm(fh)
                    file.remove(fn)
                }
            })
    }


    ## Begin processing
    file.info(path)$isdir &&
    ((file.access(path, mode=2) == 0) || canwrite(path))
}


##-----------------------------------------------------------------------------
## Specifies names of possible stages as set by process monitoring code. If a
## new capability is added to the package, so should an associated stage.
getStages <- function() {
    ## :NOTE: SuperCurveGUI::setStages() uses these EXACT list names...
    stagesList <- list(input    = "Data Input",
                       spatial  = "Spatial Adj",
                       prefitqc = "Pre-Fit QC",
                       fit      = "Curve Fitting",
                       graph    = "Graphing")
    stages <- as.character(stagesList)
    names(stages) <- names(stagesList)

    return(stages)
}


##-----------------------------------------------------------------------------
## Returns data.frame containing RPPA data merged with design layout.
.mergeDataWithLayout <- function(rppadata, layout) {
    ## Check arguments
    if (is.RPPA(rppadata)) {
        rppadata <- rppadata@data
    }
    if (is.RPPADesign(layout)) {
        layout <- layout@layout
    }
    stopifnot(is.data.frame(rppadata))
    stopifnot(is.data.frame(layout))

    ## Begin processing
    #merge.by <- c(.locationColnames(), "Sample")
    #merged.df <- merge(rppadata, layout, by=merge.by)
    merged.df <- merge(rppadata, layout, sort=FALSE)
    merged.df[do.call(order, merged.df), ]
}


##-----------------------------------------------------------------------------
## A version of all.equal() for the slots of an object
slot.all.equal <- function(x,
                           y,
                           ...) {
    ## Check arguments
    stopifnot(isS4(x))
    stopifnot(isS4(y))

    ## Begin processing
    msg <- NULL
    slotnames <- slotNames(x)
    for (slotname in slotnames) {
        aeq <- all.equal(slot(x, slotname),
                         slot(y, slotname),
                         ...)
        if (!isTRUE(aeq)) {
            msg <- c(msg, paste("slot ", sQuote(slotname), ": ", aeq, sep=''))
        }
    }

    ## Pass or fail?
    if (is.null(msg)) {
        TRUE
    } else {
        msg
    }
}

