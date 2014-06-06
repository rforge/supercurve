###
### SUPERCURVEGUI.R
###

require(tcltk) || stop("tcltk support is missing")
require(SuperCurve)

tclpackage.require("Tcl", "8.4")               # Requires Tcl 8.4 or later


##
## Module Variables
##
.GuiEnv <- new.env(hash=TRUE)                  # Private environment
attr(.GuiEnv, "name") <- "GlobalVars"


##
## Methods
##


##-----------------------------------------------------------------------------
## Returns logical value indicating whether debugging support is enabled.
.appDebugEnabled <- function() {
    return(TRUE)
}


##-----------------------------------------------------------------------------
## Prints message if debugging support is enabled.
.appDebugStr <- function(str) {
    stopifnot(is.character(str) && length(str) == 1)

    if (.appDebugEnabled()) {
        cat(str, "\n")
        flush.console()
    }
}


##-----------------------------------------------------------------------------
## Prints name of function being evaluated if debugging support is enabled.
.appEntryStr <- function(fname) {

    if (.appDebugEnabled()) {
        if (missing(fname)) {
            ## :NOTE: No silver bullet - don't trust this without testing!
            value <- deparse(sys.call(-1)[[1]])
            fname <- if (length(value) == 1) {
                         value
                     } else {
                         "<<unknown>>"
                     }
        }

        .appDebugStr(fname)
    }
}


##-----------------------------------------------------------------------------
## Causes error message to be displayed if expr does not evaluate successfully.
Try <- function(expr) {
    if (data.class(result <- try(expr, TRUE)) == "try-error") {
        showerror(message=as.character(result),
                  parent=getenv("toplevel"),
                  title="Error Occurred!")
    }

    return(result)
}


##-----------------------------------------------------------------------------
## Returns private environment for storing application's global variables.
guienv <- function() {
    return(.GuiEnv)
}


##-----------------------------------------------------------------------------
## Get variable from private environment.
getenv <- function(name) {
    stopifnot(is.character(name) && length(name) == 1)

    Try(get(name, envir=guienv()))
}


##-----------------------------------------------------------------------------
## Update value of variable in private environment.
setenv <- function(name,
                   value) {
    stopifnot(is.character(name) && length(name) == 1)

    Try(assign(name, value, envir=guienv()))
}


##-----------------------------------------------------------------------------
## Specify whether the document is "dirty".
setDocumentEdited <- function(value) {
    stopifnot(is.logical(value) && length(value) == 1)

    if (value) {
        evalq(dirty <- TRUE, envir=guienv())
    } else {
        evalq(dirty <- FALSE, envir=guienv())
    }
}


##-----------------------------------------------------------------------------
## Returns TRUE if document is "dirty" (has unsaved changes).
isDocumentEdited <- function() {
    as.logical(getenv("dirty"))
}


##-----------------------------------------------------------------------------
## Save global variables in private environment.
initGlobals <- function(glist) {
    .appEntryStr("initGlobals")
    stopifnot(is.list(glist))

    sapply(seq_along(glist),
           function(i, ll) {
               setenv(names(ll)[i], ll[[i]])
           },
           glist)

    if (.appDebugEnabled()) {
        show(objects(envir=guienv()))
    }
}


##-----------------------------------------------------------------------------
## Initialize the Tk option database with application defaults.
initOptions <- function(olist) {
    stopifnot(is.list(olist))

    sapply(seq_along(olist),
           function(i, ll) {
               rsrc <- names(ll)[i]
               value <- ll[[i]]
               priority <- "startupFile"
               optiondb_add(rsrc, value, priority)
           },
           olist)
}


##-----------------------------------------------------------------------------
## Returns directory user selected via dialog
.chooseDirectory <- function(title,
                             initialdir) {
    if (missing(initialdir) || is.null(initialdir)) {
        initialdir <- getwd()
    }

    stopifnot(is.character(title)      && length(title) == 1)
    stopifnot(is.character(initialdir) && length(initialdir) == 1)

    directory <- tclvalue(tkchooseDirectory(initialdir=initialdir,
                                            title=title))
    if (nzchar(directory)) {
        directory
    } else {
        NULL
    }
}


##-----------------------------------------------------------------------------
## Returns directory of file user selected via dialog
.chooseDirectoryOfFile <- function(title,
                                   initialdir,
                                   filetypes) {
    if (missing(initialdir) || is.null(initialdir)) {
        initialdir <- getwd()
    }
    if (missing(filetypes)) {
        filetypes <- "{{All files} * }"
    }

    stopifnot(is.character(title)      && length(title) == 1)
    stopifnot(is.character(initialdir) && length(initialdir) == 1)
    stopifnot(is.character(filetypes)  && length(filetypes) == 1)

    filename <- tclvalue(tkgetOpenFile(filetypes=filetypes,
                                       initialdir=initialdir,
                                       title=title))
    if (nzchar(filename)) {
        dirname(filename)
    } else {
        NULL
    }
}


##-----------------------------------------------------------------------------
## Returns file user selected via dialog
.chooseFile <- function(title,
                        initialdir,
                        filetypes,
                        defaultextension) {
    if (missing(initialdir) || is.null(initialdir)) {
        initialdir <- getwd()
    }
    if (missing(filetypes)) {
        filetypes <- "{{All files} * }"
    }
    if (missing(defaultextension)) {
        defaultextension <- ""
    }

    stopifnot(is.character(title)      && length(title) == 1)
    stopifnot(is.character(initialdir) && length(initialdir) == 1)
    stopifnot(is.character(filetypes)  && length(filetypes) == 1)

    filename <- tclvalue(tkgetOpenFile(defaultextension=defaultextension,
                                       filetypes=filetypes,
                                       initialdir=initialdir,
                                       parent=getenv("toplevel"),
                                       title=title))
    if (nzchar(filename)) {
        filename
    } else {
        NULL
    }
}


##-----------------------------------------------------------------------------
.chooseQuantificationDirectory <- function(initialdir) {
    .chooseDirectoryOfFile("Select quantification directory",
                           initialdir,
                           "{{Quantification Files} {.txt}} {{All files} * }")
}


##-----------------------------------------------------------------------------
.chooseImageDirectory <- function(initialdir) {
    .chooseDirectoryOfFile("Select image directory",
                           initialdir,
                           "{{TIFF Files} {.tif .tiff}} {{All files} * }")
}


##-----------------------------------------------------------------------------
.chooseOutputDirectory <- function(initialdir) {
    .chooseDirectory("Select output directory",
                     initialdir)
}


##-----------------------------------------------------------------------------
.chooseAliasFile <- function(initialdir) {
    .chooseFile("Select SuperCurve alias information datafile",
                initialdir,
                defaultextension=".tsv",
                "{{TSV Files} {.tsv}} {{All files} * }")
}


##-----------------------------------------------------------------------------
.chooseAntibodyFile <- function(initialdir) {
    .chooseFile("Select SuperCurve antibody assay datafile",
                initialdir,
                defaultextension=".tsv",
                "{{TSV Files} {.tsv}} {{All files} * }")
}


##-----------------------------------------------------------------------------
.chooseDesignFile <- function(initialdir) {
    .chooseFile("Select SuperCurve slide design datafile",
                initialdir,
                defaultextension=".tsv",
                "{{TSV Files} {.tsv}} {{All files} * }")
}


##-----------------------------------------------------------------------------
.chooseSettingsFile <- function(initialdir) {
    .chooseFile("Select SuperCurve settings datafile",
                initialdir,
                defaultextension=".RData",
                "{{R Data Files} {.rda .RData}} {{All files} * }")
}


##-----------------------------------------------------------------------------
getImageFilenames <- function(path) {
    ## Check arguments
    stopifnot(nzchar(path))

    tif.re <- ".*[tT][iI][fF]{1,2}$"
    list.files(path=path, pattern=tif.re)
}


##-----------------------------------------------------------------------------
getQuantificationFilenames <- function(path) {
    ## Check arguments
    stopifnot(nzchar(path))

    ## Assumes all .txt files in the directory are slides
    txt.re <- ".*[tT][xX][tT]$"
    list.files(path=path, pattern=txt.re)
}


##-----------------------------------------------------------------------------
chooseQuantificationDirectoryWithRestarts <- function(initialdir) {
    withRestarts({
            dirname <- .chooseQuantificationDirectory(initialdir)

            if (is.null(dirname)) {
                #stop("user canceled selection")
                message("user canceled selection")
            }

            ## Does directory contain text files at all?
            files <- getQuantificationFilenames(dirname)
            if (length(files) == 0) {
                stop(sprintf("directory %s contains no text files",
                             sQuote(dirname)))
            }

            ## Does directory really contain quantification files?
            file1 <- files[1]
            tryCatch(SuperCurve::RPPA(file1, path=dirname),
                     error=function(cond) {
                         stop(sprintf("directory %s contains invalid file: %s",
                                      dQuote(dirname),
                                      conditionMessage(cond)))
                     })

            dirname
        },
        retry=function() NULL)
}


##-----------------------------------------------------------------------------
chooseImageDirectoryWithRestarts <- function(initialdir) {
    withRestarts({
            dirname <- .chooseImageDirectory(initialdir)

            if (is.null(dirname)) {
                stop("user canceled selection")
            }

            ## Does directory contain image files at all?
            files <- getImageFilenames(dirname)
            if (length(files) == 0) {
                stop(sprintf("directory %s contains no image files",
                             sQuote(dirname)))
            }

            dirname
        },
        retry=function() NULL)
}


##-----------------------------------------------------------------------------
chooseOutputDirectoryWithRestarts <- function(initialdir) {
    withRestarts({
            dirname <- .chooseOutputDirectory(initialdir)

            if (is.null(dirname)) {
                stop("user canceled selection")
            }

            ## Is directory writable?
            if (!(file.access(dirname, mode=2) == 0)) {
                stop(sprintf("directory %s is not writable",
                             sQuote(dirname)))
            }

            dirname
        },
        retry=function() NULL)
}


##-----------------------------------------------------------------------------
updateMeasuresOptionMenu <- function() {

    ##-------------------------------------------------------------------------
    getRelevantColnamesFromQuantificationFile <- function(txtdir) {
        ## Check arguments
        stopifnot(is.character(txtdir) && length(txtdir) == 1)
        stopifnot(nzchar(txtdir))

        ## Get first filename of slides to process
        slideFilename1 <- getQuantificationFilenames(txtdir)[1]
        rppa.df <- SuperCurve::RPPA(slideFilename1, txtdir)@data

        ## Keep only columns with numeric data
        x.numeric <- sapply(rppa.df, is.numeric)
        rppa.df <- rppa.df[x.numeric]

        ## Toss columns with location data
        locationColnames <- SuperCurve:::.locationColnames()
        x.location <- colnames(rppa.df) %in% locationColnames

        colnames(rppa.df)[!x.location]
    }


    ## Begin processing
    txtdir <- tclvalue(getenv("txtdir.var"))
    measures <- tryCatch(getRelevantColnamesFromQuantificationFile(txtdir),
                         error=function(cond) {
                             ## Provide basic defaults
                             c("Mean.Net", "Mean.Total")
                         })


    measure.var <- getenv("measure.var")
    curr.value <- tclvalue(measure.var)

    measure.optmenu <- getenv("measure.optmenu")
    measure.popup <- paste(measure.optmenu$ID, "menu", sep=".") # :HACK:

    tkdelete(measure.popup, 0, "end")
    sapply(measures,
           function(measure, popupmenu) {
               tkinsert(popupmenu, "end",
                        "radiobutton",
                        label=measure,
                        variable=getenv("measure.var"))
           },
           popupmenu=measure.popup)

    if (!(curr.value %in% measures)) {
        new.value <- measures[1]
        tclvalue(measure.var) <- new.value

        msg <- sprintf(paste("Measure unavailable in quantification files.",
                             "Substituting %s for %s.",
                             "If unacceptable, select a different measure",
                             "on the FitParams panel instead.",
                             sep="\n"),
                       sQuote(new.value),
                       sQuote(curr.value))

        showwarning(message=msg,
                    parent=getenv("toplevel"),
                    title="Spot Measure Unavailable!")
    }
}


##-----------------------------------------------------------------------------
loadSettingsWithRestarts <- function(pathname) {
    stopifnot(is.character(pathname) && length(pathname) == 1)

    withRestarts({
            ## Verify file contents in private environment
            local({
                load(pathname)
                ## XDR datafiles created by this package will contain a
                ## variable named 'settings'.
                stopifnot(exists("settings"))
                stopifnot(SuperCurve:::is.SuperCurveSettings(settings))
                settings
            })
        },
        retry=function() NULL)
}


##-----------------------------------------------------------------------------
reloadInterface <- function(settings) {
    ## Check arguments
    stopifnot(SuperCurve:::is.SuperCurveSettings(settings))

    ##-------------------------------------------------------------------------
    ## Updates values displayed by UI and associated globals
    reloadValue <- function(varname,
                            envir) {
        ## Check arguments
        stopifnot(is.character(varname) && length(varname) == 1)
        stopifnot(is.environment(envir))

        ## Get value of variable
        value <- get(varname, envir=envir, inherits=FALSE)
        if (is.null(value)) {
            value <- ""
        }

        ## Get global variable and its value
        envvarname <- paste(varname, "var", sep=".")
        envvar <- getenv(envvarname)

        ## Update user interface
        tclvalue(envvar) <- value           ## Updates UI
        setenv(envvarname, envvar)          ## Updates global
        tclupdate("idletasks")
    }


    ##-------------------------------------------------------------------------
    ## Updates label displayed by UI and associated global
    reloadLabel <- function(varname,
                            envir) {
        ## Check arguments
        stopifnot(is.character(varname) && length(varname) == 1)
        stopifnot(is.environment(envir))

        ## Get value of variable
        value <- get(varname, envir=envir, inherits=FALSE)
        if (is.null(value)) {
            value <- ""
        }

        ## Get associated label for value
        label <- SuperCurve::getRegisteredModelLabel(value)
        if (is.null(label)) {
            stop(sprintf("fit model value %s has no registered match",
                         dQuote(value)))
        }

        ## Get global variable, but assign label as its value
        envvarname <- paste(varname, "label", "var", sep=".")
        envvar <- getenv(envvarname)

        ## Update user interface
        tclvalue(envvar) <- label           ## Updates UI
        setenv(envvarname, envvar)          ## Updates global
        tclupdate("idletasks")
    }


    ## Put values from settings into private environment
    loadenv <- new.env(hash=TRUE)
    attr(loadenv, "name") <- "Settings"

    local({
              txtdir         <- as(settings@txtdir, "character")
              imgdir         <- as(settings@imgdir, "character")
              outdir         <- as(settings@outdir, "character")
              software       <- settings@software
              antibodyfile   <- settings@antibodyfile
              aliasfile      <- settings@designparams@aliasfile
              designfile     <- settings@designparams@designfile
              grouping       <- settings@designparams@grouping
              ordering       <- settings@designparams@ordering
              center         <- as.character(settings@designparams@center)
              measure        <- settings@fitparams@measure
              model          <- settings@fitparams@model
              method         <- settings@fitparams@method
              trim           <- as.character(settings@fitparams@trim)
              ci             <- as.character(settings@fitparams@ci)
              ignoreNegative <- as.character(settings@fitparams@ignoreNegative)
              prefitqc       <- as.character(settings@doprefitqc)
              if (!is.null(settings@spatialparams)) {
                  spatial    <- as.character(TRUE)
                  cutoff     <- as.character(settings@spatialparams@cutoff)
                  gamma      <- as.character(settings@spatialparams@gamma)
                  k          <- as.character(settings@spatialparams@k)
                  plotSurface<- as.character(settings@spatialparams@plotSurface)
              } else {
                  formal.args <- formals(SuperCurve:::spatialCorrection)

                  spatial    <- as.character(FALSE)
                  cutoff     <- as.character(eval(formal.args$cutoff))
                  k          <- as.character(eval(formal.args$k))
                  gamma      <- as.character(eval(formal.args$gamma))
                  plotSurface<- as.character(eval(formal.args$plotSurface))

                  rm(formal.args)
              }
        },
        envir=loadenv)

    ## Load values from private environment
    reloadLabel("model", loadenv)
    remove(model, envir=loadenv)

    sapply(objects(envir=loadenv),
           reloadValue,
           envir=loadenv)

    tclafter.idle(updateMeasuresOptionMenu)
    setDocumentEdited(FALSE)
}


##-----------------------------------------------------------------------------
## Create action area for frame.
actionArea <- function(parent) {
    stopifnot(is.tkwin(parent))

    tkframe(parent, class="ActionArea")
}


##-----------------------------------------------------------------------------
## Create command area for frame.
commandArea <- function(parent) {
    stopifnot(is.tkwin(parent))

    tkframe(parent, class="CommandArea")
}


##-----------------------------------------------------------------------------
## Create banner frame for command area of left frame.
bannerFrame <- function(parent) {
    stopifnot(is.tkwin(parent))

    tkframe(parent, class="BannerFrame")
}


##-----------------------------------------------------------------------------
createOptionMenu <- function(parent,
                             varname,
                             values) {
    ## Check arguments
    stopifnot(is.tkwin(parent))
    stopifnot(is.tclVar(varname))
    stopifnot(length(values) > 1)

    ## Begin processing
    optmenu.args <- c(list(parent=parent,
                           variable=varname),
                      as.character(values))
    optmenu <- do.call("tkOptionMenu",
                       optmenu.args)

    return(optmenu)
}


##-----------------------------------------------------------------------------
displayErrorAndAllowRetry <- function(msg, e) {
    stopifnot(is.character(msg) && length(msg) == 1)
    stopifnot(inherits(e, "error"))

    message <- sprintf("%s\nReason: %s", msg, conditionMessage(e))
    if (askretrycancel(default="retry",
                       icon="error",
                       message=message,
                       parent=getenv("toplevel"),
                       title="Error Occurred!")) {
        invokeRestart("retry")
    } else {
        cat("**user canceled selection**", "\n")
    }
}


##-----------------------------------------------------------------------------
createPathnamesPanel <- function(parent) {
    ## Check arguments
    stopifnot(is.tkwin(parent))


    ##-------------------------------------------------------------------------
    ## If the chosen quantification directory fits "known" hierarchy, update
    ## its sibling directories appropriately if they are currently undefined.
    updateSiblingDirs <- function() {
        txtdir <- tclvalue(getenv("txtdir.var"))
        parent.txtdir <- dirname(txtdir)

        imgdir.var <- getenv("imgdir.var")
        if (!nzchar(tclvalue(imgdir.var))) {
            siblingdir <- file.path(parent.txtdir, "tif")
            if (dir.exists(siblingdir)) {
                tclvalue(imgdir.var) <- siblingdir
                setenv("imgdir.var", imgdir.var)
            }
        }

        outdir.var <- getenv("outdir.var")
        if (!nzchar(tclvalue(outdir.var))) {
            siblingdir <- file.path(parent.txtdir, "results")
            if (dir.exists(siblingdir)) {
                tclvalue(outdir.var) <- siblingdir
                setenv("outdir.var", outdir.var)
            }
        }

        invisible(NULL)
    }


    ##-------------------------------------------------------------------------
    chooseQuantificationDirectoryCB <- function() {
        txtdir.var <- getenv("txtdir.var")
        initialdir <- tclvalue(txtdir.var)
        withCallingHandlers({
                txtdir <- initialdir
                repeat {
                    txtdir <- chooseQuantificationDirectoryWithRestarts(initialdir)
                    if (!is.null(txtdir)) {
                        tclvalue(txtdir.var) <- txtdir      ## Updates UI
                        setenv("txtdir.var", txtdir.var)    ## Updates global

                        ## Update other parts of UI dependent on this selection
                        tclafter.idle(updateSiblingDirs)
                        tclafter.idle(updateMeasuresOptionMenu)

                        ## Success
                        break
                    }
                }
            },
            error=function(cond) {
                msg <- "Failed to select quantification directory"
                displayErrorAndAllowRetry(msg, cond)
            })
    }


    ##-------------------------------------------------------------------------
    chooseImageDirectoryCB <- function() {
        imgdir.var <- getenv("imgdir.var")
        initialdir <- tclvalue(imgdir.var)
        withCallingHandlers({
                repeat {
                    imgdir <- chooseImageDirectoryWithRestarts(initialdir)
                    if (!is.null(imgdir)) {
                        tclvalue(imgdir.var) <- imgdir      ## Updates UI
                        setenv("imgdir.var", imgdir.var)    ## Updates global
                        break
                    }
                }
            },
            error=function(cond) {
                msg <- "Failed to select image directory"
                displayErrorAndAllowRetry(msg, cond)
            })
    }


    ##-------------------------------------------------------------------------
    chooseOutputDirectoryCB <- function() {
        outdir.var <- getenv("outdir.var")
        initialdir <- tclvalue(outdir.var)
        withCallingHandlers({
                repeat {
                    outdir <- chooseOutputDirectoryWithRestarts(initialdir)
                    if (!is.null(outdir)) {
                        tclvalue(outdir.var) <- outdir      ## Updates UI
                        setenv("outdir.var", outdir.var)    ## Updates global
                        break
                    }
                }
            },
            error=function(cond) {
                msg <- "Failed to select output directory"
                displayErrorAndAllowRetry(msg, cond)
            })
    }


    ##-------------------------------------------------------------------------
    chooseAliasFileCB <- function() {
        txtdir <- tclvalue(getenv("txtdir.var"))
        aliasfile.var <- getenv("aliasfile.var")

        aliasfile <- .chooseAliasFile(txtdir)
        if (!is.null(aliasfile)) {
            tclvalue(aliasfile.var) <- aliasfile
        } else {
            cat("**user canceled alias file selection**", "\n")
        }
    }


    ##-------------------------------------------------------------------------
    chooseAntibodyFileCB <- function() {
        txtdir <- tclvalue(getenv("txtdir.var"))
        antibodyfile.var <- getenv("antibodyfile.var")

        antibodyfile <- .chooseAntibodyFile(txtdir)
        if (!is.null(antibodyfile)) {
            tclvalue(antibodyfile.var) <- antibodyfile
        } else {
            cat("**user canceled antibody file selection**", "\n")
        }
    }


    ##-------------------------------------------------------------------------
    chooseDesignFileCB <- function() {
        txtdir <- tclvalue(getenv("txtdir.var"))
        designfile.var <- getenv("designfile.var")

        designfile <- .chooseDesignFile(txtdir)
        if (!is.null(designfile)) {
            tclvalue(designfile.var) <- designfile
        } else {
            cat("**user canceled design file selection**", "\n")
        }
    }


    ## Begin processing
    tkpack(tkframe(parent,
                   class="Spacing"),
           pady="3m")

    dir.entry.width <- as.integer(50)
    file.entry.width <- as.integer(40)
    ellipsis <- "..."
    required <- "requiredImage"    ## Must be same as in supercurveGUI()

    ## Quantification directory
    txtdir.label <- tklabel(parent,
                            compound="right",
                            image=required,
                            text="Quantification Directory")
    txtdir.frame <- tklabelframe(parent,
                                 labelwidget=txtdir.label)
    {
        txtdir.entry <- tkentry(txtdir.frame,
                                textvariable=getenv("txtdir.var"),
                                width=dir.entry.width)

        txtdir.button <- tkbutton(txtdir.frame,
                                  command=chooseQuantificationDirectoryCB,
                                  text=ellipsis)
        tkgrid(txtdir.entry,
               txtdir.button)
        tkgrid.configure(txtdir.entry,
                         sticky="e")
        tkgrid.configure(txtdir.button,
                         sticky="w")
    }

    ## Create dummy frame
    filearea.frame <- tkframe(parent)
    spacer.frame <- tkframe(filearea.frame,
                            class="EmptySpace")

    ## Create input section for 'antibodyfile' argument
    antibodyfile.frame <- tklabelframe(filearea.frame,
                                       text="Antibody File")
    {
        antibodyfile.entry <- tkentry(antibodyfile.frame,
                                      textvariable=getenv("antibodyfile.var"),
                                      width=file.entry.width)

        antibodyfile.button <- tkbutton(antibodyfile.frame,
                                        command=chooseAntibodyFileCB,
                                        text=ellipsis)
        tkgrid(antibodyfile.entry,
               antibodyfile.button)
        tkgrid.configure(antibodyfile.entry,
                         sticky="e")
        tkgrid.configure(antibodyfile.button,
                         sticky="w")
    }

    ## Create input section for 'aliasfile' argument
    aliasfile.frame <- tklabelframe(filearea.frame,
                                    text="Alias File")
    {
        aliasfile.entry <- tkentry(aliasfile.frame,
                                   textvariable=getenv("aliasfile.var"),
                                   width=file.entry.width)

        aliasfile.button <- tkbutton(aliasfile.frame,
                                     command=chooseAliasFileCB,
                                     text=ellipsis)
        tkgrid(aliasfile.entry,
               aliasfile.button)
        tkgrid.configure(aliasfile.entry,
                         sticky="e")
        tkgrid.configure(aliasfile.button,
                         sticky="w")
    }

    ## Create input section for 'designfile' argument
    designfile.label <- tklabel(parent,
                                compound="right",
                                image=required,
                                text="Slide Design File")
    designfile.frame <- tklabelframe(filearea.frame,
                                     labelwidget=designfile.label)
    {
        designfile.entry <- tkentry(designfile.frame,
                                    textvariable=getenv("designfile.var"),
                                    width=file.entry.width)

        designfile.button <- tkbutton(designfile.frame,
                                      command=chooseDesignFileCB,
                                      text=ellipsis)
        tkgrid(designfile.entry,
               designfile.button)
        tkgrid.configure(designfile.entry,
                         sticky="e")
        tkgrid.configure(designfile.button,
                         sticky="w")
    }

    ## Image directory
    imgdir.frame <- tklabelframe(parent,
                                 text="Image Directory")
    {
        imgdir.entry <- tkentry(imgdir.frame,
                                textvariable=getenv("imgdir.var"),
                                width=dir.entry.width)

        imgdir.button <- tkbutton(imgdir.frame,
                                  command=chooseImageDirectoryCB,
                                  text=ellipsis)
        tkgrid(imgdir.entry,
               imgdir.button)
        tkgrid.configure(imgdir.entry,
                         sticky="e")
        tkgrid.configure(imgdir.button,
                         sticky="w")
    }

    ## Output directory
    outdir.label <- tklabel(parent,
                            compound="right",
                            image=required,
                            text="Output Directory")
    outdir.frame <- tklabelframe(parent,
                                 labelwidget=outdir.label)
    {
        outdir.entry <- tkentry(outdir.frame,
                                textvariable=getenv("outdir.var"),
                                width=dir.entry.width)

        outdir.button <- tkbutton(outdir.frame,
                                  command=chooseOutputDirectoryCB,
                                  text=ellipsis)
        tkgrid(outdir.entry,
               outdir.button)
        tkgrid.configure(outdir.entry,
                         sticky="e")
        tkgrid.configure(outdir.button,
                         sticky="w")
    }

    ## Manage
    tkpack(spacer.frame,
           side="left",
           fill="y")
    tkpack(antibodyfile.frame,
           side="top",
           fill="x")
    tkpack(aliasfile.frame,
           fill="x")
    tkpack(designfile.frame,
           side="bottom",
           fill="x")

    tkpack(txtdir.frame,
           filearea.frame,
           imgdir.frame,
           outdir.frame,
           padx="3m")
}


##-----------------------------------------------------------------------------
createDesignParamsPanel <- function(parent) {
    ## Check arguments
    stopifnot(is.tkwin(parent))

    ## Begin processing
    formal.args <- formals(SuperCurve::RPPADesignParams)
    formal.args$software <- local({
                                readmethod.re <- "read\\."
                                methods <- ls(pattern=readmethod.re,
                                              env=getNamespace("SuperCurve"))
                                sub(readmethod.re, "", methods)
                            })
    txtdir <- tclvalue(getenv("txtdir.var"))

    ## Prepare possible input values
    grouping.arg <- list(default="bySample",
                         values=eval(formal.args$grouping))
    ordering.arg <- list(default="decreasing",
                         values=eval(formal.args$ordering))
    center.arg <- list(default=as.character(FALSE),
                       values=as.character(c(TRUE, FALSE)))
    software.arg <- local({
        rsrcClass <- "Software"
        rsrcName <- tolower(rsrcClass)
        uservalue <- tclvalue(optiondb_get(rsrcName=rsrcName,
                                           rsrcClass=rsrcClass))
        value <- as.character(uservalue)
cat(sprintf("%s: [%s] -> [%s]", rsrcName, uservalue, value))

        values <- eval(formal.args$software)
        x.default <- match(formals(SuperCurve::RPPA)$software, values)
        stopifnot(!is.na(x.default))
cat(sprintf("\tdefault: [%s]", values[x.default]))

        x.match <- match(value, values, nomatch=x.default)
        default <- values[x.match]
cat(sprintf("\tfinal: [%s]\n", default))
        list(default=as.character(default),
             values=as.character(values))
    })

    ##-------------------------------------------------------------------------
    ## Weird but it won't handle 'tclvalue(getenv("grouping.var")) <- value'
    ## This achieves the same via temporary variable.
    setVariable <- function(x, value) {
        stopifnot(is.tclVar(x))
        stopifnot(!missing(value))

        tclvalue(x) <- value
    }

## :NOPE: Works but painful...
#    tcl("set", objects(unclass(getenv("grouping.var"))$env), grouping.arg$default)

## :BROKEN: Basic idea but doesn't work...
#    tclvalue(getenv("grouping.var")) <- grouping.arg$default   # "blockSample"

## Works via temporary...
    setVariable(getenv("grouping.var"), grouping.arg$default)
    setVariable(getenv("ordering.var"), ordering.arg$default)
    setVariable(getenv("center.var"), center.arg$default)
    setVariable(getenv("software.var"), software.arg$default)

    ## Design Parameters
    design.frame <- tklabelframe(parent,
                                 text="Design Parameters")
    {
        ## Create input section for 'grouping' argument
        grouping.label <- tklabel(design.frame,
                                  text="Grouping:")
        grouping.optmenu <- createOptionMenu(design.frame,
                                             getenv("grouping.var"),
                                             grouping.arg$values)

        ## Create input section for 'ordering' argument
        ordering.label <- tklabel(design.frame,
                                  text="Ordering:")
        ordering.optmenu <- createOptionMenu(design.frame,
                                             getenv("ordering.var"),
                                             ordering.arg$values)

        ## Create input section for 'center' argument
        center.label <- tklabel(design.frame,
                                text="Center?:")
        center.checkbox <- tkcheckbutton(design.frame,
                                         offvalue="FALSE",
                                         onvalue="TRUE",
                                         text="",
                                         variable=getenv("center.var"))

        ## Manage widgets
        tkgrid(grouping.label,
               grouping.optmenu)
        tkgrid(ordering.label,
               ordering.optmenu)
        tkgrid(center.label,
               center.checkbox)

        tkgrid.configure(grouping.label,
                         ordering.label,
                         center.label,
                         sticky="e")
        tkgrid.configure(grouping.optmenu,
                         ordering.optmenu,
                         center.checkbox,
                         sticky="w")
    }

    ## Read Method
    readmethod.frame <- tklabelframe(parent,
                                     text="Read Method")
    {
        ## Create input section for 'software' argument
        software.label <- tklabel(readmethod.frame,
                                  text="Software:")
        software.optmenu <- createOptionMenu(readmethod.frame,
                                             getenv("software.var"),
                                             software.arg$values)

        ## Manage widgets
        tkgrid(software.label,
               software.optmenu)

        tkgrid.configure(software.label,
                         sticky="e")
        tkgrid.configure(software.optmenu,
                         sticky="w")
    }

    ## Manage
    tkpack(design.frame,
           readmethod.frame,
           fill="x",
           padx="3m")
}


##-----------------------------------------------------------------------------
createFitParamsPanel <- function(parent) {
    ## Check arguments
    stopifnot(is.tkwin(parent))


    ##-------------------------------------------------------------------------
    getIntensityMeasures <- function(path) {
        ## Check arguments
        stopifnot(is.character(path) && length(path) == 1)

        ##---------------------------------------------------------------------
        getRelevantColnamesFromQuantificationFile <- function(txtdir) {
            ## Check arguments
            stopifnot(nzchar(txtdir))

            ## Get first filename of slides to process
            slideFilename1 <- getQuantificationFilenames(txtdir)[1]
            rppa.df <- SuperCurve::RPPA(slideFilename1, txtdir)@data

            ## Keep only columns with numeric data
            x.numeric <- sapply(rppa.df, is.numeric)
            rppa.df <- rppa.df[x.numeric]

            ## Toss columns with location data
            locationColnames <- SuperCurve:::.locationColnames()
            x.location <- colnames(rppa.df) %in% locationColnames

            colnames(rppa.df)[!x.location]
        }


        ## Begin processing
        measures <- tryCatch(getRelevantColnamesFromQuantificationFile(path),
                             error=function(cond) {
                                 ## Provide basic defaults
                                 c("Mean.Net", "Mean.Total")
                             })

        return(measures)
    }


    ## Begin processing
    formal.args <- formals(SuperCurve::RPPAFitParams)

    ## Prepare possible input values
    {
        txtdir <- tclvalue(getenv("txtdir.var"))
        measure.arg <- list(default="Mean.Net",
                            values=getIntensityMeasures(txtdir))
    }

    model.arg <- local({
        rsrcClass <- "FitModel"
        rsrcName <- tolower(rsrcClass)
        uservalue <- tclvalue(optiondb_get(rsrcName=rsrcName,
                                           rsrcClass=rsrcClass))
        value <- as.character(uservalue)
cat(sprintf("%s: [%s] -> [%s]", rsrcName, uservalue, value))

        values <- SuperCurve::getRegisteredModelKeys()
        x.default <- match(formal.args$model, values)
        stopifnot(!is.na(x.default))
cat(sprintf("\tdefault: [%s]", values[x.default]))

        x.match <- match(value, values, nomatch=x.default)
        default <- values[x.match]
cat(sprintf("\tfinal: [%s]\n", default))
        list(default=as.character(default),
             values=as.character(values))
    })

    method.arg <- local({
        rsrcClass <- "FitMethod"
        rsrcName <- tolower(rsrcClass)
        uservalue <- tclvalue(optiondb_get(rsrcName=rsrcName,
                                           rsrcClass=rsrcClass))
        value <- as.character(uservalue)
cat(sprintf("%s: [%s] -> [%s]", rsrcName, uservalue, value))

        values <- eval(formal.args$method)
        x.default <- 1
cat(sprintf("\tdefault: [%s]", values[x.default]))

        x.match <- match(value, values, nomatch=x.default)
        default <- values[x.match]
cat(sprintf("\tfinal: [%s]\n", default))
        list(default=as.character(default),
             values=as.character(values))
    })

    trim.arg <- list(default=eval(formal.args$trim))
    ci.arg <- list(default=as.character(FALSE),
                   values=as.character(c(TRUE, FALSE)))
    ignoreNegative.arg <- list(default=as.character(FALSE),
                               values=as.character(c(TRUE, FALSE)))

    ##-------------------------------------------------------------------------
    ## Weird but it won't handle 'tclvalue(getenv("grouping.var")) <- value'
    ## This achieves the same via temporary variable.
    setVariable <- function(x, value) {
        stopifnot(is.tclVar(x))
        stopifnot(!missing(value))

        tclvalue(x) <- value
    }

    setVariable(getenv("measure.var"), measure.arg$default)
    setVariable(getenv("method.var"), method.arg$default)
    setVariable(getenv("trim.var"), trim.arg$default)
    setVariable(getenv("ci.var"), ci.arg$default)
    setVariable(getenv("ignoreNegative.var"), ignoreNegative.arg$default)

    ## Create panel

    ## Fit Parameters
    fit.frame <- tklabelframe(parent,
                              text="Fit Parameters")
    {
        ## Create input section for 'measure' argument
        measure.label <- tklabel(fit.frame,
                                 text="Spot Measure:")
        measure.optmenu <- createOptionMenu(fit.frame,
                                            getenv("measure.var"),
                                            measure.arg$values)
        setenv("measure.optmenu", measure.optmenu)  # Save to update menu items

        ## Create input section for 'model' argument
        model.label <- tklabel(fit.frame,
                               text="Fit Model:")
        {
            model.arg.labels <- sapply(model.arg$values,
                                       SuperCurve::getRegisteredModelLabel)
            setVariable(getenv("model.label.var"),
                        model.arg.labels[model.arg$default])
            model.optmenu <- createOptionMenu(fit.frame,
                                              getenv("model.label.var"),
                                              model.arg.labels)
        }

        ## Create input section for 'method' argument
        method.label <- tklabel(fit.frame,
                                text="Fit Method:")
        method.optmenu <- createOptionMenu(fit.frame,
                                           getenv("method.var"),
                                           method.arg$values)

        ## Create input section for 'trim' argument
        trim.spinbox.min <- as.integer(0)
        trim.spinbox.max <- as.integer(12)
        trim.spinbox.width <- as.integer(10)

        trim.label <- tklabel(fit.frame,
                              text="Trim Level:")
        trim.spinbox <- tkspinbox(fit.frame,
                                  from=trim.spinbox.min,
                                  state="readonly",
                                  to=trim.spinbox.max,
                                  textvariable=getenv("trim.var"),
                                  width=trim.spinbox.width)

        ## Create input section for 'ci' argument
        ci.label <- tklabel(fit.frame,
                            text="Confidence Interval?:")
        ci.checkbox <- tkcheckbutton(fit.frame,
                                     offvalue="FALSE",
                                     onvalue="TRUE",
                                     text="",
                                     variable=getenv("ci.var"))

        ## Create input section for 'ignoreNegative' argument
        ignoreNegative.label <- tklabel(fit.frame,
                                        text="Ignore Negative?:")
        ignoreNegative.checkbox <- tkcheckbutton(fit.frame,
                                                 offvalue="FALSE",
                                                 onvalue="TRUE",
                                                 text="",
                                         variable=getenv("ignoreNegative.var"))

        ## Manage widgets
        tkgrid(measure.label,
               measure.optmenu)
        tkgrid(model.label,
               model.optmenu)
        tkgrid(method.label,
               method.optmenu)
        tkgrid(trim.label,
               trim.spinbox)
        tkgrid(ci.label,
               ci.checkbox)
        tkgrid(ignoreNegative.label,
               ignoreNegative.checkbox)

        tkgrid.configure(measure.label,
                         model.label,
                         method.label,
                         trim.label,
                         ci.label,
                         ignoreNegative.label,
                         padx=c("10m", "0"),
                         sticky="e")
        tkgrid.configure(measure.optmenu,
                         model.optmenu,
                         method.optmenu,
                         trim.spinbox,
                         ci.checkbox,
                         ignoreNegative.checkbox,
                         padx=c("0", "10m"),
                         sticky="w")
    }

    ## Manage
    tkpack(fit.frame,
           fill="x",
           padx="3m")
}


##-----------------------------------------------------------------------------
createSpatialAdjPanel <- function(parent) {
    ## Check arguments
    stopifnot(is.tkwin(parent))

    ## Begin processing
    formal.args <- formals(SuperCurve:::spatialCorrection)
    measure <- tclvalue(getenv("measure.var"))

    ## Prepare possible input values
    spatial.arg <- list(default=as.character(TRUE),
                        values=as.character(c(TRUE, FALSE)))
    cutoff.arg <- list(default=eval(formal.args$cutoff))
    k.arg <- list(default=eval(formal.args$k))
    gamma.arg <- list(default=eval(formal.args$gamma))
    plotSurface.arg <- list(default=as.character(FALSE),
                            values=as.character(c(TRUE, FALSE)))

    ##-------------------------------------------------------------------------
    ## Weird but it won't handle 'tclvalue(getenv("grouping.var")) <- value'
    ## This achieves the same via temporary variable.
    setVariable <- function(x, value) {
        stopifnot(is.tclVar(x))
        stopifnot(!missing(value))

        tclvalue(x) <- value
    }

## :NOPE: Works but painful...
#    tcl("set", objects(unclass(getenv("grouping.var"))$env), grouping.arg$default)

## :BROKEN: Basic idea but doesn't work...
#    tclvalue(getenv("grouping.var")) <- grouping.arg$default   # "blockSample"

## Works via temporary...
    setVariable(getenv("spatial.var"), spatial.arg$default)
    setVariable(getenv("cutoff.var"), cutoff.arg$default)
    setVariable(getenv("k.var"), k.arg$default)
    setVariable(getenv("gamma.var"), gamma.arg$default)
    setVariable(getenv("plotSurface.var"), plotSurface.arg$default)

    ## Spatial Adjustment
    spatial.checkbox <- tkcheckbutton(parent,
                                      offvalue="FALSE",
                                      onvalue="TRUE",
                                      text="Spatial Adjustment",
                                      variable=getenv("spatial.var"))
    spatial.frame <- tklabelframe(parent,
                                  labelwidget=spatial.checkbox)
    {
        ## Create input section for 'cutoff' argument
        cutoff.spinbox.incr <- as.numeric(0.05)
        cutoff.spinbox.min  <- as.numeric(0)
        cutoff.spinbox.max  <- as.numeric(1)
        cutoff.spinbox.width <- as.numeric(10)

        cutoff.label <- tklabel(spatial.frame,
                                text="Cutoff:")
        cutoff.spinbox <- tkspinbox(spatial.frame,
                                    format="%5.2f",
                                    from=cutoff.spinbox.min,
                                    increment=cutoff.spinbox.incr,
                                    state="readonly",
                                    to=cutoff.spinbox.max,
                                    textvariable=getenv("cutoff.var"),
                                    width=cutoff.spinbox.width)

        ## Create input section for 'k' argument
        k.label <- tklabel(spatial.frame,
                           text="k:")
        k.entry <- tkentry(spatial.frame,
                           textvariable=getenv("k.var"),
                           width=as.integer(12))

        ## Create input section for 'gamma' argument
        gamma.spinbox.incr <- as.numeric(0.05)
        gamma.spinbox.min  <- as.numeric(0)
        gamma.spinbox.max  <- as.numeric(2)
        gamma.spinbox.width <- as.numeric(10)

        gamma.label <- tklabel(spatial.frame,
                               text="Gamma:")
        gamma.spinbox <- tkspinbox(spatial.frame,
                                   format="%5.2f",
                                   from=gamma.spinbox.min,
                                   increment=gamma.spinbox.incr,
                                   state="readonly",
                                   to=gamma.spinbox.max,
                                   textvariable=getenv("gamma.var"),
                                   width=gamma.spinbox.width)

        ## Create input section for 'plotSurface' argument
        plotSurface.label <- tklabel(spatial.frame,
                                     text="Plot Surface?:")
        plotSurface.checkbox <- tkcheckbutton(spatial.frame,
                                              offvalue="FALSE",
                                              onvalue="TRUE",
                                              text="",
                                             variable=getenv("plotSurface.var"))

        ## Manage widgets
        tkgrid(cutoff.label,
               cutoff.spinbox)
        tkgrid(k.label,
               k.entry)
        tkgrid(gamma.label,
               gamma.spinbox)
        tkgrid(plotSurface.label,
               plotSurface.checkbox)

        tkgrid.configure(cutoff.label,
                         k.label,
                         gamma.label,
                         plotSurface.label,
                         sticky="e")
        tkgrid.configure(cutoff.spinbox,
                         k.entry,
                         gamma.spinbox,
                         plotSurface.checkbox,
                         sticky="w")
    }

    ## Manage widgets
    tkpack(spatial.frame,
           fill="x",
           padx="3m")
}


##-----------------------------------------------------------------------------
createQCParamsPanel <- function(parent) {
    ## Check arguments
    stopifnot(is.tkwin(parent))

    ## Begin processing
    formal.args <- pairlist(prefitqc=formals(SuperCurve::RPPASet)$doprefitqc)

    ## Prepare possible input values
    prefitqc.arg <- local({
        rsrcClass <- "PreFitQC"
        rsrcName <- tolower(rsrcClass)
        uservalue <- tclvalue(optiondb_get(rsrcName=rsrcName,
                                           rsrcClass=rsrcClass))
        value <- as.logical(uservalue)
cat(sprintf("%s: [%s] -> [%s]", rsrcName, uservalue, value))

        values <- c(TRUE, FALSE)
        x.default <- match(eval(formal.args$prefitqc), values)
        stopifnot(!is.na(x.default))
cat(sprintf("\tdefault: [%s]", values[x.default]))

        x.match <- match(value, values, nomatch=x.default)
        default <- values[x.match]
cat(sprintf("\tfinal: [%s]\n", default))
        list(default=as.character(default),
             values=as.character(values))
    })



    ##-------------------------------------------------------------------------
    ## Weird but it won't handle 'tclvalue(getenv("grouping.var")) <- value'
    ## This achieves the same via temporary variable.
    setVariable <- function(x, value) {
        stopifnot(is.tclVar(x))
        stopifnot(!missing(value))

        tclvalue(x) <- value
    }


    setVariable(getenv("prefitqc.var"), prefitqc.arg$default)

    ## PreFit QC
    prefitqc.frame <- tklabelframe(parent,
                                   text="PreFit Quality Control")
    {
        ## Create input section for 'prefitqc' argument
        prefitqc.label <- tklabel(prefitqc.frame,
                                  text="Perform QC?:")
        prefitqc.checkbox <- tkcheckbutton(prefitqc.frame,
                                           offvalue="FALSE",
                                           onvalue="TRUE",
                                           text="",
                                           variable=getenv("prefitqc.var"))

        ## Manage widgets
        tkgrid(prefitqc.label,
               prefitqc.checkbox)

        tkgrid.configure(prefitqc.label,
                         sticky="e")
        tkgrid.configure(prefitqc.checkbox,
                         sticky="w")
    }

    ## Manage widgets
    tkpack(prefitqc.frame,
           fill="x",
           padx="3m")
}


##-----------------------------------------------------------------------------
createSettingsFromUserInput <- function() {

    ##-------------------------------------------------------------------------
    getPathnameFromInput <- function(textvar) {
        ## Check arguments
        stopifnot(is.character(textvar) && length(textvar) == 1)

        ## Begin processing
        pathname <- tclvalue(getenv(textvar))

        ## Convert empty strings to NULL
        if (!nzchar(pathname)) {
            pathname <- NULL
        }

        pathname
    }


    ##-------------------------------------------------------------------------
    getModelFromLabel <- function() {
        model.arg.labels <- sapply(SuperCurve::getRegisteredModelKeys(),
                                   SuperCurve::getRegisteredModelLabel)
        model.label <- tclvalue(getenv("model.label.var"))
        model <- names(which(model.arg.labels == model.label))
    }


    ## Get file/directory user input
    txtdir <- getPathnameFromInput("txtdir.var")
    outdir <- getPathnameFromInput("outdir.var")
    imgdir <- getPathnameFromInput("imgdir.var")
    antibodyfile <- getPathnameFromInput("antibodyfile.var")

    ## Get software argument for RPPA() method
    software <- tclvalue(getenv("software.var"))

    ## Collect design parameters
    aliasfile <- getPathnameFromInput("aliasfile.var")
    designfile <- getPathnameFromInput("designfile.var")
    dp <- list(grouping=tclvalue(getenv("grouping.var")),
               ordering=tclvalue(getenv("ordering.var")),
               center=tclvalue(getenv("center.var")),
               aliasfile=aliasfile,
               designfile=designfile)

    ## Collect fit parameters
    fp <- list(measure=tclvalue(getenv("measure.var")),
               model=getModelFromLabel(),
               method=tclvalue(getenv("method.var")),
               trim=tclvalue(getenv("trim.var")),
               ci=tclvalue(getenv("ci.var")),
               ignoreNegative=tclvalue(getenv("ignoreNegative.var")))

    ## Collect spatial parameters
    sp <- list(cutoff=tclvalue(getenv("cutoff.var")),
               k=tclvalue(getenv("k.var")),
               gamma=tclvalue(getenv("gamma.var")),
               plotSurface=tclvalue(getenv("plotSurface.var")))

    ## Should Spatial Adjustment be performed?
    spatial <- tclvalue(getenv("spatial.var"))

    ## Should PreFit QC be performed?
    prefitqc <- tclvalue(getenv("prefitqc.var"))

    ## Postprocess
    dp$center <- as.logical(dp$center)

    fp$trim <- as.integer(fp$trim)
    fp$ci <- as.logical(fp$ci)
    fp$ignoreNegative <- as.logical(fp$ignoreNegative)
    fp$warnLevel <- as.integer(-1)

    sp$cutoff <- as.numeric(sp$cutoff)
    sp$k <- as.numeric(sp$k)
    sp$gamma <- as.numeric(sp$gamma)
    sp$plotSurface <- as.logical(sp$plotSurface)

message(sprintf("spatial:  %s (%s)", spatial, class(spatial)))
    spatial <- as.logical(spatial)
message(sprintf("spatial:  %s (%s)", spatial, class(spatial)))

message(sprintf("prefitqc:  %s (%s)", prefitqc, class(prefitqc)))
    prefitqc <- as.logical(prefitqc)
message(sprintf("prefitqc:  %s (%s)", prefitqc, class(prefitqc)))

    ## Error checking
    if (is.null(txtdir)) {
        stop("quantification directory not specified",
             call.=FALSE)
    } else if (is.null(outdir)) {
        stop("output directory not specified",
             call.=FALSE)
    } else if (txtdir == outdir) {
        stop("quantification and output directories must be different",
             call.=FALSE)
    }

    if (is.null(designfile)) {
        if (spatial || prefitqc) {
            stop("slide design file required for requested processing",
                 call.=FALSE)
        } else {
            message("slide design file suggested for any processing",
                    call.=FALSE)
        }
    }

    ## Create appropriate classes
    designparams <- do.call(SuperCurve::RPPADesignParams, dp)
    fitparams <- do.call(SuperCurve::RPPAFitParams, fp)
    spatialparams <- if (spatial) {
                         do.call(SuperCurve::RPPASpatialParams, sp)
                     } else {
                         NULL
                     }

    SuperCurve::SuperCurveSettings(txtdir,
                                   imgdir,
                                   outdir,
                                   designparams,
                                   fitparams,
                                   spatialparams=spatialparams,
                                   doprefitqc=prefitqc,
                                   antibodyfile=antibodyfile,
                                   software=software)
}


##-----------------------------------------------------------------------------
saveSettings <- function(settings,
                         rda.pathname,
                         txt.pathname) {
    ## Check arguments
    stopifnot(SuperCurve:::is.SuperCurveSettings(settings))
    stopifnot(is.character(rda.pathname) && length(rda.pathname) == 1)
    stopifnot(is.character(txt.pathname) && length(txt.pathname) == 1)

    ##---------------------------------------------------------------------
    makeFileHeader <- function(string) {
        stopifnot(is.character(string) && length(string) == 1)

        paste("###",
              paste("###", string),
              "###",
              "\n",
              sep="\n")
    }


    ## Save settings (XDR format)
    save(settings, file=rda.pathname, ascii=TRUE)

    ## Save settings (human-readable text format)
    version <- packageDescription("SuperCurve", fields="Version")
    cat(makeFileHeader("SuperCurve settings"),
        paramString(settings),
        paste("supercurve version:", version), "\n",
        "\n",  # blank line at EOF
        sep="",
        file=txt.pathname)
}


##-----------------------------------------------------------------------------
## Saves names of stages to be displayed by progress dialog in environment.
setStages <- function(settings) {
    stages <- SuperCurve::getStages()

    ## If spatial adjustment not enabled, remove it
    if (is.null(settings@spatialparams)) {
        stages <- stages[-match("spatial", names(stages))]
    }

    ## If pre-fit QC not enabled, remove it
    if (!settings@doprefitqc) {
        stages <- stages[-match("prefitqc", names(stages))]
    }

    final <- "Summary"
    names(final) <- "summary"

    setenv("stages", c(stages, final))
}


##-----------------------------------------------------------------------------
## Redisplays radiobutton widgets to represent a completed stage.
displayAsCompleted <- function(radiobutton) {
    stopifnot(is.tkwin(radiobutton))
    .appEntryStr(sprintf("displayAsCompleted(%s): %s",
                         .Tk.ID(radiobutton),
                         tclvalue(tkcget(radiobutton, '-value'))))
    stopifnot(tclvalue(tkwinfo.class(radiobutton)) == "Radiobutton")

    tkconfigure(radiobutton,
                selectcolor="",
                state="disabled")
    tclupdate("idletasks")
}


##-----------------------------------------------------------------------------
## Redisplays radiobutton widget to represent the stage in work.
displayAsInProgress <- function(radiobutton) {
    stopifnot(is.tkwin(radiobutton))
    .appEntryStr(sprintf("displayAsInProgress(%s): %s",
                         .Tk.ID(radiobutton),
                         tclvalue(tkcget(radiobutton, '-value'))))
    stopifnot(tclvalue(tkwinfo.class(radiobutton)) == "Radiobutton")

    stageFont <- "stagead"    ## Must be same as in supercurveGUI()
    foreground <- tclvalue(tkcget(radiobutton, "-foreground"))
    tkconfigure(radiobutton,
                command=function() {
                    displayAsCompleted(radiobutton)
                },
                disabledforeground=foreground,
                font=stageFont,
                selectcolor="blue")
    tclupdate("idletasks")
}


##-----------------------------------------------------------------------------
createProgressDialog <- function(parent,
                                 stages,
                                 pbcol="green") {
    ## Check arguments
    stopifnot(is.tkwin(parent))
    stopifnot(is.character(stages) && all(nzchar(stages)))

    ## Begin processing

    ## Create toplevel shell and frame as its child
    dialog <- tktoplevel(parent=parent)
    tkwm.title(dialog, "ProgressDialog")
    tkwm.group(dialog, parent)

    tkpack(progress.frame <- tkframe(dialog,
                                     class="ProgressBarDialog"))

    ## Create area frames and separator
    tkpack(command.area <- commandArea(progress.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(progress.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(progress.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    stages.frame <- tkframe(command.area,
                            class="RadioBox")
    message(sprintf("stages: [%s] names: [%s]", stages, names(stages)))
    sapply(stages,
           function(stage, radiobox) {
               stopifnot(nzchar(stage))
               stopifnot(is.tkwin(radiobox))

               message(sprintf("stage: [%s] name: [%s]", stage, names(stage)))
               ## :TODO: Convert 'value' to use key (aka, names(stage)) instead
               radiobutton <- tkradiobutton(radiobox,
                                            anchor="w",
                                            state="disabled",
                                            text=stage,
                                            value=stage)
               cmd <- substitute(function() displayAsInProgress(widget),
                                 list(widget=radiobutton))
               tkconfigure(radiobutton,
                           command=eval(cmd))
               tkpack(radiobutton,
                      side="top",
                      fill="x")
           },
           radiobox=stages.frame)

    ## Create frame for progress bar and marquee/detail labels
    details.frame <- tkframe(command.area,
                             background="white",
                             relief="sunken")

    ## :TBD: Are these label width settings the problem with dialog width?
    marquee.label <- tklabel(details.frame,
                             background="white",
                             font="banner",
                             width="100")
    detail.label <- tklabel(details.frame,
                            background="white",
                            text="-",
                            width="100")
    progressbar <- progressbar_create(details.frame,
                                      pbcol)

    tkpack(marquee.label,
           progressbar,
           detail.label,
           expand=TRUE,
           fill="both",
           padx=10,
           pady=10)

    tkpack(stages.frame,
           side="left",
           padx=10,
           pady=10)
    tkpack(details.frame,
           side="right",
           padx=10,
           pady=10)

    ## Create action area
    close.button <- tkbutton(action.area,
                             command=function() {
                                 closeDialog(dialog)
                             },
                             text="Close")
    tkpack(close.button)

    ## Save userdata
    userdata <- list(RadioBox=stages.frame,
                     Marquee=marquee.label,
                     Detail=detail.label,
                     ProgressBar=progressbar,
                     CloseButton=close.button)
    assign("userdata", userdata, env=dialog$env)

    return(dialog)
}


##-----------------------------------------------------------------------------
closeDialog <- function(dialog) {
    message("closeDialog() entry")

    ## Check arguments
    stopifnot(is.tkwin(dialog))

    ## Begin processing
    tkwm.withdraw(dialog)
    tclupdate()
    tkdestroy(dialog)
}


##-----------------------------------------------------------------------------
monitorAnalysis <- function(dialog,
                            monitor,
                            settings) {
    message("monitorAnalysis() entry")

    ## Check arguments
    stopifnot(is.tkwin(dialog))
    stopifnot(is.SCProgressMonitor(monitor))

    ##-------------------------------------------------------------------------
    ## Returns string summarizing fit processing
    fitsummary <- function(summaryfile) {
        completed.df <- read.delim(summaryfile)
        fitted <- completed.df[, "fit"]
        nslides <- length(fitted)
        nfitted <- length(fitted[fitted])
        if (nfitted == 0) {
            "Processing completed but no slides were fitted."
        } else if (nfitted == nslides) {
            ## Approximate ngettext() with three input values
            switch(EXPR=as.character(nslides),
                   "1" = "Fitted single slide.",
                   "2" = "Fitted both slides.",
                   sprintf("Fitted all %d slides.", nslides))
        } else {
            sprintf("Fitted %d of %d slides. See summary file for details.",
                    nfitted,
                    nslides)
        }
    }


    ## Begin processing
    tclupdate()    # Do update before processing starts

    ## Log session output
    outputdir <- as(settings@outdir, "character")
    fnamebase <- strftime(Sys.time(), "session-%Y%m%dT%H%M")   # ISO-8601
    log.pathname <- file.path(outputdir,
                              paste(fnamebase, "log", sep="."))
    sessionlog <- file(log.pathname, open="w")
    sink(sessionlog, type="output")
    sink(sessionlog, type="message")
    on.exit({
        ## End diversions (LIFO)
        sink(type="message")
        sink(type="output")
    })
    on.exit(close(sessionlog), add=TRUE)

    ## Start...
    show(settings)
    tryCatch({
            ## Disable analyze button (toplevel) - one analysis at a time
            tkconfigure(getenv("analyze.button"),
                        state="disabled")

            ## Display busy cursor on toplevel
            tkconfigure(getenv("toplevel"),
                        cursor="watch")

            ## Load prerequisite packages before starting analysis
            progressMarquee(monitor) <- "Loading packages needed for analysis"
            tclupdate()
            packages <- SuperCurve:::getPrerequisitePackages(settings)
            sapply(packages,
                   function(pkgname) {
                       progressLabel(monitor) <- pkgname
                       tclupdate()
                       ## Nonstandard evaluation done by invoked method
                       do.call("library", list(package=pkgname))
                   })

            ## Perform analysis
            SuperCurve:::fitCurveAndSummarizeFromSettings(settings, monitor)

            ## Update interface to show processing complete
            progressStage(monitor) <- "Summary"
            progressDone(monitor) <- TRUE
            tryCatch({
                    ## Update label with summary of fits, if possible
                    summaryfilename <- "supercurve_summary.tsv"
                    summarypathname <- file.path(outputdir, summaryfilename)
                    if (file.exists(summarypathname)) {
                        progressLabel(monitor) <- fitsummary(summarypathname)
                    }
                })
        },
        interrupt=function(cond) {
            ## Despite my efforts, I have never triggered this...
            message("\tin interrupt clause of tryCatch()")
            condmsg <- conditionMessage(cond)
            message(sprintf("<<<INTR>>>  %s", condmsg))
            progressError(monitor) <- TRUE

            showwarning(title="Processing Interrupted", message=condmsg)
        },
        error=function(cond) {
            message("\tin error clause of tryCatch()")

            message("###stacktrace###")
            dump.frames()
            invisible(sapply(names(last.dump),
                             function(acall) {
                                 message(paste("   ", acall))
                             },
                             USE.NAMES=FALSE))
            condmsg <- conditionMessage(cond)
            message(sprintf("<<<ERROR>>>  %s", condmsg))

            progressError(monitor) <- TRUE

            showerror(title="Processing Error", message=condmsg)
            setenv("errmsg", condmsg)
        },
        finally={
            message("\tin finally clause of tryCatch()")
            et <- elapsed(monitor@etime)
            message(sprintf("elapsed time: %.3f %s", et, units(et)))
cat("monitor@stage.var:", tclvalue(monitor@stage.var), "\n")
cat("monitor@marquee.var:", tclvalue(monitor@marquee.var), "\n")
cat("monitor@label.var:", tclvalue(monitor@label.var), "\n")

            ## Restore standard cursor on toplevel
            tkconfigure(getenv("toplevel"),
                        cursor="")

            ## (Re)enable analyze button (toplevel)
            tkconfigure(getenv("analyze.button"),
                        state="normal")

            ## Restore dialog button back to its original purpose
            message("\t\treconfiguring button from cancel to close")
            closeButton <- .getCloseButtonFromDialog(dialog)
            tkconfigure(closeButton,
                        command=function() {
                            closeDialog(dialog)
                        },
                        relief="raised",
                        text="Close")
        })
}


##-----------------------------------------------------------------------------
displayProgressDialog <- function(dialog,
                                  monitor,
                                  settings) {
    stopifnot(is.tkwin(dialog))
    stopifnot(is.SCProgressMonitor(monitor))

    ##-------------------------------------------------------------------------
    ## Update window manager's geometry to shrink the progress dialog.
    wmGeometry <- function() {
        .appEntryStr("wmGeometry")

        tclupdate("idletasks")
        toplevel <- getenv("toplevel")

        screenwidth <- as.integer(tclvalue(tkwinfo.screenwidth(toplevel)))
        screenheight <- as.integer(tclvalue(tkwinfo.screenheight(toplevel)))
        cat("screen width:", screenwidth, "\n")
        cat("screen height:", screenheight, "\n")

        toplevel.reqwidth <- tclvalue(tkwinfo.reqwidth(toplevel))
        toplevel.reqheight <- tclvalue(tkwinfo.reqheight(toplevel))
        cat("toplevel reqwidth:", toplevel.reqwidth, "\n")
        cat("toplevel reqheight:", toplevel.reqheight, "\n")

        dialog.reqwidth <- tclvalue(tkwinfo.reqwidth(dialog))
        dialog.reqheight <- tclvalue(tkwinfo.reqheight(dialog))
        cat("dialog reqwidth:", dialog.reqwidth, "\n")
        cat("dialog reqheight:", dialog.reqheight, "\n")

        dialog.width <- as.integer(toplevel.reqwidth) * as.integer(2)
        dialog.height <- as.integer(dialog.reqheight)

        ## Update geometry
        geometry <- sprintf("%dx%d",
                            dialog.width,
                            dialog.height)
        cat("new wm geometry:", geometry, "\n")
        flush.console()
        tkwm.geometry(dialog, geometry)

        ##---------------------------------------------------------------------
        ## Update window manager's geometry to enlarge dialog width one pixel.
        wmGeometryHack <- function() {
            .appEntryStr("wmGeometryHack")

            geometry <- tclvalue(tkwm.geometry(dialog))
            nonumbers.re <- "[^[:digit:]]"
            geometry.vals <- unlist(strsplit(geometry, nonumbers.re))

            dialog.width <- as.integer(geometry.vals[1]) + 1
            dialog.height <- as.integer(geometry.vals[2])

            ## Update geometry
            geometry <- sprintf("%dx%d",
                                dialog.width,
                                dialog.height)
            cat("new wm geometry:", geometry, "(YA)", "\n")
            flush.console()
            tkwm.geometry(dialog, geometry)
        }


        ## :HACK: For some reason, the 'detail' label on the progress dialog
        ## doesn't seem to get updated for quite some time, unless the dialog
        ## gets resized. Since we REALLY want to be able to see details during
        ## processing, set a timer to resize the dialog by one pixel.
        tclafter(500, wmGeometryHack)
    }


    ##-------------------------------------------------------------------------
    ## Update window manager's minimum width of dialog to half that of toplevel.
    wmMinSizeResetWidth <- function() {
        .appEntryStr("wmMinSizeResetWidth")

        tclupdate("idletasks")
        toplevel <- getenv("toplevel")

        minwidth <- as.integer(tclvalue(tkwinfo.reqwidth(toplevel)))
        minheight <- as.integer(tclvalue(tkwinfo.height(dialog)))
        cat("new wm minsize:", paste(minwidth, "x", minheight, sep=""), "\n")
        flush.console()
        tkwm.minsize(dialog, minwidth, minheight)
    }


    ## Handle WM close button
    tkwm.protocol(dialog,
                  "WM_DELETE_WINDOW",
                  function() {
                      message("[WM close: progress dialog]")
                      ## Invoke button as its callback is dynamic
                      close.button <- .getCloseButtonFromDialog(dialog)
                      tkinvoke(close.button)
                  })

    ## Bind to event tag used to begin analysis
    startAnalysis.bindtag <- "StartAnalysis"
    tkbind(startAnalysis.bindtag,
           "<Map>",
           function() {
               ## Initiate analysis
               tclafter.idle(function() {
                   ## Analyze the data...
                   monitorAnalysis(dialog, monitor, settings)
               })
           })

    ## Grab progress bar from dialog
    progressbar <- .getProgressBarFromDialog(dialog)
    stopifnot(is.tkwin(progressbar))

    ## Prepend new event tag to progress bar event-handler bindtags
    orig.bindtags <- getBindtags(progressbar)
    initprog.bindtags <- c(startAnalysis.bindtag, orig.bindtags)
    tkbindtags(progressbar, paste(initprog.bindtags, collapse=' '))

    ## Bind to single dialog item (bind to dialog itself applies to all widgets)
    tkbind(progressbar,
           "<Map>",
           function() {
               message("[dialog mapped]")

               ## Once analysis initiated, remove tag (so it only occurs once)
               bindtags <- getBindtags(progressbar)
               if (startAnalysis.bindtag %in% bindtags) {
                   tkbindtags(progressbar, paste(orig.bindtags, collapse=' '))
               }
           })

    ## Use marquee as progress bar is often unmanaged before dialog dismissal
    marquee.label <- .getMarqueeLabelFromDialog(dialog)
    stopifnot(is.tkwin(marquee.label))
    tkbind(marquee.label,
           "<Unmap>",
           function() {
               message("[dialog unmapped]")
           })

    ## Resize dialog to something more appropriate
    ## :PLR: Why is it so big to begin with?
    tclafter.idle(wmGeometry)

    ## Set minimum size for dialog
    tclafter.idle(wmMinSizeResetWidth)

    ## Designate dialog as the focus window for input
    tkfocus(dialog)

    invisible(NULL)
}


##-----------------------------------------------------------------------------
.getCloseButtonFromDialog <- function(dialog) {
    userdata <- get("userdata", envir=dialog$env)
    stopifnot(is.list(userdata))
    close.button <- userdata$CloseButton
}


##-----------------------------------------------------------------------------
.getDetailLabelFromDialog <- function(dialog) {
    userdata <- get("userdata", envir=dialog$env)
    stopifnot(is.list(userdata))
    detail.label <- userdata$Detail
}


##-----------------------------------------------------------------------------
.getMarqueeLabelFromDialog <- function(dialog) {
    userdata <- get("userdata", envir=dialog$env)
    stopifnot(is.list(userdata))
    marquee.label <- userdata$Marquee
}


##-----------------------------------------------------------------------------
.getProgressBarFromDialog <- function(dialog) {
    userdata <- get("userdata", envir=dialog$env)
    stopifnot(is.list(userdata))
    progressbar <- userdata$ProgressBar
}


##-----------------------------------------------------------------------------
.getRadioBoxFromDialog <- function(dialog) {
    userdata <- get("userdata", envir=dialog$env)
    stopifnot(is.list(userdata))
    radiobox <- userdata$RadioBox
}


##-----------------------------------------------------------------------------
.getRadioButtons <- function(radiobox) {
    stopifnot(is.tkwin(radiobox))
    unlist(strsplit(tclvalue(tkwinfo.children(radiobox)), ' '))
}


##-----------------------------------------------------------------------------
performAnalysis <- function(settings) {
    ## Check arguments
    stopifnot(SuperCurve:::is.SuperCurveSettings(settings))

    ## Confirm user selections
    question <- paste("Run analysis with the following options:",
                      "\n\n",
                      SuperCurve:::paramString(settings),
                      sep="")
    if (!askyesno(default="yes",
                  message=question,
                  title="Confirm")) {
        cat("**user canceled analysis**", "\n")
        return(NULL)
    }

    ## Check for existing analysis
    outputdir <- as(settings@outdir, "character")
    fnamebase <- "sc-settings"
    rda.pathname <- file.path(outputdir,
                              paste(fnamebase, "RData", sep="."))
    txt.pathname <- file.path(outputdir,
                              paste(fnamebase, "txt", sep="."))
    if (file.exists(rda.pathname) ||
        file.exists(txt.pathname)) {
        ## Ask user whether to proceed
        question <- "Overwrite existing settings and analysis?"
        if (!askyesno(default="no",
                      message=question,
                      title="Overwrite?")) {
            cat("**existing settings not overwritten**", "\n")
            return(NULL)
        }
    }

    ## Save settings
    saveSettings(settings,
                 rda.pathname,
                 txt.pathname)
    tclupdate()

    ## Configure progress dialog
    setStages(settings)
    progressDialog <- createProgressDialog(getenv("toplevel"),
                                           getenv("stages"))

    ## Create SuperCurve progress monitor
    monitor <- TkProgressMonitor(progressDialog)

    ##-------------------------------------------------------------------------
    cancelCB <- function(cancelButton) {
        stopifnot(is.tkwin(cancelButton))
        stopifnot(tclvalue(tkwinfo.class(cancelButton)) == "Button")

        ## Display with "pressed in" visual
        tkconfigure(cancelButton,
                    relief="sunken")

        ## Confirm user selections
        question <- paste("Are you sure you want to terminate processing?",
                          "All work thus far will be lost...",
                          sep="\n\n")
        if (!askyesno(default="no",
                      message=question,
                      title="Confirm")) {
            cat("**user canceled abort**", "\n")

            ## Restore normal visual
            tkconfigure(cancelButton,
                        relief="raised")
            return(NULL)
        }

        ## Launch RPG
        progressAbort(monitor) <- TRUE
        stopifnot(!progressContinue(monitor))

        ##
        ## No way to verify abort from here... the monitor object really
        ## should be based on an environment; just looking at original
        ## object here, SuperCurve has its own copy.
        ## Hope it works...
        ##
    }


    userdata <- get("userdata", envir=progressDialog$env)
    stopifnot(is.list(userdata))
    {
        radiobox <- userdata$RadioBox
        marquee.label <- userdata$Marquee
        detail.label <- userdata$Detail
        close.button <- userdata$CloseButton

        ## Glue progress monitor values to dialog widgets
        sapply(.getRadioButtons(radiobox),
               function(buttonID, variable) {
                   radiobutton <- .Tk.newwin(buttonID)
                   tkconfigure(radiobutton,
                               variable=variable)
               },
               variable=monitor@stage.var)
        tkconfigure(marquee.label,
                    textvariable=monitor@marquee.var)
        tkconfigure(detail.label,
                    textvariable=monitor@label.var)
        ## Solitary button performs double duty...
        tkconfigure(close.button,
                    command=function() {
                        cancelCB(close.button)
                    },
                    text="Cancel")
    }

    ## Bind to event tag used to deiconify and raise dialog
    modalDialog.bindtag <- "ModalDialog"
    tkbind(modalDialog.bindtag,
           "<ButtonPress>",
           function() {
               tkwm.deiconify(progressDialog)
               tkraise(progressDialog)
           })

    ## Prepend new event tag to dialog event-handler bindtags
    modal.bindtags <- c(modalDialog.bindtag, getBindtags(progressDialog))
    tkbindtags(progressDialog, paste(modal.bindtags, collapse=' '))

    ## Display progress dialog
    displayProgressDialog(progressDialog, monitor, settings)
}


##-----------------------------------------------------------------------------
## Builds menu system tied to menubar usage.
buildMenus <- function(parent) {
    ## Check arguments
    stopifnot(is.tkwin(parent))
    stopifnot(tclvalue(tkwinfo.class(parent)) == "Menu")

    ## Create menu widgets
    file.menu <- tkmenu(parent,
                        tearoff=FALSE)
    help.menu <- tkhelpmenu(parent,
                            tearoff=FALSE)

    ## Create cascade menus
    tkadd(parent,
          "cascade",
          label="File",
          menu=file.menu,
          underline=0)
    tkadd(parent,
          "cascade",
          label="Help",
          menu=help.menu,
          underline=0)

    ##-------------------------------------------------------------------------
    ## Add FILE menu items.
    buildFileMenu <- function(file.menu) {
        ## Check arguments
        stopifnot(tclvalue(tkwinfo.class(file.menu)) == "Menu")

        ##---------------------------------------------------------------------
        ## Load existing settings.
        loadSettingsCB <- function() {

            txtdir <- getenv("initialdir")
            withCallingHandlers({
                    repeat {
                        settingsFile <- .chooseSettingsFile(txtdir)
                        if (!is.null(settingsFile)) {
                            txtdir <- dirname(settingsFile)
                            settings <- loadSettingsWithRestarts(settingsFile)
                            if (!is.null(settings)) {
                                setenv("settings", settings)
                                reloadInterface(settings)
                                break
                            }
                        } else {
                            cat("**user canceled settings file selection**", "\n")
                            break
                        }
                    }
                },
                error=function(cond) {
                    msg <- sprintf(paste("Failed to load SuperCurve settings",
                                         "File: %s",
                                         sep="\n"),
                                   settingsFile)
                    displayErrorAndAllowRetry(msg, cond)
                })
            try(show(str(settings)))
        }


        ## Add menu items
        tkadd(file.menu,
              "command",
              label="Load Settings...",
              command=loadSettingsCB)
        tkadd(file.menu,
              "command",
              label="Quit",
              command=appExit)
    }


    ##---------------------------------------------------------------------
    ## Add HELP menu items.
    buildHelpMenu <- function(help.menu) {
        ## Check arguments
        stopifnot(tclvalue(tkwinfo.class(help.menu)) == "Menu")

        ##-----------------------------------------------------------------
        ## Display overview dialog.
        overviewCB <- function() {
            overview <- paste("Tcl/Tk application for analyzing",
                              "RPPA data.")
            showinfo(message=overview,
                     parent=getenv("toplevel"),
                     title="Overview")
        }


        ##---------------------------------------------------------------------
        ## Display user guide in web browser window.
        userguideCB <- function() {
            userguide.url <- "http://bioinformatics.mdanderson.org/Software/supercurve/UserGuide.html"

            ## Ask web browser to display the URL
            browseURL(URLencode(userguide.url))
        }


        ##-----------------------------------------------------------------
        ## Display about dialog.
        aboutCB <- function() {

            ##-------------------------------------------------------------
            ## Returns application version string.
            getAppVersionLabelstring <- function(default="NA") {
                ## Check arguments
                stopifnot(is.character(default) && length(default) == 1)

                ## Begin processing
                pkgname <- getPackageName()
                if (pkgname == ".GlobalEnv") {
                    paste("Version:", default, "(unpackaged)")
                } else {
                    packageDescription(pkgname, fields = "Version")
                }
            }


            ##-------------------------------------------------------------
            ## Returns Tcl/Tk version string.
            getTclTkVersionLabelstring <- function() {
                tcltk.version <- tclvalue(tclinfo("patchlevel"))
                paste("Tcl/Tk:", tcltk.version)
            }


            ##-----------------------------------------------------------------
            ## Returns Tk windowing system string.
            getTkWindowingSystemLabelstring <- function() {
                windowing.system <- tclvalue(tktk.windowingsystem())
                paste("Windowing System:", windowing.system)
            }


            about <- paste(app.name <- "SuperCurveGUI",
                           getAppVersionLabelstring(),
                           getTclTkVersionLabelstring(),
                           getTkWindowingSystemLabelstring(),
                           sep="\n")
            showinfo(message=about,
                     parent=getenv("toplevel"),
                     title="About")
        }


        ## Add menu items
        tkadd(help.menu,
              "command",
              label="Overview",
              command=overviewCB)
        tkadd(help.menu,
              "command",
              label="User Guide",
              command=userguideCB)
        tkadd(help.menu,
              "command",
              label="About SuperCurveGUI",
              command=aboutCB)
    }

    ## Add menu items to menus
    buildFileMenu(file.menu)
    buildHelpMenu(help.menu)
}


##-----------------------------------------------------------------------------
## Terminates the application. May be canceled by user if unsaved
## modifications would be lost.
appExit <- function() {

    ##-------------------------------------------------------------------------
    ## Terminate the application.
    terminate <- function() {
        .appEntryStr("terminate")

        tclupdate("idletasks")

        ## Unmap toplevel
        tkwm.withdraw(toplevel <- getenv("toplevel"))

        ## Arrange for server resource cleanup
        for (font in getenv("fonts")) {
            on.exit(tkfont.delete(font))
        }

        for (image in getenv("images")) {
            on.exit(tkimage.delete(image))
        }

        ## Destroy toplevel
        tkdestroy(toplevel)
    }


    ## Destroy toplevel indirectly to workaround problem with X11
    tclafter.idle(terminate)
}


##-----------------------------------------------------------------------------
.getDefaultDirectory <- function() {
    scdir <- Sys.getenv("SC_DIR")
    if (nzchar(scdir)) {
        scdir
    } else {
        NULL
    }
}


##-----------------------------------------------------------------------------
## This is the only user-visible function in the file. You call this
## function to start the application.

## Prompt user for parameters and run SuperCurve using specified directories

## :TODO: Most of the hard-coded choices should be converted to a data-driven
## model that allows us to read them from a table or flat file somewhere. The
## idea should be to allow new classes to "register" themselves somewhere and
## thus get added to the GUI automagically without having to modify the code
## here. This method was used by the 'affy' class in Bioconductor to allow for
## the easy plug-in of new methods. Specifically, we previously worked out a
## model that had three basic processing steps:
##  [1] SpotLevel Corrections (which might just be local background correction)
##      but might now involve Shannon's nested surface fits to the diluted
##      positive controls.
##  [2] Curve fitting, which now has at least three possible models. At
##      present, truncation (trimming) is included a part of this step, but
##      we might want to separate it to allow for alternative trimming
##      algorithms.
##  [3] Normalization, which is not presently included in the GUI, but should
##      be.
supercurveGUI <- function() {
    ## Set WM_CLIENT_MACHINE property on root window
    tkwm.client('.', tclinfo.hostname())

    ## Create named fonts for later use
    availableFonts <- unlist(strsplit(tclvalue(tkfont.names()), " "))
    bannerFont <- "banner"
    if (!(bannerFont %in% availableFonts)) {
        #cat(sprintf("creating %s font", sQuote(bannerFont)), "\n")
        tkfont.create(bannerFont,
                      family="helvetica",
                      size=16,
                      weight="bold")
    }

    stageFont <- "stagead"
    if (!(stageFont %in% availableFonts)) {
        #cat(sprintf("creating %s font", sQuote(stageFont)), "\n")
        tkfont.create(stageFont,
                      family="helvetica",
                      size=12,
                      weight="bold")
    }

    prestageFont <- "stagebc"
    if (!(prestageFont %in% availableFonts)) {
        #cat(sprintf("creating %s font", sQuote(prestageFont)), "\n")
        tkfont.create(prestageFont,
                      family="helvetica",
                      size=12,
                      weight="normal")
    }

    ## Create images for later use
    existingImages <- unlist(strsplit(tclvalue(tkimage.names()), " "))

    ## When Tcl creates an image, it uses the name to create a new command.
    ## Ensure that image name doesn't accidentally overwrite existing ones.
    required <- "requiredImage"
    if (!(required %in% existingImages)) {
        #cat(sprintf("creating %s image", sQuote(requiredImage)), "\n")
        tkimage.create("photo",
                       required,
                       file=system.file("images", "required.gif",
                                        package="SuperCurveGUI"))
    }

    ## Add entries to Tk option database
    local({
        ## Add "fake" resource values into options database
        software  <- formals(RPPASet)$software
        prefitqc  <- as.character(formals(RPPASet)$doprefitqc)
        fitmethod <- eval(formals(RPPAFitParams)$method)[1]
        fitmodel  <- formals(RPPAFitParams)$model

        initOptions(list("*software"=software,
                         "*prefitqc"=prefitqc,
                         "*fitmethod"=fitmethod,
                         "*fitmodel"=fitmodel))

        ## Add widget resource values into options database
        initOptions(list("*BannerFrame.Label.font"=bannerFont,
                         "*BannerFrame.Label.justify"="left",
                         "*EmptySpace.width"="20",
                         "*Entry.background"="white",
                         "*Entry.foreground"="black",
                         "*Entry.selectBackground"="yellow",
                         "*Entry.selectForeground"="black",
                         "*Dialog.msg.font"="courier",
                         "*Dialog.msg.wrapLength"="9i",
                         "*RadioBox.Radiobutton.font"=prestageFont,
                         "*Progressbar.borderWidth"="2",
                         "*Progressbar.relief"="sunken",
                         "*Progressbar.length"="200"))

        ## Handle app-defaults file(s), if any exist
        tkloadappdefaults(appdefaultsfile <- "supercurveGUI")
    })

    ## Create toplevel shell
    toplevel <- tktoplevel()
    tkwm.title(toplevel, "SuperCurve")

    ## Initialize "global" variables
    initGlobals(list(
                     aliasfile.var=tclVar(""),
                     analyze.button=NULL,
                     antibodyfile.var=tclVar(""),
                     center.var=tclVar(""),
                     ci.var=tclVar(""),
                     cutoff.var=tclVar(""),
                     designfile.var=tclVar(""),
                     dirty=FALSE,
                     errmsg=NULL,
                     fonts=c(bannerFont, stageFont, prestageFont),
                     gamma.var=tclVar(""),
                     grouping.var=tclVar(""),
                     ignoreNegative.var=tclVar(""),
                     images=c(required),
                     imgdir.var=tclVar(""),
                     initialdir=.getDefaultDirectory(),
                     k.var=tclVar(""),
                     measure.optmenu=NULL,
                     measure.var=tclVar(""),
                     method.var=tclVar(""),
                     model.label.var=tclVar(""),
                     model.var=tclVar(""),
                     ordering.var=tclVar(""),
                     outdir.var=tclVar(""),
                     plotSurface.var=tclVar(""),
                     prefitqc.var=tclVar(""),
                     settings=NULL,
                     spatial.var=tclVar(""),
                     software.var=tclVar(""),
                     spatial.checkbox=NULL,
                     toplevel=toplevel,
                     trim.var=tclVar(""),
                     txtdir.var=tclVar("")
                ))

    ## Create area frames and separator
    tkpack(command.area <- commandArea(toplevel),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(toplevel),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(toplevel),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(tabnotebook <- tabnotebook_create(command.area),
           expand=TRUE,
           fill="both")
    pathnamespage <- tabnotebook_page(tabnotebook, "Pathnames")
    createPathnamesPanel(pathnamespage)
    designparamspage <- tabnotebook_page(tabnotebook, "DesignParams")
    createDesignParamsPanel(designparamspage)
    fitparamspage <- tabnotebook_page(tabnotebook, "FitParams")
    createFitParamsPanel(fitparamspage)
    spatialadjpage <- tabnotebook_page(tabnotebook, "SpatialAdj")
    createSpatialAdjPanel(spatialadjpage)
    qcparamspage <- tabnotebook_page(tabnotebook, "QC")
    createQCParamsPanel(qcparamspage)
    tclupdate()

    ## :BUG: all background tasks are not being processed by tcltk package.
    ## Contact Peter Dalgaard about this problem!
    ## :WORKAROUND: Perform manual refresh if background task still exists
    ## to ensure all tabs are displayed on GUI.
    {
        userdata <- get("userdata", envir=tabnotebook$env)
        if (nzchar(userdata$Pending)) {
            message(sprintf("\t@@performing manual refresh (workaround)"))
            tclish:::.tabnotebook_refresh(tabnotebook)
        }
    }

    ## :KRC: Post-processing steps (truncation, normalization) should be added.
    ## Again, this needs to be data-driven.

    ##-------------------------------------------------------------------------
    ##
    analyzeCB <- function() {

        ##---------------------------------------------------------------------
        ## Give R some time to process its event loop
        idleTask <- function() {
            tclupdate()
            if (reschedule) {
                tclafter(2000, idleTask)
            }
        }

        reschedule <- TRUE
        tclafter.idle(idleTask)

        Try({
            ## Create settings class from inputs
            settings <- createSettingsFromUserInput()

    #Sys.setenv("SC_DIR"=as(settings@txtdir, "character"))

            ## Perform analysis
            performAnalysis(settings)
        })

        reschedule <- FALSE
    }

    ## Create action area
    analyze.button <- tkbutton(action.area,
                               command=analyzeCB,
                               text="Analyze")
    setenv("analyze.button", analyze.button)

    tkpack(analyze.button,
           pady="3m")

    tclafter.idle(tkbell())

    cat("windowing system:", tclvalue(tktk.windowingsystem()), "\n")

    ## Create menus
    menubar <- tkmenu(toplevel)
    buildMenus(menubar)
    tkconfigure(toplevel,
                menu=menubar)

    ## Handle WM close button
    tkwm.protocol(toplevel,
                  "WM_DELETE_WINDOW",
                  function() {
                      message("[WM close: toplevel]")
                      appExit()
                  })
}

scui <- supercurveGUI

