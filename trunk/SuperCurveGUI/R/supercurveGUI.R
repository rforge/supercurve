###
### SUPERCURVEGUI.R
###


##-----------------------------------------------------------------------------
## Adds entry into Tcl options database.
optiondb_add <- function(pattern,
                         value) {
    stopifnot(is.character(pattern) && length(pattern) == 1)
    stopifnot(!missing(value))

    #cat("option", "add", pattern, value, "startupFile", "\n")
    tcl("option", "add", pattern, value, "startupFile")
}


##-----------------------------------------------------------------------------
## Initialize the Tk option database with application defaults.
initOptions <- function(olist) {
    stopifnot(is.list(olist))

    sapply(seq_along(olist),
           function(i, ll) {
               rsrc <- names(ll)[i]
               value <- ll[[i]]
               optiondb_add(rsrc, value)
           },
           olist)
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

    filename <- tclvalue(tkgetOpenFile(title=title,
                                       filetypes=filetypes,
                                       initialdir=initialdir))
    if (!nzchar(filename)) {
        NULL
    } else {
        dirname(filename)
    }
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

    directory <- tclvalue(tkchooseDirectory(title=title,
                                            initialdir=initialdir))
    if (!nzchar(directory)) {
        NULL
    } else {
        directory
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
chooseQuantificationDirectoryWithRestarts <- function(initialdir) {
    withRestarts({
            dirname <- .chooseQuantificationDirectory(initialdir)

            txt.re <- ".*[tT][xX][tT]$"
            if (length(list.files(dirname, pattern=txt.re)) == 0) {
                stop(sprintf("directory %s contains no text files",
                             sQuote(dirname)))
            }

            dirname
        },
        retry=function() NULL)
}


##-----------------------------------------------------------------------------
chooseImageDirectoryWithRestarts <- function(initialdir) {
    withRestarts({
            dirname <- .chooseImageDirectory(initialdir)

            tif.re <- ".*[tT][iI][fF]{1,2}$"
            if (length(list.files(dirname, pattern=tif.re)) == 0) {
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

            if (!(file.access(dirname, mode=2) == 0)) {
                stop(sprintf("directory %s is not writable",
                             sQuote(dirname)))
            }

            dirname
        },
        retry=function() NULL)
}


##-----------------------------------------------------------------------------
createDesignParamsWithRestarts <- function(txtdir) {
    ## :TODO: Refit getFitParamsInput to take default param args, not hardcode
    withRestarts({
            do.call(SuperCurve::RPPADesignParams,
                    getDesignParamsInput(txtdir))
        },
        retry=function() NULL)
}


##-----------------------------------------------------------------------------
createFitParamsWithRestarts <- function(txtdir) {
    ## :TODO: Refit getFitParamsInput to take default param args, not hardcode
    withRestarts({
            do.call(SuperCurve::RPPAFitParams,
                    SuperCurveGUI:::getFitParamsInput(txtdir))
        },
        retry=function() NULL)
}


##-----------------------------------------------------------------------------
.trim <- function(string) {
    stopifnot(is.character(string))

    lt <- gsub('^[[:space:]]+', '', string)  ## remove spaces on left side
    gsub('[[:space:]]+$', '', lt)            ## remove spaces on right side
}


##-----------------------------------------------------------------------------
## Create action area for left frame.
actionArea <- function(parent) {
    stopifnot(is.tkwin(parent))

    tkframe(parent, class="ActionArea")
}


##-----------------------------------------------------------------------------
## Create command area for left frame.
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
                             labelstring,
                             varname,
                             values) {
    ## Check arguments
    stopifnot(is.tkwin(parent))
    stopifnot(is.character(labelstring) && length(labelstring) == 1)
    stopifnot(is.tclVar(varname))
    stopifnot(length(values) > 1)

    ## Begin processing
    label <- tklabel(parent,
                     text=labelstring)

    ## Create option menu
    optmenu.args <- c(list(parent=parent,
                           variable=varname),
                      as.character(values))
    optmenu <- do.call("tkOptionMenu",
                       optmenu.args)

    ## Manage widgets
    tkgrid(label,
           optmenu)
    tkgrid.configure(label,
                     sticky="e")
    tkgrid.configure(optmenu,
                     sticky="w")

    return(optmenu)
}


##-----------------------------------------------------------------------------
displayDesignParamsDialog <- function(grouping.arg,
                                      ordering.arg,
                                      center.arg,
                                      control.arg) {
    ## Check arguments
    stopifnot(is.list(grouping.arg) && length(grouping.arg) >= 1)
    stopifnot(is.list(ordering.arg) && length(ordering.arg) >= 1)
    stopifnot(is.list(center.arg) && length(center.arg) >= 1)
    stopifnot(is.list(control.arg) && length(control.arg) >= 1)

    ## Begin processing

    ## Create toplevel shell and frame as its child
    design.toplevel <- tktoplevel()
    tkwm.title(design.toplevel, "Design Parameters")

    tkpack(design.frame <- tkframe(design.toplevel))

    ## Create area frames and separator
    tkpack(command.area <- commandArea(design.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(design.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(design.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="2m")

    labelstring <- paste("Specify the design parameters",
                         "to use for the selected slides.",
                         sep="\n")
    tkpack(instruct.label <- tklabel(command.area,
                                     justify="left",
                                     text=labelstring),
           pady="3m")

    tkpack(tklabel(banner.frame,
                   text="Design Parameters"))

    ## Create input section for 'grouping' argument
    tkpack(grouping.frame <- tkframe(command.area,
                                     class="ArgumentInput"))

    grouping.value <- tclVar(grouping.arg$default)   # "blockSample"
    grouping.optmenu <- createOptionMenu(grouping.frame,
                                         "Grouping:",
                                         grouping.value,
                                         grouping.arg$values)

    ## Create input section for 'ordering' argument
    tkpack(ordering.frame <- tkframe(command.area,
                                     class="ArgumentInput"))

    ordering.value <- tclVar(ordering.arg$default)   # "decreasing"
    ordering.optmenu <- createOptionMenu(ordering.frame,
                                         "Ordering:",
                                         ordering.value,
                                         ordering.arg$values)

    ## Create input section for 'center' argument
    tkpack(center.frame <- tkframe(command.area,
                                   class="ArgumentInput"))

    center.value <- tclVar(center.arg$default)       # "FALSE"
    center.optmenu <- createOptionMenu(center.frame,
                                       "Center?",
                                       center.value,
                                       center.arg$values)

    ## Create input section for 'controls' argument
    tkpack(controls.frame <- tklabelframe(command.area,
                                          text="Controls",
                                          class="ArgumentInput"))

    controls.value <- tclVar(control.arg$default)
    tkgrid(tkentry(parent <- controls.frame,
                   width=60,
                   textvariable=controlvar <- controls.value))

    ## Create action area
    ok.button <- tkbutton(action.area,
                          command=function() tkdestroy(design.toplevel),
                          text="  OK  ")

    tkpack(ok.button,
           pady="3m")

    ## Display dialog
    tkwait.window(design.toplevel)

    return(list(grouping=tclvalue(grouping.value),
                ordering=tclvalue(ordering.value),
                center=tclvalue(center.value),
                controls=tclvalue(controls.value)))
}


##-----------------------------------------------------------------------------
displayFitParamsDialog <- function(measure.arg,
                                   model.arg,
                                   method.arg,
                                   trim.arg,
                                   ci.arg,
                                   ignoreNegative.arg) {
    ## Check arguments
    stopifnot(is.list(measure.arg) && length(measure.arg) >= 1)
    stopifnot(is.list(model.arg) && length(model.arg) >= 1)
    stopifnot(is.list(method.arg) && length(method.arg) >= 1)
    stopifnot(is.list(trim.arg) && length(trim.arg) >= 1)
    stopifnot(is.list(ci.arg) && length(ci.arg) >= 1)
    stopifnot(is.list(ignoreNegative.arg) && length(ignoreNegative.arg) >= 1)

    ## Begin processing

    ## Create toplevel shell and frame as its child
    fit.toplevel <- tktoplevel()
    tkwm.title(fit.toplevel, "Fit Parameters")

    tkpack(fit.frame <- tkframe(fit.toplevel))

    ## Create area frames and separator
    tkpack(command.area <- commandArea(fit.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(fit.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(fit.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="2m")

    labelstring <- paste("Specify the fit parameters",
                         "to use for the selected slides.",
                         sep="\n")
    tkpack(instruct.label <- tklabel(command.area,
                                     justify="left",
                                     text=labelstring),
           pady="3m")

    tkpack(tklabel(banner.frame,
                   text="Fit Parameters"))

    ## Create input section for 'measure' argument
    tkpack(measure.frame <- tkframe(command.area,
                                    class="ArgumentInput"))

    measure.value <- tclVar(measure.arg$default)   # "Mean.Total"
    measure.optmenu <- createOptionMenu(measure.frame,
                                        "Spot Measure:",
                                        measure.value,
                                        measure.arg$values)

    ## Create input section for 'model' argument
    tkpack(model.frame <- tkframe(command.area,
                                  class="ArgumentInput"))

    model.arg.labels <- sapply(model.arg$values,
                               SuperCurve::getRegisteredModelLabel)
    model.value <- tclVar(model.arg.labels[model.arg$default])   # "loess"
    model.optmenu <- createOptionMenu(model.frame,
                                      "Fit Model:",
                                      model.value,
                                      model.arg.labels)

    ## Create input section for 'method' argument
    tkpack(method.frame <- tkframe(command.area,
                                   class="ArgumentInput"))

    method.value <- tclVar(method.arg$default)   # "nlrob"
    method.optmenu <- createOptionMenu(method.frame,
                                       "Fit Method:",
                                       method.value,
                                       method.arg$values)

    ## Create input section for 'trim' argument
    tkpack(trim.frame <- tkframe(command.area,
                                 class="ArgumentInput"))

    trim.value <- tclVar(trim.arg$default)    # "2"
    tkgrid(tklabel(parent <- trim.frame,
                   text="Trim Level:"),
           tkentry(parent,
                   width=10,
                   textvariable=controlvar <- trim.value))

    ## Create input section for 'ci' argument
    tkpack(ci.frame <- tkframe(command.area,
                               class="ArgumentInput"))

    ci.value <- tclVar(ci.arg$default)   # "FALSE"
    ci.optmenu <- createOptionMenu(ci.frame,
                                   "Confidence Interval?",
                                   ci.value,
                                   ci.arg$values)

    ## Create input section for 'ignoreNegative' argument
    tkpack(ignoreNegative.frame <- tkframe(command.area,
                                           class="ArgumentInput"))

    ignoreNegative.value <- tclVar(ignoreNegative.arg$default)   # "FALSE"
    ignoreNegative.optmenu <- createOptionMenu(ignoreNegative.frame,
                                               "Ignore Negative?",
                                               ignoreNegative.value,
                                               ignoreNegative.arg$values)

    ## Create action area
    ok.button <- tkbutton(action.area,
                          command=function() tkdestroy(fit.toplevel),
                          text="  OK  ")

    tkpack(ok.button,
           pady="3m")

    ## Display dialog
    tkwait.window(fit.toplevel)

    return(list(measure=tclvalue(measure.value),
                model=names(which(model.arg.labels == tclvalue(model.value))),
                method=tclvalue(method.value),
                trim=tclvalue(trim.value),
                ci=tclvalue(ci.value),
                ignoreNegative=tclvalue(ignoreNegative.value)))
}


##-----------------------------------------------------------------------------
getDesignParamsInput <- function(txtdir,
                                 Class="RPPADesignParams") {
    ## Check arguments
    stopifnot(is.character(txtdir) && length(txtdir) == 1)
    stopifnot(is.character(Class) && length(Class) == 1)

    ##-------------------------------------------------------------------------
    getControls <- function(dirpath) {
        ## Check arguments
        stopifnot(is.character(dirpath) && length(dirpath) == 1)

        ## Begin processing
        pathname <- file.path(dirpath,
                              paste("slidedesign", "tsv", sep="."))
        spottype <- if (file.exists(pathname)) {
                        read.delim(pathname)$SpotType
                    } else {
                        as.factor(c("Blank",
                                    "Buffer",
                                    "NegCtrl",
                                    "PosCtrl",
                                    "Sample"))
                    }

        paste(levels(spottype)[spottype != "Sample"],
              collapse=", ")
    }


    ## Begin processing
    class.args <- formals(Class)

    ## Prepare possible input values
    grouping.arg <- list(default="blockSample",
                         values=eval(class.args$grouping))
    ordering.arg <- list(default="decreasing",
                         values=eval(class.args$ordering))
    center.arg <- list(default=as.character(FALSE),
                       values=as.character(c(TRUE, FALSE)))
    control.arg <- list(default=getControls(txtdir))

    ## Get user input
    input <- displayDesignParamsDialog(grouping.arg,
                                       ordering.arg,
                                       center.arg,
                                       control.arg)

    ## Postprocess
    input$center <- as.logical(input$center)
    input$controls <- sapply(unlist(strsplit(input$controls, ",")),
                             .trim,
                             USE.NAMES=FALSE)

    return(input)
}


##-----------------------------------------------------------------------------
getFitParamsInput <- function(txtdir,
                              Class="RPPAFitParams") {
    ## Check arguments
    stopifnot(is.character(txtdir) && length(txtdir) == 1)
    stopifnot(is.character(Class) && length(Class) == 1)

    ##-------------------------------------------------------------------------
    getIntensityMeasures <- function(path) {
        ## Check arguments
        stopifnot(is.character(path) && length(path) == 1)

        ##---------------------------------------------------------------------
        getRelevantColnamesFromQuantificationFile <- function(txtdir) {

            ##-----------------------------------------------------------------
            getQuantificationFilenames <- function(path) {
                ## Assumes all .txt files in the directory are slides
                txt.re <- ".*[tT][xX][tT]$"
                list.files(path=path, pattern=txt.re)
            }


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
                             error=function(e) {
                                 ## Provide basic defaults
                                 c("Mean.Net", "Mean.Total")
                             })

        return(measures)
    }


    ## Begin processing
    class.args <- formals(Class)

    ## Prepare possible input values
    measure.arg <- list(default="Mean.Total",
                        values=getIntensityMeasures(txtdir))
    model.arg <- list(default="loess",
                      values=SuperCurve::getRegisteredModelKeys())
    method.arg <- list(default="nlrob",
                       values=eval(class.args$method))
    trim.arg <- list(default=eval(class.args$trim))
    ci.arg <- list(default=as.character(FALSE),
                   values=as.character(c(TRUE, FALSE)))
    ignoreNegative.arg <- list(default=as.character(FALSE),
                               values=as.character(c(TRUE, FALSE)))

    ## Get user input
    input <- displayFitParamsDialog(measure.arg,
                                    model.arg,
                                    method.arg,
                                    trim.arg,
                                    ci.arg,
                                    ignoreNegative.arg)

    ## Postprocess
    #input$model <- names(input$model)
    input$trim <- as.integer(input$trim)
    input$ci <- as.logical(input$ci)
    input$ignoreNegative <- as.logical(input$ignoreNegative)
    input$warnLevel <- as.integer(-1)

    return(input)
}


##-----------------------------------------------------------------------------
getUserInput <- function(.path) {
    ## Check arguments
    stopifnot(is.null(.path) || (is.character(.path) && length(.path) == 1))

    ##-------------------------------------------------------------------------
    displayErrorAndRestart <- function(e) {
        stopifnot(inherits(e, "error"))

        tkmessageBox(icon="error",
                     message=as.character(e$message),
                     title="Error Occurred!",
                     type="ok")
        invokeRestart("retry")
    }


    ## Begin processing
    txtdir <- .path

    ## Get quantification directory
    withCallingHandlers({
            repeat {
                txtdir <- chooseQuantificationDirectoryWithRestarts(txtdir)
                if (!is.null(txtdir)) {
                    break
                }
            }
        },
        error=function(e) displayErrorAndRestart(e))

    parent.txtdir <- dirname(txtdir)

    ## Get image directory
    withCallingHandlers({
            repeat {
                imgdir <- chooseImageDirectoryWithRestarts(parent.txtdir)
                if (!is.null(imgdir)) {
                    break
                }
            }
        },
        error=function(e) displayErrorAndRestart(e))

    ## Get output directory
    withCallingHandlers({
            repeat {
                outdir <- chooseOutputDirectoryWithRestarts(parent.txtdir)
                if (!is.null(outdir)) {
                    break
                }
            }
        },
        error=function(e) displayErrorAndRestart(e))

    ## Get design parameters
    withCallingHandlers({
            repeat {
                designparams <- createDesignParamsWithRestarts(txtdir)
                if (!is.null(designparams)) {
                    break
                }
            }
        },
        error=function(e) displayErrorAndRestart(e))

    ## Get fit parameters
    withCallingHandlers({
            repeat {
                fitparams <- createFitParamsWithRestarts(txtdir)
                if (!is.null(fitparams)) {
                    break
                }
            }
        },
        error=function(e) displayErrorAndRestart(e))

    return(SuperCurveSettings(txtdir,
                              imgdir,
                              outdir,
                              designparams,
                              fitparams))
}


##-----------------------------------------------------------------------------
performAnalysis <- function(settings,
                            preexisting=FALSE) {
    ## Check arguments
    stopifnot(SuperCurve:::is.SuperCurveSettings(settings))
    stopifnot(is.logical(preexisting) && length(preexisting) == 1)

    ##-------------------------------------------------------------------------
    saveSettings <- function(settings,
                             overwrite) {
        ## Check arguments
        stopifnot(SuperCurve:::is.SuperCurveSettings(settings))
        stopifnot(is.logical(overwrite) && length(overwrite) == 1)

        ##---------------------------------------------------------------------
        makeFileHeader <- function(string) {
            stopifnot(is.character(string) && length(string) == 1)

            paste("###",
                  paste("###", string),
                  "###",
                  "\n",
                  sep="\n")
        }


        ## Begin processing
        outputdir <- as(settings@outdir, "character")

        ## Save settings (XDR format), if either overwrite or doesn't exist
        pathname <- file.path(outputdir,
                              paste("settings", "rda", sep="."))
        if (overwrite || !file.exists(pathname)) {
            save(settings, file=pathname, ascii=TRUE)
        }

        ## Save settings (human-readable text format)
        pathname <- file.path(outputdir,
                              paste("settings", "txt", sep="."))
        version <- packageDescription("SuperCurve", fields="Version")
        cat(makeFileHeader("SuperCurve settings"),
            paramString(settings),
            paste("supercurve version:", version), "\n",
            "\n",  # blank line at EOF
            sep="",
            file=pathname)
    }


    ## Begin processing
    question <- paste("Run analysis with the following options:",
                      "\n\n",
                      paramString(settings),
                      sep="")
    response <- tkmessageBox(default="yes",
                             icon="question",
                             message=question,
                             title="Confirm",
                             type="yesno")

    if (as.character(response) == "no") {
        stop("Quantification aborted")
    }

    ## Save settings
    saveSettings(settings, preexisting)

    ## Perform analysis
    SuperCurve:::fitCurveAndSummarizeFromSettings(settings)
}


##-----------------------------------------------------------------------------
## Idle task that simply reschedules itself. Although it may seem pointless, it
## gives R a constant sliver of time (w/o user interaction) for its event loop.
idleTask <- function() {
    tclafter(2000, idleTask)
}


##-----------------------------------------------------------------------------
## Prompt user for parameters and run SuperCurve using specified directories

## :TODO: Most of the hard-coded choices should be converted to a data-driven
## model that allows us to read them from a table or flat file somewhere. The
## idea should be to allow new classes to "register" themselves somewhere and
## thus get added to the GUI automagically without having to modify the code
## here. This method was used by the 'affy' class in Bioconductor to allow for
## the easy plug-in of new methods. Specifically, we previously worked out a
## model that had three basic processing steps:
##  [1] SpotLevel Corrections (which might just be local background correction
##      but might now involve Shannon's nested surface fits to the diluted
##      positive controls.
##  [2] Curve fitting, which now has at least three possible models. At
##      present, truncation (trimming) is included a part of this step, but
##      we might want to separate it to allow for alternative trimming
##      algorithms.
##  [3] Normalization, which is not presently included in the GUI, but should
##      be.
supercurveGUI <- function() {

    bannerFont <- "banner"

    ## Add entries to Tk option database
    local({
        ## Add widget resource values into options database
        initOptions(list("*BannerFrame.Label.font"=bannerFont,
                         "*BannerFrame.Label.justify"="left",
                         "*Entry.background"="white",
                         "*Entry.foreground"="black",
                         "*Entry.selectBackground"="yellow",
                         "*Entry.selectForeground"="black",
                         "*Dialog.msg.font"="courier",
                         "*Dialog.msg.wrapLength"="9i"))
    })

    ## Create named font for later use
    if (!(bannerFont %in% unlist(strsplit(tclvalue(tkfont.names()), " ")))) {
        #cat(sprintf("creating %s font", sQuote(bannerFont)), "\n")
        tkfont.create(bannerFont,
                      family="helvetica",
                      size=18,
                      weight="bold")
        on.exit({
            #cat("destroying font", "\n")
            tkfont.delete(bannerFont)
        })
    } else {
        #cat(sprintf("%s font already exists", sQuote(bannerFont)), "\n")
    }

    .path <- if (nzchar(scdir <- Sys.getenv("SC_DIR"))) {
                 scdir
             } else {
                 NULL
             }

    ## :KRC: Post-processing steps (truncation, normalization) should be added.
    ## Again, this needs to be data-driven.

    ## Give R some time to process its event loop
    tclafter.idle(idleTask)

    ## Get user input
    settings <- getUserInput(.path)

    Sys.setenv("SC_DIR"=as(settings@txtdir, "character"))

## :TBD: If an existing analysis exists, should it be archived to a
## subdirectory before proceeding (for comparison purposes)?

    ## Perform analysis
    ## :TODO: Need to pass whether the settings are new or existing
    performAnalysis(settings, TRUE)
}

sc <- supercurveGUI
scui <- supercurveGUI

