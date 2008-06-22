###
### SUPERCURVEGUI.R
###


##-----------------------------------------------------------------------------
## Returns directory of file user selected via dialog
.chooseDirectoryOfFile <- function(title, initialdir, filetypes) {
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
.chooseDirectory <- function(title, initialdir) {
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
.editBox <- function(title, message, default="", width=40) {
    stopifnot(is.character(title)   && length(title) == 1)
    stopifnot(is.character(message) && length(message) == 1)
    stopifnot(is.character(default) && length(default) == 1)
    stopifnot(is.numeric(width)     && length(width) == 1)

    ## Create a new toplevel window
    tt <- tktoplevel()
    ## Give the window a title
    tkwm.title(tt, title)

    Text <- tclVar(default)
    done <- tclVar(0)
    width <- as.character(as.integer(width))
    textField <- tkentry(tt, width=width, textvariable=Text)
    tkgrid(tklabel(tt, text=message))
    tkgrid(textField)

    okButton <- tkbutton(tt,
                         text="   OK   ",
                         command=function() {
                             tclvalue(done) <- 1
                         })
    tkgrid(okButton)

    ## Capture destroy event (e.g., Alt-F4 in Windows)
    tkbind(tt,
           "<Destroy>",
           function() {
               tclvalue(done) <- 1
           })

    tkfocus(tt)
    tkwait.variable(done)

    TextVal <- tclvalue(Text)
    tkdestroy(tt)
    TextVal
}


##-----------------------------------------------------------------------------
## Given a list of button names, create a dialog box with those labels
## Return a number indiciating which button in the list was selected
.listButtonDialog <- function(title, message, buttonLabels) {
    stopifnot(is.character(title)   && length(title) == 1)
    stopifnot(is.character(message) && length(message) == 1)
    stopifnot(is.character(buttonLabels))

    ## Create a new toplevel window
    tt <- tktoplevel()

    ## Give the window a title
    tkwm.title(tt, title)

    ## :TBD:
    tkgrid(tklabel(tt, text=message))

    ## Create a variable to keep track of the state of the dialog window:
    ##   If window is active,                                        done = 0
    ##   If window has been closed using OK button,                  done = 1
    ##   If window has been closed using Cancel button or destroyed, done = 2
    done <- tclVar(0)

    ## Create buttons. For each button, set its value to the index number
    for (i in seq(1, length(buttonLabels))) {
        cmd <- substitute(function() {
                              tclvalue(done) <- res
                          },
                          list(res=i))
        tkgrid(tkbutton(tt, text=buttonLabels[i], command=eval(cmd)))
    }

    ## Capture destroy event (e.g., Alt-F4 in Windows)
    tkbind(tt,
           "<Destroy>",
           function() {
               tclvalue(done) <- 1
           })

    tkfocus(tt)

    ## Do not proceed until the variable done is non-zero.
    ##   (But other processes can still run, i.e. the system is not frozen.)
    tkwait.variable(done)

    ## The variable done is now non-zero, so record its value before
    ## destroying the window tt (If window is destroyed first, then
    ## done will be set to 2 because of earlier binding.)
    ## Determine whether user pressed OK (i.e., is done equal to 1).
    doneVal <- as.integer(tclvalue(done))
    tkdestroy(tt)

    doneVal
}


##-----------------------------------------------------------------------------
.logSettings <- function(settings, file) {
    stopifnot(is.character(settings))
    stopifnot(is.character(file) || inherits(file, "file"))

    version <- packageDescription("SuperCurve", fields="Version")
    cat("Run from GUI. Settings:", "\n",
        settings,
        paste("supercurve version =", version, "\n"),
        sep="",
        file=file)
}


##-----------------------------------------------------------------------------
.trim <- function(str) {
    stopifnot(is.character(str))

    lt <- gsub('^[[:space:]]+', '', str)  ## remove spaces on left side
    gsub('[[:space:]]+$', '', lt)         ## remove spaces on right side
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

    .path <- if (nzchar(scdir <- Sys.getenv("SC_DIR"))) {
                 scdir
             } else {
                 NULL
             }

    .path <- .chooseQuantificationDirectory(.path)

    if (!is.null(.path)) {
        Sys.setenv("SC_DIR"=.path)
    }

    tiffdir <- .chooseImageDirectory(dirname(.path))
    outputdir <- .chooseOutputDirectory(dirname(.path))

    net.total <- .listButtonDialog(title="Intensity Measure",
                                   message="Choose spot measure to use for quantification",
                                   c("Mean Net", "Mean Total"))
    ## :KRC: more general measures should be possible, especially if
    ## we allow spot-level adjustment
    measure <- switch(EXPR=net.total,
                      "Mean.Net",
                      "Mean.Total")

    ## :KRC: Needs to be data-driven so it extends to other models easily
    curve.model <- .listButtonDialog(title="Dilution Curve Model",
                                     message="Choose a model for fitting the antibody response curve",
                                     c("Monotone Increasing B-spline",
                                       "Loess",
                                       "Logistic"))
    model <- switch(EXPR=curve.model,
                    "cobs",
                    "loess",
                    "logistic")

    controls <- .editBox(title="Controls",
                         message="Names of control spots",
                         "control, pos con, neg con, blank",
                         width=60)
    controls <- unlist(strsplit(controls, ","))
    for (i in seq(1, length(controls))) {
        controls[i] <- .trim(controls[i])
    }

    ## :KRC: Post-processing steps (truncation, normalization) should be added.
    ## Again, this needs to be data-driven.
    settings <- paste("txt dir = ", shQuote(.path), "\n",
                      "tiff dir = ", shQuote(tiffdir), "\n",
                      "result dir = ", shQuote(outputdir), "\n",
                      "quantification type = ", shQuote(measure), "\n",
                      "dilution curve model = ", shQuote(model), "\n",
                      "control spot labels = ",
                      shQuote(paste(controls, collapse=", ")), "\n",
                      sep="")

    confirm <- tkmessageBox(message=paste("Run analysis with the following options:\n\n", settings, sep=""),
                            icon="question",
                            title="Confirm",
                            type="yesno")

    if (as.character(confirm) == "no") {
        print("Quantification aborted")
        return()
    }

    ## :KRC: This may be the most important step in the whole process!
    ## Can this be improved so that the algorithm can be run simply by
    ## loading the saved log information?
    .logSettings(settings, file.path(outputdir, "analysis.log"))

    ## Perform analysis
    designparams <- SuperCurve::RPPADesignParams(grouping="blockSample",
                                                 center=FALSE,
                                                 controls=list(controls))
    fitparams <- SuperCurve::RPPAFitParams(measure=measure,
                                           ignoreNegative=FALSE,
                                           method="nlrob",
                                           warnLevel=-1,
                                           model=model)

    fitset <- SuperCurve::RPPASet(.path, designparams, fitparams)
    ## :TODO: Convert this into a 'summary' method for the RPPASet class.
    ## I think this requires us to load the SuperCurve class explicitly
    ## in order for the S4 system to correctly perform method dispatch.
    SuperCurve::write.summary(fitset,
                              path=outputdir,
                              graphs=TRUE,
                              tiffdir=tiffdir)
}

sc <- supercurveGUI

