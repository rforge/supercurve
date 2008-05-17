###
### SUPERCURVEGUI.R
###

library(tcltk)


##-----------------------------------------------------------------------------
.browsePath <- function(title, initialdir=NULL) {
    fileName <- if (!is.null(initialdir)) {
                    tclvalue(tkgetOpenFile(title=title, initialdir=initialdir))
                } else {
                    tclvalue(tkgetOpenFile(title=title))
                }
    result <- if (!nchar(fileName)) {
                  NULL
              } else {
                  dirname(fileName)
              }
}


##-----------------------------------------------------------------------------
## Given a list of button names, create a dialog box with those labels
## Return a number indiciating which button in the list was selected
.listButtonDialog <- function(buttonLabels, title, msg) {
    ## Create a new toplevel window
    tt <- tktoplevel()

    ## Give the window a title
    tkwm.title(tt, title)
    tkgrid(tklabel(tt, text=msg))

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
.editBox <- function(title, msg, default="", width="40") {
    tt<-tktoplevel()
    tkwm.title(tt, msg)
    Text <- tclVar(default)
    done <- tclVar(0)
    entry.Text <-tkentry(tt, width="20", textvariable=Text)
    tkgrid(tklabel(tt, text=msg))
    tkgrid(entry.Text)

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
.trim <- function(str) {
    lt <- gsub('^[[:space:]]+', '', str)
    rt <- gsub('[[:space:]]+$', '', lt)
    rt
}


##-----------------------------------------------------------------------------
## Prompt user for parameters and run SuperCurve using specified directories
supercurveGUI <- function() {

    .path <- Sys.getenv("SC_DIR")
    if (nchar(.path) < 1) {
       .path <- NULL
    }

    .path <- if (is.null(.path)) {
                 .browsePath("Select quantification file (*.txt) directory")
             } else {
                 .browsePath("Select quantification file (*.txt) directory",
                             .path)
             }

    if (!is.null(.path)) {
        Sys.setenv("SC_DIR"=.path)
    }

    tiffdir <- .browsePath("Select image (*.tif) directory",
                           dirname(.path))
    results <- .browsePath("Select output directory for results",
                           dirname(.path))
    net.total <- .listButtonDialog(c("Mean Net", "Mean Total"),
                                   "Intensity Measure",
                                   "Choose spot measure to use for quantification")
    measure <- switch(EXPR=net.total,
                      "Mean.Net",
                      "Mean.Total")

    curve.model <- .listButtonDialog(c("Monotone Increasing B-spline",
                                       "Loess",
                                       "Logistic"),
                                     "Dilution Curve Model",
                                     "Choose a model for fitting the antibody response curve")
    model <- switch(EXPR=curve.model,
                    "cobs",
                    "loess",
                    "logistic")

    controls <- .editBox("Controls",
                         "Names of control spots",
                         "control, pos con, neg con",
                         width="60")
    controls <- unlist(strsplit(controls, ","))
    for (i in seq(1, length(controls))) {
        controls[i] <- .trim(controls[i])
    }

    settings <- paste("txt dir = ", sQuote(.path), "\n",
                      "tiff dir = ", sQuote(tiffdir), "\n",
                      "result dir = ", sQuote(results), "\n",
                      "quantification type = ", sQuote(measure), "\n",
                      "dilution curve model = ", sQuote(model), "\n",
                      "control spot labels = ",
                      sQuote(paste(controls, collapse=", ")),
                      sep="")

    confirm <- tkmessageBox(message=paste("Run analysis with the following options:\n\n", settings, sep=""),
                            type="yesno")

    if (as.character(confirm) == "no") {
        print("Quantification aborted")
        return()
    }

    version <- packageDescription("SuperCurve", fields="Version")

    cat("Run from GUI. Settings:", "\n",
        settings,
        paste("\nsupercurve version = ", version),
        sep="",
        file=file.path(results, "analysis.log"))

    designparams <- SuperCurve::RPPADesignParams(grouping="blockSample",
                                                 center=FALSE,
                                                 controls=list(controls))
    fitparams <- SuperCurve::RPPAFitParams(measure=measure,
                                           ignoreNegative=FALSE,
                                           method='nlrob',
                                           warnLevel=-1,
                                           model=model)

    fitset <- SuperCurve::RPPAFitDir(.path, designparams, fitparams)
    SuperCurve::write.summary(fitset,
                              namebase='supercurve',
                              path=results,
                              normalize='median',
                              graphs=TRUE,
                              tiffdir=tiffdir)
}

