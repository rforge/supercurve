###
### TKPROGRESSBAR.R
###

options(warn=1)
require(tcltk) || stop("tcltk support is missing")


##
## Private Methods
##

##-----------------------------------------------------------------------------
.progressbar_canvas <- function(progressbar) {
    ## Check arguments
    stopifnot(is.tkwin(progressbar))
    stopifnot(tclvalue(tkwinfo.class(progressbar)) == "Progressbar")

    ## Begin processing
    return(evalq(userdata$Canvas, envir=progressbar$env))
}


##
## Public Methods
##

##-----------------------------------------------------------------------------
progressbar_create <- function(parent,
                               col) {
    ## Check arguments
    stopifnot(is.tkwin(parent))
    if (missing(col)) {
        col <- tclvalue(optiondb_get(parent, "color", "Color"))
    }
    stopifnot(is.character(col))

    ## Begin processing
    progressbar <- tkframe(parent,
                           class="Progressbar")

    value <- tclvalue(optiondb_get(progressbar, "length", "Length"))
    len <- as.numeric(value)
    canvas <- tkcanvas(progressbar,
                       background="white",
                       borderwidth=0,
                       height=20,
                       highlightthickness=0,
                       width=len)
    tkpack(canvas,
           expand=TRUE)

    userdata <- list(Canvas=canvas)
    assign("userdata", userdata, envir=progressbar$env)

    ## Create background for progress bar
    tkcreate(canvas,
             "rectangle",
             0, 0,
             0, 20,
             outline="",
             fill=col,
             tags="bar")

    ## Create text for progress bar (percent complete)
    tkcreate(canvas,
             "text",
             (0.5 * len),
             10,
             anchor="c",
             text="0%",
             tags="value")

    return(progressbar)
}


##-----------------------------------------------------------------------------
progressbar_value <- function(progressbar,
                              value) {
    ## Check arguments
    stopifnot(is.tkwin(progressbar))
    stopifnot(tclvalue(tkwinfo.class(progressbar)) == "Progressbar")

    if (!is.numeric(value)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("value")))
    } else if (!(length(value) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("value")))
    } else if (!(value >= 0 && value <= 100)) {
        stop(sprintf("argument %s must be in interval [%d, %d]",
                     sQuote("value"), 0, 100))
    }

    ## Begin processing
    canvas <- .progressbar_canvas(progressbar)

    ## Update displayed value
    tkitemconfigure(canvas,
                    "value",
                    text=sprintf("%3.0f%%", value))

    ## Adjust bar length
    w <- 0.01 * value * as.integer(tkwinfo.width(canvas))
    h <- as.integer(tkwinfo.height(canvas))
    tkcoords(canvas,
             "bar",
             0, 0,
             w, h)

    tclupdate()
}


##-----------------------------------------------------------------------------
progressbar_updatebarcolor <- function(progressbar,
                                       col) {
    ## Check arguments
    stopifnot(is.tkwin(progressbar))
    stopifnot(tclvalue(tkwinfo.class(progressbar)) == "Progressbar")
    stopifnot(is.character(col))

    ## Begin processing
    canvas <- .progressbar_canvas(progressbar)

    ## Set new color
    tkitemconfigure(canvas,
                    "bar",
                    fill=col)
}


##
## Tcl resource database
##

optiondb_add("*Progressbar.borderWidth", 2, "widgetDefault")
optiondb_add("*Progressbar.relief", "sunken", "widgetDefault")
optiondb_add("*Progressbar.length", 200, "widgetDefault")
optiondb_add("*Progressbar.color", "gray", "widgetDefault")

