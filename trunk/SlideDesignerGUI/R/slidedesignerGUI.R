###
### SLIDEDESIGNERGUI.R
###

require(tcltk) || stop("tcltk support is missing")
library(tclish)

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
        showerror(title="Error Occurred!",
                  message=as.character(result),
                  parent=getenv("toplevel"))
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
## :DEBUG: Displays pcseries data structure.
showpcseries <- function(text="pc series:") {
    stopifnot(is.character(text) && length(text) == 1)

    cat(text, "\n")
    pcseries <- getenv("pcseries")
    show(str(pcseries))
    flush.console()
}


##-----------------------------------------------------------------------------
## :DEBUG: Displays pcseriesgrid data structure.
showpcseriesgrid <- function(text="pc series grid:") {
    stopifnot(is.character(text) && length(text) == 1)

    cat(text, "\n")
    pcseriesgrid <- getenv("pcseriesgrid")
    show(pcseriesgrid)
    flush.console()
}


##-----------------------------------------------------------------------------
## Returns the widget ids of the argument's siblings.
getsiblings <- function(widget) {
    .appEntryStr("getsiblings")
    stopifnot(is.tkwin(widget))

    siblings <- as.character(NA)
    parent <- tkwinfo.parent(widget)
    children <- tclvalue(tkwinfo.children(parent))
    if (nzchar(children)) {
        children <- unlist(strsplit(children, " "))
        siblings <- children[!children %in% widget$ID]
    }

    return(siblings)
}


##-----------------------------------------------------------------------------
## Displays button widgets as "pressed in".
displayButtonAsPressed <- function(button) {
    .appEntryStr("displayButtonAsPressed")
    stopifnot(is.tkwin(button))
    stopifnot(tclvalue(tkwinfo.class(button)) == "Button")

    tkconfigure(button,
                relief="sunken")
}


##-----------------------------------------------------------------------------
## Displays button widgets as "normal".
displayButtonAsNormal <- function(button) {
    .appEntryStr("displayButtonAsNormal")
    stopifnot(is.tkwin(button))
    stopifnot(tclvalue(tkwinfo.class(button)) == "Button")

    tkconfigure(button,
                relief="raised")
}


##-----------------------------------------------------------------------------
## Displays argument as pressed and all sibling buttons as normal.
updateButtonVisuals <- function(widget) {
    .appEntryStr("updateButtonVisuals")
    stopifnot(is.tkwin(widget))
    stopifnot(tclvalue(tkwinfo.class(widget)) == "Button")

    sapply(getsiblings(widget),
           function(sibling) {
               if (tclvalue(tkwinfo.class(sibling)) == "Button") {
                   displayButtonAsNormal(.Tk.newwin(sibling))
               }
           })
    displayButtonAsPressed(widget)
    tclupdate("idletasks")
}


##-----------------------------------------------------------------------------
## Destroys all children of the specified parent, but not the parent itself.
destroychildren <- function(parent) {
    .appEntryStr("destroychildren")
    stopifnot(is.tkwin(parent))

    ##-------------------------------------------------------------------------
    ## Destroys all children of the specified parent.
    .tkdestroychildren <- function(parent) {
        stopifnot(is.tkwin(parent))

        children <- tclvalue(tkwinfo.children(parent))
        if (nzchar(children)) {
            sapply(unlist(strsplit(children, " ")),
                   function(child) {
                       tcl("destroy", child)
                   })
        }
        invisible()
    }


    .tkdestroychildren(parent)
    assign("num.subwin", 0, env=parent$env)
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
## Returns encoded components from the SubgridAlias for positive controls.
decodePosCtrlSubgridAlias <- function(alias) {
    stopifnot(is.character(alias) && length(alias) == 1)
    parts <- unlist(strsplit(alias, "-", fixed=TRUE))
    stopifnot(length(parts) == 3)
    names(parts) <- c("spottype", "series", "position")
    stopifnot(parts["spottype"] == "PosCtrl")

    components <- as.list(parts)
    mode(components$series) <- "integer"
    mode(components$position) <- "integer"

    return(components)
}


##-----------------------------------------------------------------------------
## Returns SubgridAlias label for positive controls.
encodePosCtrlSubgridAlias <- function(series,
                                      pos) {
    stopifnot(is.numeric(series) && length(series) == 1)
    stopifnot(is.numeric(pos) && length(pos) == 1)
    return(sprintf("PosCtrl-%d-%d",
                   series,
                   pos))
}


##-----------------------------------------------------------------------------
## Returns encoded components from the SubgridAlias for samples.
decodeSampleSubgridAlias <- function(alias) {
    stopifnot(is.character(alias) && length(alias) == 1)
    parts <- unlist(strsplit(alias, "-", fixed=TRUE))
    stopifnot(length(parts) == 4)
    names(parts) <- c("spottype", "row", "series", "position")
    stopifnot(parts["spottype"] == "Sample")

    components <- as.list(parts)
    mode(components$row) <- "integer"
    mode(components$series) <- "integer"
    mode(components$position) <- "integer"

    return(components)
}


##-----------------------------------------------------------------------------
## Returns SubgridAlias label for samples.
encodeSampleSubgridAlias <- function(row,
                                     series,
                                     pos) {
    stopifnot(is.numeric(row) && length(row) == 1)
    stopifnot(is.numeric(series) && length(series) == 1)
    stopifnot(is.numeric(pos) && length(pos) == 1)
    return(sprintf("Sample-%d-%d-%d",
                   row,
                   series,
                   pos))
}


##-----------------------------------------------------------------------------
## Returns coordinates of button (as integer vector) based on its button label.
decodeButtonLabel <- function(labelstring) {
    stopifnot(is.character(labelstring) && length(labelstring) == 1)

    labelparts <- unlist(strsplit(labelstring, "-"))
    stopifnot(length(labelparts) == 4)
    return(c(as.integer(labelparts[2]),
             as.integer(labelparts[4])))
}


##-----------------------------------------------------------------------------
## Returns button label based on coordinates of button.
encodeButtonLabel <- function(row,
                              col) {
    stopifnot(is.numeric(row) && length(row) == 1)
    stopifnot(is.numeric(col) && length(col) == 1)

    return(sprintf("Row-%02d-Col-%02d",
                   row,
                   col))
}


##-----------------------------------------------------------------------------
## Generates a series of colors from darker to lighter.
generateColors <- function(n,
                           hue=300) {
    stopifnot(is.numeric(n) && length(n) == 1)
    stopifnot(is.numeric(hue) && length(hue) == 1)
    stopifnot(n > 0)
    stopifnot(hue >= 0 && hue <= 360)

    return(hcl(h=hue, c=85, l=seq(from=30, to=100, length=n)))
}


##-----------------------------------------------------------------------------
## Generates a series of colors denoting positive controls.
## Darker colors implies stronger intensity level.
generatePosCtrlDilutionColors <- function(n) {
    .appEntryStr("generatePosCtrlDilutionColors")

    varname <- "hue.posctrl.dilution"

    ## Is value "cached" in global variable?
    if (exists(varname, envir=guienv())) {
        posCtrlDilutionHue <- getenv(varname)
    } else {
        ## Grab value from option database instead
        rsrcClass <- "PosCtrlDilutionHue"
        rsrcName <- initLowercase(rsrcClass)
        value <- tclvalue(optiondb_get(rsrcName=rsrcName,
                                       rsrcClass=rsrcClass))
        posCtrlDilutionHue <- as.numeric(value)
        setenv(varname, posCtrlDilutionHue)
    }

    return(generateColors(n, hue=posCtrlDilutionHue))
}


##-----------------------------------------------------------------------------
## Generates a series of colors denoting samples.
## Darker colors implies stronger intensity level.
generateSampleDilutionColors <- function(n) {
    .appEntryStr("generateSampleDilutionColors")

    varname <- "hue.sample.dilution"

    ## Is value "cached" in global variable?
    if (exists(varname, envir=guienv())) {
        sampleDilutionHue <- getenv(varname)
    } else {
        ## Grab value from option database instead
        rsrcClass <- "SampleDilutionHue"
        rsrcName <- initLowercase(rsrcClass)
        value <- tclvalue(optiondb_get(rsrcName=rsrcName,
                                       rsrcClass=rsrcClass))
        sampleDilutionHue <- as.numeric(value)
        setenv(varname, sampleDilutionHue)
    }

    return(generateColors(n, hue=sampleDilutionHue))
}


##-----------------------------------------------------------------------------
## Idle task that simply reschedules itself. Although it may seem pointless, it
## gives R a constant sliver of time (w/o user interaction) for its event loop.
idleTask <- function() {
    tclafter(2000, idleTask)
}


##-----------------------------------------------------------------------------
## Has series of positive controls in dilution series been specified?
inDilutionSeries <- function() {
    return(getenv("currpc") != 0)
}


##-----------------------------------------------------------------------------
## Changes first letter of string to lowercase and returns the string.
initLowercase <- function(s) {
    return(paste(tolower(substring(s, 1, 1)),
                 substring(s, 2),
                 sep=""))
}


##-----------------------------------------------------------------------------
## Rounds up to nearest integer.
roundup <- function(x) {
    stopifnot(is.numeric(x) && length(x) == 1)

    return(trunc(x + 0.5))
}


##-----------------------------------------------------------------------------
## Used to mark/paint the buttons in the right frame. Works for marking
## blanks, buffer, negative controls, and positive controls.
##
## This IS the most complicated part of the code, since it needs to know
## how to change colors depending on the current state.
## The main switch has three branches:
##
##   blank, buffer, nc, or pc (marking a single level)
##   firstpc                  (marking most intense spot of dilution series)
##   lastpc                   (marking least intense spot of dilution series)
##
## :BUG: Previously marked controls are overwritten without warning
##
pressButtonCB <- function(widget) {
    .appEntryStr("pressButtonCB")
    stopifnot(is.tkwin(widget))
    stopifnot(tclvalue(tkwinfo.class(widget)) == "Button")


    ##-------------------------------------------------------------------------
    ## Changes background of selected widget to current color scheme.
    swapGrids <- function(i, j) {
        stopifnot(is.numeric(i) && length(i) == 1)
        stopifnot(is.numeric(j) && length(j) == 1)

        colorgrid <- getenv("colorgrid")
        altgrid <- getenv("altgrid")
        ## Swap colors
        current <- colorgrid[i, j]
        colorgrid[i, j] <- altgrid[i, j]
        altgrid[i, j] <- current
        ## Save changes
        setenv("colorgrid", colorgrid)
        setenv("altgrid", altgrid)
        ## Mark as modified
        setDocumentEdited(TRUE)

        ## Update affected widget
        subgrid.frame <- scrollframe_interior()
        tkconfigure(id <- sprintf("%s.%d.%d",
                                  subgrid.frame$ID,
                                  i,
                                  j),
                    background=colorgrid[i, j])
    }


    ##-------------------------------------------------------------------------
    ## Handles processing for setting most intense positive control.
    markFirstPC <- function(who, i, j) {
        stopifnot(is.character(who) && length(who) == 1)
        stopifnot(is.numeric(i) && length(i) == 1)
        stopifnot(is.numeric(j) && length(j) == 1)

        currpc <- getenv("currpc")
        pcseries <- getenv("pcseries")
        pcseriesgrid <- getenv("pcseriesgrid")

        ## Start dilution series
        highest <- pcseries[[currpc]]$High
        .appDebugStr(paste("Highest:", as.character(highest)))
        if (!nzchar(highest)) {
            ## Nothing marked yet
            .appDebugStr("nothing marked yet")
            swapGrids(i, j)
            pcseries[[currpc]]$High <- who
        } else if (highest == who) {
            ## Remove an existing mark
            .appDebugStr("remove existing")
            swapGrids(i, j)
            pcseries[[currpc]]$High <- ""
cat("after removal", "\n")
showpcseries()
        } else {
            ## Mark new, so unmark old
            .appDebugStr("old for new")
            swapGrids(i, j)
            ans <- decodeButtonLabel(highest)
            swapGrids(i=ans[1], j=ans[2])
            pcseries[[currpc]]$High <- who
        }
        setenv("pcseries", pcseries)
        .appDebugStr("done setting high positive control")
        showpcseries()
    }
    #debug(markFirstPC)


    ##-------------------------------------------------------------------------
    ## Handles processing for setting least intense positive control.
    markLastPC <- function(who, i, j) {
        stopifnot(is.character(who) && length(who) == 1)
        stopifnot(is.numeric(i) && length(i) == 1)
        stopifnot(is.numeric(j) && length(j) == 1)

        currpc <- getenv("currpc")
        pcseries <- getenv("pcseries")
        pcseriesgrid <- getenv("pcseriesgrid")
showpcseriesgrid("pc series grid (early):")

        ## Finish dilution series
        lowest <- pcseries[[currpc]]$Low
        .appDebugStr(paste("Lowest:", as.character(lowest)))
        pcseries[[currpc]]$Low <- who
        showpcseries()
        highest <- pcseries[[currpc]]$High
        ans <- decodeButtonLabel(highest)
        hi <- ans[1]
        hj <- ans[2]

        colorgrid <- getenv("colorgrid")
        subgrid.frame <- scrollframe_interior()

        if ((i == hi) || (j == hj) && nzchar(lowest)) {
            ## Unmark previous sequence
            tf.grid <- pcseriesgrid > currpc
            colorgrid[tf.grid] <- spottype2background("Unmarked")
            for (ui in seq_len(nrow(tf.grid))) {
                for (uj in seq_len(ncol(tf.grid))) {
                    if (tf.grid[ui, uj]) {
                        tkconfigure(id <- sprintf("%s.%d.%d",
                                                  subgrid.frame$ID,
                                                  ui,
                                                  uj),
                                    background=colorgrid[ui, uj])
                    }
                }
            }
            rm(tf.grid, ui, uj)
        }

        if (i == hi) {
            ## All in one row
            .appDebugStr("  one row")

            ## Mark new sequence
            n <- 1 + abs(j - hj)
            colors <- generatePosCtrlDilutionColors(n)
            for (p in seq_len(n)) {
                sign <- abs(hj - j) / (hj - j)
                id <- sprintf("%s.%d.%d",
                              subgrid.frame$ID,
                              i,
                              y <- hj - sign * (p - 1))
                .appDebugStr(paste("sign =", sign, ";  id =", id))
                tkconfigure(id,
                            background=colors[p])
                colorgrid[i, y] <- colors[p]
            }
            setenv("colorgrid", colorgrid)
            setenv("pcseries", pcseries)
showpcseriesgrid("pc series grid (before):")
            pcseriesgrid <- updateseriesgrid(pcseriesgrid,
                                             pcseries[[currpc]]$High,
                                             pcseries[[currpc]]$Low,
                                             currpc)
cat("updating pcseriesgrid", "\n")
            setenv("pcseriesgrid", pcseriesgrid)
show(ls(envir=guienv()))
showpcseriesgrid("pc series grid (after):")
        } else if (j == hj) {
            ## All in one column
            .appDebugStr("  one col")

            ## Mark new sequence
            n <- 1 + abs(i - hi)
            colors <- generatePosCtrlDilutionColors(n)
            for (p in seq_len(n)) {
                sign <- abs(hi - i) / (hi - i)
                id <- sprintf("%s.%d.%d",
                              subgrid.frame$ID,
                              x <- hi - sign * (p - 1),
                              j)
                .appDebugStr(paste("sign =", sign, ";  id =", id))
                tkconfigure(id,
                            background=colors[p])
                colorgrid[x, j] <- colors[p]
            }
            setenv("colorgrid", colorgrid)
            setenv("pcseries", pcseries)
#showpcseriesgrid("pc series grid (before):")
cat("pc series grid (before):", "\n")
show(pcseriesgrid)
flush.console()
            pcseriesgrid <- updateseriesgrid(pcseriesgrid,
                                             pcseries[[currpc]]$High,
                                             pcseries[[currpc]]$Low,
                                             currpc)
cat("updating pcseriesgrid", "\n")
            setenv("pcseriesgrid", pcseriesgrid)
show(ls(envir=guienv()))
showpcseriesgrid("pc series grid (after):")
        } else {
            ## Not allowed
            showerror(title="Invalid Selection",
                      message=paste("Must have all positive controls",
                                    "in one row or in one column."),
                      parent=getenv("toplevel"))
        }
    }
    #debug(markLastPC)


    ## Begin processing
    userdata <- get("userdata", envir=widget$env)
    i <- userdata$row
    j <- userdata$col

    who <- encodeButtonLabel(i, j)
    .appDebugStr(paste("who:", as.character(who)))

    switch(state <- getenv("state"),
           blank   =,
           buffer  =,
           nc      =,
           pc      = swapGrids(i, j),
           firstpc = markFirstPC(who, i, j),
           lastpc  = markLastPC(who, i, j))
}


##-----------------------------------------------------------------------------
## Sets up the alternative color grid.
makeAltGrid <- function(color) {
    stopifnot(is.character(color) && length(color) == 1)

    sr <- as.numeric(tclvalue(getenv("nsubrow")))
    sc <- as.numeric(tclvalue(getenv("nsubcol")))
    altgrid <- matrix(color, ncol=sc, nrow=sr)
    setenv("altgrid", altgrid)
}


##-----------------------------------------------------------------------------
## Fills the current grid to make it ready for color swapping by marking.
fillGrid <- function() {
    .appEntryStr("fillGrid")

    sr <- as.numeric(tclvalue(getenv("nsubrow")))
    sc <- as.numeric(tclvalue(getenv("nsubcol")))
    grid <- matrix(NA, ncol=sc, nrow=sr)
    subgrid.frame <- scrollframe_interior()
    for (i in seq_len(sr)) {
        for (j in seq_len(sc)) {
            grid[i, j] <- tclvalue(tkcget(id <- sprintf("%s.%d.%d",
                                                        subgrid.frame$ID,
                                                        i,
                                                        j),
                                          "-background"))
        }
    }
    setenv("colorgrid", grid)
}


##-----------------------------------------------------------------------------
## Returns TRUE if value is somehow "bad" as an integer value.
badinteger <- function(value) {
    return(inherits(value, "try-error") ||
           is.na(value) ||
           !identical(ceiling(value), floor(value)))
}


##-----------------------------------------------------------------------------
## Displays error dialog for invalid user-provided values.
displayInvalidValueDialog <- function(fieldname,
                                      value,
                                      minvalue,
                                      maxvalue) {
    .appEntryStr("displayInvalidValueDialog")
    stopifnot(is.character(fieldname) && length(fieldname) == 1)
    stopifnot(is.character(value) && length(value) == 1)
    stopifnot(is.numeric(minvalue) && length(minvalue) == 1)
    stopifnot(is.numeric(maxvalue) && length(maxvalue) == 1)

    errmsg <- sprintf("%s (%s) must be integer between %d and %d.",
                      fieldname,
                      value,
                      minvalue,
                      maxvalue)
    showerror(title="Invalid Value",
              message=errmsg,
              parent=getenv("toplevel"))
}


##-----------------------------------------------------------------------------
## Displays error dialog for invalid user-provided values.
displayNotMultipleDialog <- function(fieldname,
                                     value,
                                     nsamp.row) {
    .appEntryStr("displayNotMultipleDialog")
    stopifnot(is.character(fieldname) && length(fieldname) == 1)
    stopifnot(is.integer(value) && length(value) == 1)
    stopifnot(is.integer(nsamp.row) && length(nsamp.row) == 1)

    errmsg <- sprintf("%s (%s) must be integer multiple of %d.",
                      fieldname,
                      value,
                      nsamp.row)
    showerror(title="Invalid Value",
              message=errmsg,
              parent=getenv("toplevel"))
}


##-----------------------------------------------------------------------------
## Verify grid dimensions are numeric and within acceptable range.
verifyGridDimensions <- function() {
    .appEntryStr("verifyGridDimensions")

    ## Maximum grid dimensions
    mrmax <- 1000
    mcmax <- 1000
    srmax <- 30     # This better be overkill ...
    scmax <- 30     # This better be overkill ...

    ## Verify user input
    mr.value <- tclvalue(getenv("nmainrow"))
    mr <- try(as.numeric(mr.value))
    if (badinteger(mr) || mr < 1 || mr > mrmax) {
        displayInvalidValueDialog("Main Row", mr.value, 1, mrmax)
        return(-1)
    }

    mc.value <- tclvalue(getenv("nmaincol"))
    mc <- try(as.numeric(mc.value))
    if (badinteger(mc) || mc < 1 || mc > mcmax) {
        displayInvalidValueDialog("Main Col", mc.value, 1, mcmax)
        return(-1)
    }

    sr.value <- tclvalue(getenv("nsubrow"))
    sr <- try(as.numeric(sr.value))
    if (badinteger(sr) || sr < 1 || sr > srmax) {
        displayInvalidValueDialog("Sub Row", sr.value, 1, srmax)
        return(-1)
    }

    sc.value <- tclvalue(getenv("nsubcol"))
    sc <- try(as.numeric(sc.value))
    if (badinteger(sc) || sc < 1 || sc > scmax) {
        displayInvalidValueDialog("Sub Col", sc.value, 1, scmax)
        return(-1)
    }

    return(0)
}


##-----------------------------------------------------------------------------
## Paints the subgrid layout in the right-hand frame. It is invoked when the
## user presses the "Create Subgrid" button. Note that the overall design of
## the application has a single toplevel window divided into 'left.frame' and
## 'subgrid.frame' parts. Logical flow control in the application is handled
## in the left frame. The right frame starts out invisible until the user
## specifies valid dimensions.
createSubgrid <- function() {
    .appEntryStr("createSubgrid")

    ## Grab subgrid dimensions
    sr <- as.numeric(tclvalue(getenv("nsubrow")))
    sc <- as.numeric(tclvalue(getenv("nsubcol")))

    ## Remove existing components of subgrid frame (maybe dimensions changed)
    subgrid.frame <- scrollframe_interior()
    destroychildren(subgrid.frame)

    ##-------------------------------------------------------------------------
    mod <- function(i, n) {
        ## Modifies the usual '%%' operator so it can be used to index into
        ## arrays, which start at position 1 in R.
        x <- i %% n
        x[x == 0] <- n
        x
    }


    ## For the desired number of rows...
    hues <- c(0, 120, 240, 60, 180) # Useful color choices
    for (i in seq_len(sr)) {
        ## Make an internal frame to hold the row
        temp.frame <- tkframe(subgrid.frame,
                              class="SpotFrame")
        ## Fill row with correct number of columns
        createRow(temp.frame,
                  i,
                  sc,
                  hue=hues[mod(i, length(hues))])
        ## Manage the row
        tkgrid(temp.frame)
        tkgrid.configure(temp.frame,
                         sticky="ew")
    }

    ## Display the subgrid area onscreen, if necessary
    right.frame <- getenv("right.frame")
    ismapped <- as.logical(tkwinfo.ismapped(right.frame))
    if (!ismapped) {

        ##---------------------------------------------------------------------
        ## Update window manager's geometry to ensure display of right frame.
        wmGeometry <- function() {
            .appEntryStr("wmGeometry")

            tclupdate("idletasks")
            toplevel <- getenv("toplevel")
            #rf.reqwidth <- tclvalue(tkwinfo.reqwidth(right.frame))
            #cat("right.frame reqwidth:", rf.reqwidth, "\n")

            screenwidth <- as.integer(tclvalue(tkwinfo.screenwidth(toplevel)))
            screenheight <- as.integer(tclvalue(tkwinfo.screenheight(toplevel)))
            #cat("screen width:", screenwidth, "\n")
            #cat("screen height:", screenheight, "\n")

            reqwidth <- as.integer(tclvalue(tkwinfo.reqwidth(toplevel)))
            #cat("toplevel reqwidth:", reqwidth, "\n")
            height <- as.integer(tclvalue(tkwinfo.height(toplevel)))

            ## Don't exceed screen dimensions
            if (reqwidth > screenwidth) {
                cat("adjusted width to fit screen dimensions", "\n")
                reqwidth <- screenwidth
            }

            if (height > screenheight) {
                cat("adjusted height to fit screen dimensions", "\n")
                height <- screenheight
            }

            ## Update geometry
            geometry <- sprintf("%dx%d",
                                reqwidth,
                                height)
            #cat("new proposed geometry:", geometry, "\n")
            flush.console()
            tkwm.geometry(toplevel, geometry)
        }


        ##---------------------------------------------------------------------
        ## Update window manager's minimum width of toplevel such that some
        ## portion of right frame is always visible.
        wmMinSizeResetWidth <- function() {
            .appEntryStr("wmMinSizeResetWidth")

            tclupdate("idletasks")
            toplevel <- getenv("toplevel")
            left.frame <- getenv("left.frame")
            lf.width <- as.integer(tclvalue(tkwinfo.reqwidth(left.frame)))
            rf.width <- as.integer(tclvalue(tkwinfo.reqwidth(right.frame)))
            minwidth <- lf.width + as.integer(rf.width / 10)
            minheight <- as.integer(tclvalue(tkwinfo.height(toplevel)))
            tkwm.minsize(toplevel, minwidth, minheight)
        }


        tkpack(right.frame,
               fill="y",
               side="left")

        ## Resize window so right frame is visible
        tclafter.idle(wmGeometry)

        ## Ensure some portion of right frame is always onscreen
        tclafter.idle(wmMinSizeResetWidth)
    }

    setenv("pcseriesgrid", matrix(0.0, ncol=sc, nrow=sr))
    setenv("state", "gridmade")

    return(0)
}


##-----------------------------------------------------------------------------
## Creates a row of pushbuttons within the subgrid area.
## The key trick here is to combine 'substitute' and 'eval' in order to put the
## values of variables into the command invoked when you press the button.
createRow <- function(parent,
                      i,
                      m,
                      hue=0) {
    .appEntryStr("createRow")
    stopifnot(is.tkwin(parent))
    stopifnot(is.numeric(i) && length(i) == 1)
    stopifnot(is.numeric(m) && length(m) == 1)
    stopifnot(is.numeric(hue) && length(hue) == 1)

    for (j in seq_len(m)) {
        button <- tkbutton(parent,
                           text=encodeButtonLabel(i, j))
        userdata <- list(row=i, col=j)
        assign("userdata", userdata, env=button$env)
        cmd <- substitute(function() pressButtonCB(widget),
                          list(widget=button))
        tkconfigure(button,
                    command=eval(cmd))

        tkgrid(button,
               row="0",
               column=j-1,
               padx="1",
               pady="1")
    }
}


##-----------------------------------------------------------------------------
## After setting the grid size, mark the different kinds of negative controls.
negCtrlStepCB <- function() {
    .appEntryStr("negCtrlStepCB")

    state <- getenv("state")
    if (state == "initial") {
        ## Grid not yet made
        showerror(title="No Grid Found",
                  message="You have to create the grid first!",
                  parent=getenv("toplevel"))
        return(-1)
    } else if (state != "gridmade") {
        ## This is not supposed to happen
        errmsg <- sprintf("Unexpected state (%s) in %s.",
                          state,
                          "negCtrlStepCB")
        showerror(title="Internal Error",
                  message=errmsg,
                  parent=getenv("toplevel"))
        return(-1)
    }

    ## Remove existing components of left frame
    left.frame <- getenv("left.frame")
    destroychildren(left.frame)

    ## Create area frames and separator
    tkpack(command.area <- commandArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(left.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="2m")
    tkpack(tklabel(banner.frame,
                   text="Mark Spots"))

    labelstring <- paste("Press each button below, then",
                         "mark corresponding spots on",
                         "the displayed subgrid.",
                         sep="\n")
    tkpack(instruct.label <- tklabel(command.area,
                                     justify="left",
                                     text=labelstring),
           pady="5m")

    ##-------------------------------------------------------------------------
    ## Swaps between previous color and blank color (white).
    markBlankCB <- function(widget) {
        .appEntryStr("markBlankCB")
        stopifnot(is.tkwin(widget))

        setenv("state", "blank")
        labelstring <- "Marking blank spots..."
        tkconfigure(instruct.label,
                    text=labelstring)
        updateButtonVisuals(widget)
        fillGrid()
        makeAltGrid(spottype2background("Blank"))
    }


    ##-------------------------------------------------------------------------
    ## Swaps between previous color and buffer color (light gray).
    markBufferCB <- function(widget) {
        .appEntryStr("markBufferCB")
        stopifnot(is.tkwin(widget))

        setenv("state", "buffer")
        labelstring <- "Marking buffer spots..."
        tkconfigure(instruct.label,
                    text=labelstring)
        updateButtonVisuals(widget)
        fillGrid()
        makeAltGrid(spottype2background("Buffer"))
    }


    ##-------------------------------------------------------------------------
    ## Swaps between previous color and negative control color (medium gray).
    markNegCtrlCB <- function(widget) {
        .appEntryStr("markNegCtrlCB")
        stopifnot(is.tkwin(widget))

        setenv("state", "nc")
        labelstring <- "Marking negative controls..."
        tkconfigure(instruct.label,
                    text=labelstring)
        updateButtonVisuals(widget)
        fillGrid()
        makeAltGrid(spottype2background("NegCtrl"))
    }


    ##-------------------------------------------------------------------------
    ## Determine what to display in left frame next.
    proceed <- function() {
        if (askyesno(title="Decide",
                     message="Are positive controls in a dilution series?",
                     default="no",
                     parent=getenv("toplevel"))) {
            ## Handle PC series
            setenv("state", "addpc")
            posCtrlSeriesSetupStepCB()
        } else {
            ## Handle standalone PC
            posCtrlStepCB()
        }
    }


    ## Create action area
    blank.button <- tkbutton(action.area,
                             background=spottype2background("Blank"),
                             command=function() {
                                 markBlankCB(blank.button)
                             },
                             text="Mark Blank Spots")
    buffer.button <- tkbutton(action.area,
                              background=spottype2background("Buffer"),
                              command=function() {
                                  markBufferCB(buffer.button)
                              },
                              text="Mark Buffer Spots")
    negctrl.button <- tkbutton(action.area,
                               background=spottype2background("NegCtrl"),
                               command=function() {
                                   markNegCtrlCB(negctrl.button)
                               },
                               text="Mark Negative Controls")
    tkpack(blank.button,
           buffer.button,
           negctrl.button,
           pady="1m")
    next.button <- tkbutton(action.area,
                            command=proceed,
                            text="Next")
    tkpack(next.button,
           pady="3m")

    ## Swaps between previous color and negative control color (medium gray)
    setenv("state", "ready2mark")
}


##-----------------------------------------------------------------------------
## This is pure control-flow, since the next steps depend on whether we have
## one level of positive control, or many.
numPosCtrlLevelsStepCB.unused <- function() {
    .appEntryStr("numPosCtrlLevelsStepCB")

    if (!(getenv("state") %in% c("ready2mark", "blank", "buffer", "nc"))) {
        errmsg <- sprintf("Unexpected state (%s) in %s.",
                          getenv("state"),
                          "numPosCtrlLevelsStepCB")
        showerror(title="Internal Error",
                  message=errmsg,
                  parent=getenv("toplevel"))
    }

    ## Need this to prevent button presses from changing things right now...
    setenv("state", "waiting")

    ## Remove existing components of left frame
    left.frame <- getenv("left.frame")
    destroychildren(left.frame)

    ## Create area frames and separator
    tkpack(command.area <- commandArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(left.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="2m")
    tkpack(tklabel(banner.frame,
                   text="Type of Positive Control"))

    tkpack(tklabel(command.area,
                   text="Are positive controls in a dilution series?"),
           no.radio  <- tkradiobutton(command.area,
                                      text="No",
                                      variable="pcmulti",
                                      value=0),
           yes.radio <- tkradiobutton(command.area,
                                      text="Yes",
                                      variable="pcmulti",
                                      value=1),
           pady="1m")

    ##-------------------------------------------------------------------------
    ## Modifies flow control based on user response.
    posCtrlTypeStepCB <- function() {
        if (tclvalue("pcmulti") == 1) {
            setenv("state", "addpc")
            posCtrlSeriesSetupStepCB()
        } else {
            posCtrlStepCB()
        }
    }


    ## Create action area
    next.button <- tkbutton(action.area,
                            command=posCtrlTypeStepCB,
                            text="Next")
    tkpack(next.button,
           pady="1m")
    tcl("set", "pcmulti", 0)  # Explicitly default to 'No'
}


##-----------------------------------------------------------------------------
## Setup to add positive control series.
posCtrlSeriesSetupStepCB <- function() {
    .appEntryStr("posCtrlSeriesSetupStepCB")

    if (!(getenv("state") %in% c("addpc", "lastpc"))) {
        errmsg <- sprintf("Unexpected state (%s) in %s.",
                          getenv("state"),
                          "posCtrlSeriesSetupStepCB")
        showerror(title="Internal Error",
                  message=errmsg,
                  parent=getenv("toplevel"))
    }

    ## Need this to prevent button presses from changing things right now...
    setenv("state", "waiting")

    ## Remove existing components of left frame
    left.frame <- getenv("left.frame")
    destroychildren(left.frame)

    ## Create area frames and separator
    tkpack(command.area <- commandArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(left.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="2m")
    tkpack(tklabel(banner.frame,
                   text="Mark Spots"))

    labelstring <- paste("Specify the steprate used",
                         "to calculate dilution levels",
                         "for this individual series",
                         "of positive controls (each",
                         "series may use a different",
                         "value).",
                         sep="\n")
    tkpack(instruct.label <- tklabel(command.area,
                                     justify="left",
                                     text=labelstring),
           pady="8m")

    csr.frame <- tkframe(command.area)

    ## Control StepRate
    csr.label <- tklabel(csr.frame,
                         text="Control StepRate:")
    csr.entry <- tkentry(csr.frame,
                         textvariable=getenv("control.steprate"),
                         width="6")
    tkbind(csr.entry,
           "<FocusIn>",
           function() entry_focusgained(csr.entry))
    tkbind(csr.entry,
           "<FocusOut>",
           function() entry_focuslost(csr.entry))
    tkfocus(csr.entry)

    tkgrid(csr.label,
           csr.entry)
    tkgrid.configure(csr.label,
                     sticky="e")
    tkgrid.configure(csr.entry,
                     sticky="w")
    tkpack(csr.frame,
           pady="3m")

    ##-------------------------------------------------------------------------
    ## Updates global variables for series information and begin next step.
    doAddPosCtrlSeriesStepCB <- function() {
        incrNumPC <- function(csr) {
            .appEntryStr("incrNumPC")

            evalq(currpc <- currpc + 1, envir=guienv())

            currpc <- getenv("currpc")
            pcseries <- getenv("pcseries")
            pcseries[[currpc]] <- list(High="", Low="", StepRate=csr)

            setenv("pcseries", pcseries)
        }

        csr.value <- tclvalue(getenv("control.steprate"))
        csr <- try(as.numeric(csr.value))
        csrmax <- 5 # Maximum control steprate
        if (badinteger(csr) || csr < 1 || csr > csrmax) {
            displayInvalidValueDialog("Control step rate", csr.value, 1, csrmax)
            return(-1)
        }

        incrNumPC(csr)
        if (getenv("currpc") > 1) {
            showwarning(title="Danger, Will Robinson!",
                        message=paste("Ensure positive control series",
                                      "do not overlap."),
                        parent=getenv("toplevel"))
        }
        showpcseries()

        dualbutton.config <- FALSE
        #dualbutton.config <- TRUE
        if (dualbutton.config) {
            posCtrlSeriesStepCB()
        } else {
            posCtrlSeriesHighStepCB()
        }

        return(0)
    }


    ## Create action area
    add.button <- tkbutton(action.area,
                           command=doAddPosCtrlSeriesStepCB,
                           text="Add Positive Control Series")
    next.button <- tkbutton(action.area,
                            command=assembleStepCB,
                            text="Next")
    tkpack(add.button,
           next.button,
           pady="1m")
    showpcseries()
}


##-----------------------------------------------------------------------------
## Mark positive controls (non-series).
posCtrlStepCB <- function() {
    .appEntryStr("posCtrlStepCB")

    ## Remove existing components of left frame
    left.frame <- getenv("left.frame")
    destroychildren(left.frame)

    ## Create area frames and separator
    tkpack(command.area <- commandArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(left.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="2m")
    tkpack(tklabel(banner.frame,
                   text="Mark Spots"))

    labelstring <- paste("Press each button below, then",
                         "mark corresponding spots on",
                         "the displayed subgrid.",
                         sep="\n")
    tkpack(instruct.label <- tklabel(command.area,
                                     justify="left",
                                     text=labelstring),
           pady="8m")

    ##-------------------------------------------------------------------------
    ## Swaps between previous color and positive control color (dark magenta).
    markPosCtrlCB <- function(widget) {
        .appEntryStr("markPosCtrlCB")
        stopifnot(is.tkwin(widget))

        setenv("state", "pc")
        labelstring <- "Marking positive controls..."
        tkconfigure(instruct.label,
                    text=labelstring)
        displayButtonAsPressed(widget)
        fillGrid()
        makeAltGrid(spottype2background("PosCtrl"))
    }


    ## Create action area
    posctrl.button <- tkbutton(action.area,
                               background=spottype2background("PosCtrl"),
                               command=function() {
                                   markPosCtrlCB(posctrl.button)
                               },
                               text="Mark Positive Controls")
    tkpack(posctrl.button,
           pady="8m")
    next.button <- tkbutton(action.area,
                            command=assembleStepCB,
                            text="Next")
    tkpack(next.button,
           pady="1m")
}


##-----------------------------------------------------------------------------
## Mark positive controls (series).
posCtrlSeriesStepCB <- function() {
    .appEntryStr("posCtrlSeriesStepCB")

    ## Remove existing components of left frame
    left.frame <- getenv("left.frame")
    destroychildren(left.frame)

    ## Create area frames and separator
    tkpack(command.area <- commandArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(left.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="2m")
    tkpack(tklabel(banner.frame,
                   text="Mark Spots"))

    labelstring <- paste("Press each button below, then",
                         "mark corresponding spots on",
                         "the displayed subgrid.",
                         sep="\n")
    tkpack(instruct.label <- tklabel(command.area,
                                     justify="left",
                                     text=labelstring),
           pady="8m")

    ##-------------------------------------------------------------------------
    ## Swaps between previous color and positive control color (dark magenta).
    ## Also has other indirect effects implemented through pressButtonCB method.
    markPosCtrlHighCB <- function(widget) {
        .appEntryStr("markPosCtrlHighCB")
        stopifnot(is.tkwin(widget))

        setenv("state", "firstpc")
        labelstring <- "Marking most intense PC..."
        tkconfigure(instruct.label,
                    text=labelstring)
        updateButtonVisuals(widget)
        fillGrid()
        makeAltGrid(spottype2background("PosCtrl", "High"))
    }


    ##-------------------------------------------------------------------------
    ## Swaps between previous color and positive control color (magenta).
    ## Also has other indirect effects implemented through pressButtonCB method.
    markPosCtrlLowCB <- function(widget) {
        .appEntryStr("markPosCtrlLowCB")
        stopifnot(is.tkwin(widget))

        currpc <- getenv("currpc")
        pcseries <- getenv("pcseries")

        if (nzchar(pcseries[[currpc]]$High)) {
            cat("**posctrllo**", "\n")
            setenv("state", "lastpc")
            labelstring <- "Marking least intense PC..."
            tkconfigure(instruct.label,
                        text=labelstring)
            updateButtonVisuals(widget)
            fillGrid()
            makeAltGrid(spottype2background("PosCtrl", "Low"))
        } else {
            showerror(title="Incomplete Series",
                      message=paste("You have to mark the most",
                                    "intense positive control first!"),
                      parent=getenv("toplevel"))
        }
    }


    ##-------------------------------------------------------------------------
    ## Verify series is completely specified before adding any more.
    proceed <- function() {
        .appDebugStr("**checking**")
        currpc <- getenv("currpc")
        pcseries <- getenv("pcseries")

        if (nzchar(pcseries[[currpc]]$High) &&
            nzchar(pcseries[[currpc]]$Low)) {
            posCtrlSeriesSetupStepCB()
        } else {
            showerror(title="Incomplete Series",
                      message=paste("You have to finish marking",
                                    "the series first!"),
                      parent=getenv("toplevel"))
        }
    }


    ## Create action area
    posctrlhi.button <- tkbutton(action.area,
                                 background=spottype2background("PosCtrl",
                                                                "High"),
                                 command=function() {
                                     markPosCtrlHighCB(posctrlhi.button)
                                 },
                                 text="Mark Highest Positive Control")
    posctrllo.button <- tkbutton(action.area,
                                 background=spottype2background("PosCtrl",
                                                                "Low"),
                                 command=function() {
                                     markPosCtrlLowCB(posctrllo.button)
                                 },
                                 text="Mark Lowest Positive Control")
    tkpack(posctrlhi.button,
           posctrllo.button,
           pady="1m")
    next.button <- tkbutton(action.area,
                            command=proceed,
                            text="Next")
    tkpack(next.button,
           pady="3m")
}


##-----------------------------------------------------------------------------
## Mark positive control with highest intensity (series).
posCtrlSeriesHighStepCB <- function() {
    .appEntryStr("posCtrlSeriesHighStepCB")

    ## Remove existing components of left frame
    left.frame <- getenv("left.frame")
    destroychildren(left.frame)

    ## Create area frames and separator
    tkpack(command.area <- commandArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(left.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="3m")
    tkpack(tklabel(banner.frame,
                   text="Mark Spots"))

    labelstring <- paste("Press the button below, then",
                         "mark corresponding spot on",
                         "the displayed subgrid.",
                         sep="\n")
    tkpack(instruct.label <- tklabel(command.area,
                                     justify="left",
                                     text=labelstring),
           pady="8m")

    ##-------------------------------------------------------------------------
    ## Swaps between previous color and positive control color (dark magenta).
    ## Also has other indirect effects implemented through pressButtonCB method.
    markPosCtrlHighCB <- function(widget) {
        .appEntryStr("markPosCtrlHighCB")
        stopifnot(is.tkwin(widget))

        setenv("state", "firstpc")
        labelstring <- "Marking most intense PC..."
        tkconfigure(instruct.label,
                    text=labelstring)
        displayButtonAsPressed(widget)
        fillGrid()
        makeAltGrid(spottype2background("PosCtrl", "High"))
    }


    ##-------------------------------------------------------------------------
    ## Verify series is completely specified before adding any more.
    proceed <- function() {
        .appDebugStr("**checking**")

        currpc <- getenv("currpc")
        pcseries <- getenv("pcseries")

        if (nzchar(pcseries[[currpc]]$High)) {
            setenv("state", "waiting")
            posCtrlSeriesLowStepCB()
        } else {
            showerror(title="Incomplete Series",
                      message=paste("You have to mark the most",
                                    "intense positive control first!"),
                      parent=getenv("toplevel"))
        }
    }


    ## Create action area
    posctrlhi.button <- tkbutton(action.area,
                                 background=spottype2background("PosCtrl",
                                                                "High"),
                                 command=function() {
                                     markPosCtrlHighCB(posctrlhi.button)
                                 },
                                 text="Mark Highest Positive Control")
    tkpack(posctrlhi.button,
           pady="1m")
    next.button <- tkbutton(action.area,
                            command=proceed,
                            text="Next")
    tkpack(next.button,
           pady="3m")
}


##-----------------------------------------------------------------------------
## Mark lowest intensity positive control (series).
posCtrlSeriesLowStepCB <- function() {
    .appEntryStr("posCtrlSeriesLowStepCB")

    ## Remove existing components of left frame
    left.frame <- getenv("left.frame")
    destroychildren(left.frame)

    ## Create area frames and separator
    tkpack(command.area <- commandArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(left.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="3m")
    tkpack(tklabel(banner.frame,
                   text="Mark Spots"))

    labelstring <- paste("Press the button below, then",
                         "mark corresponding spot on",
                         "the displayed subgrid.",
                         sep="\n")
    tkpack(instruct.label <- tklabel(command.area,
                                     justify="left",
                                     text=labelstring),
           pady="8m")

    ##-------------------------------------------------------------------------
    ## Swaps between previous color and positive control color (magenta).
    ## Also has other indirect effects implemented through pressButtonCB method.
    markPosCtrlLowCB <- function(widget) {
        .appEntryStr("markPosCtrlLowCB")
        stopifnot(is.tkwin(widget))

        setenv("state", "lastpc")
        labelstring <- "Marking least intense PC..."
        tkconfigure(instruct.label,
                    text=labelstring)
        displayButtonAsPressed(widget)
        fillGrid()
        makeAltGrid(spottype2background("PosCtrl", "Low"))
    }


    ##-------------------------------------------------------------------------
    ## Verify series is completely specified before adding any more.
    proceed <- function() {
        .appDebugStr("**checking**")
        currpc <- getenv("currpc")
        pcseries <- getenv("pcseries")

        if (nzchar(pcseries[[currpc]]$High) &&
            nzchar(pcseries[[currpc]]$Low)) {
            posCtrlSeriesSetupStepCB()
        } else {
            showerror(title="Incomplete Series",
                      message=paste("You have to finish marking",
                                    "the series first!"),
                      parent=getenv("toplevel"))
        }
    }


    ## Create action area
    posctrllo.button <- tkbutton(action.area,
                                 background=spottype2background("PosCtrl",
                                                                "Low"),
                                 command=function() {
                                     markPosCtrlLowCB(posctrllo.button)
                                 },
                                 text="Mark Lowest Positive Control")
    tkpack(posctrllo.button,
           pady="8m")
    next.button <- tkbutton(action.area,
                            command=proceed,
                            text="Next")
    tkpack(next.button)
}


##-----------------------------------------------------------------------------
## Assemble the subgrid for further use.
assembleStepCB <- function() {
    .appEntryStr("assembleStepCB")

    ## Remove existing components of left frame
    left.frame <- getenv("left.frame")
    destroychildren(left.frame)

    ## Create area frames and separator
    tkpack(command.area <- commandArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(left.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="2m")
    tkpack(tklabel(banner.frame,
                   text="Dilution"))

    labelstring <- paste("Specify the steprate used",
                         "to calculate dilution levels",
                         "for all samples, then assemble",
                         "the subgrid data fields.",
                         sep="\n")
    tkpack(instruct.label <- tklabel(command.area,
                                     justify="left",
                                     text=labelstring),
           pady="8m")

    ds.frame <- tkframe(command.area)

    ## Series Per Row
    nsr.label <- tklabel(ds.frame,
                         text="Series Per Row:")
    nsr.entry <- tkentry(ds.frame,
                         textvariable=getenv("dilution.nseries.row"),
                         width="6")

    tkbind(nsr.entry,
           "<FocusIn>",
           function() entry_focusgained(nsr.entry))
    tkbind(nsr.entry,
           "<FocusOut>",
           function() entry_focuslost(nsr.entry))

    ## Dilution StepRate
    dsr.label <- tklabel(ds.frame,
                         text="Dilution StepRate:")
    dsr.entry <- tkentry(ds.frame,
                         textvariable=getenv("dilution.steprate"),
                         width="6")
    cat("dsr.entry id = ", dsr.entry$ID, "\n")
    tkbind(dsr.entry,
           "<FocusIn>",
           function() entry_focusgained(dsr.entry))
    tkbind(dsr.entry,
           "<FocusOut>",
           function() entry_focuslost(dsr.entry))
    tkfocus(dsr.entry)

    tkgrid(nsr.label,
           nsr.entry)
    tkgrid(dsr.label,
           dsr.entry)
    tkgrid.configure(nsr.label,
                     dsr.label,
                     sticky="e")
    tkgrid.configure(nsr.entry,
                     dsr.entry,
                     sticky="w")
    tkpack(ds.frame,
           pady="3m")

    ##-------------------------------------------------------------------------
    ## Returns number of dilution samples per row if all rows contain same
    ## number of samples; otherwise, NA.
    getDilutionSamplesPerRow <- function() {
        colorgrid <- getenv("colorgrid")
        nr <- nrow(colorgrid)
        nc <- ncol(colorgrid)
        sg <- matrix(spottype(colorgrid), nr, nc)
        xxx <- matrix(match(sg, "Sample", 0), nr, nc)
        sum1 <- sum(xxx[1,])
        eligible <- if (sum1 > 0) {
                        ans <- sapply(rowSums(xxx), all.equal, sum1)
                        mode(ans) != "character"
                    } else {
                        FALSE
                    }
        nsamp.row <- if (eligible) {
                         as.integer(sum1)
                     } else {
                         as.integer(NA)
                     }

        return(nsamp.row)
    }


    ##-------------------------------------------------------------------------
    ## Assembles subgrid if user input valid.
    doAssembleStepCB <- function() {

        ## Make sure nseries/row is set and valid for this subgrid layout
        nsr.value <- tclvalue(getenv("dilution.nseries.row"))
        nsr <- try(as.numeric(nsr.value))
        if (!is.na(nsamp.row)) {
            nsrmax <- nsamp.row

            if (badinteger(nsr) || nsr < 1 || nsr > nsrmax) {
                displayInvalidValueDialog("Series per row",
                                          nsr.value,
                                          1,
                                          nsrmax)
                return(-1)
            }

            ## Ensure value is multiple of number of dilution samples per row
            mode(nsr) <- "integer"
            if (nsamp.row %% nsr) {
                displayNotMultipleDialog("Series per row",
                                         nsr,
                                         nsamp.row)
                return(-1)
            }
        }

        ## Make sure steprate is set and in the correct range
        dsr.value <- tclvalue(getenv("dilution.steprate"))
        dsr <- try(as.numeric(dsr.value))
        dsrmax <- 5 # Maximum dilution steprate
        if (badinteger(dsr) || dsr < 1 || dsr > dsrmax) {
            displayInvalidValueDialog("Dilution step rate",
                                      dsr.value,
                                      1,
                                      dsrmax)
            return(-1)
        }

        ## Assemble subgrid user input
        subgrid.df <- Try(assembleSubgrid(nsr, dsr))
        if (inherits(subgrid.df, "try-error")) {
            ## Assembly failed
            warnings()
            return(-1)
        } else {
            ## Eliminate extra column from 'View Subgrid' table
            rownames(subgrid.df) <- NULL
        }

        assign("subgrid.df", subgrid.df, envir=guienv())
        colorSamplesByDilution(subgrid.df)
        tkconfigure(next.button,
                    state="normal")
        setenv("state", "assembled")

        return(0)
    }


    ## If number of dilution samples per row is not consistent, disable entry
    nsamp.row <- getDilutionSamplesPerRow()
    if (is.na(nsamp.row)) {
        tkconfigure(nsr.entry,
                    state="readonly")
    }

    ## Create action area
    assemble.button <- tkbutton(action.area,
                                command=doAssembleStepCB,
                                text="Assemble Subgrid")
    tkpack(assemble.button,
           pady="8m")
    next.button <- tkbutton(action.area,
                            command=finalStepCB,
                            state="disabled",
                            text="Next")
    tkpack(next.button,
           pady="1m")
}


##-----------------------------------------------------------------------------
## Here is the final step, where we can choose to save the results.
finalStepCB <- function() {
    .appEntryStr("finalStepCB")

    ## Remove existing components of left frame
    left.frame <- getenv("left.frame")
    destroychildren(left.frame)

    ## Create area frames and separator
    tkpack(command.area <- commandArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(left.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="2m")
    tkpack(tklabel(banner.frame,
                   text="Assembled"))

    labelstring <- paste("Subgrid assembly complete.",
                         "Review the results and save",
                         "the grid design.",
                         sep="\n")
    tkpack(instruct.label <- tklabel(command.area,
                                     justify="left",
                                     text=labelstring),
           pady="8m")
    labelstring <- "Convert to single subgrid layout"
    tkpack(layout.checkbox <- tkcheckbutton(command.area,
                                            justify="left",
                                            text=labelstring,
                                            variable=getenv("singlesubgrid.layout")),
           pady="8m")

    ## Create action area
    view.button <- tkbutton(action.area,
                            command=function() {
                                viewSubgrid(getenv("subgrid.df"))
                            },
                            text="View Subgrid")
    save.button <- tkbutton(action.area,
                            command=function() {
                                saveGrid(getenv("subgrid.df"))
                            },
                            text="Save Grid As...")
    tkpack(view.button,
           save.button,
           pady="1m")

    setenv("state", "done")
}


##-----------------------------------------------------------------------------
## Modify background color of subgrid widgets representing samples to be based
## on dilution intensity.
colorSamplesByDilution <- function(df) {
    stopifnot(is.data.frame(df))

    ##-------------------------------------------------------------------------
    ## Returns the encoded series value from the SubgridAlias for samples.
    seriesFromAlias <- function(alias) {
        decodeSampleSubgridAlias(alias)$series
    }


    ## Update subgrid button widget backgrounds to appropriate dilution colors
    subgrid.frame <- scrollframe_interior()
    for (subrow in seq_len(max(df$Sub.Row))) {
        x.sample <- which(with(df, Sub.Row == subrow & SpotType == "Sample"))
        if (length(x.sample) > 0) {
            lastAliasInRow <- df$SubgridAlias[x.sample[length(x.sample)]]
            nseries <- seriesFromAlias(lastAliasInRow)
            for (series in seq_len(nseries)) {
                sample.re <- sprintf("^Sample-%d-%d-[[:digit:]]*$",
                                     subrow,
                                     series)
                aliases <- grep(sample.re,
                                df$SubgridAlias[x.sample],
                                value=TRUE)
                colors <- generateSampleDilutionColors(length(aliases))
                x.which <- which(df$SubgridAlias[x.sample] %in% aliases)
                for (subcol in df$Sub.Col[x.sample[x.which]]) {
                    x.colors <- df$Sub.Col[x.sample[x.which]] %in% subcol
                    tkconfigure(id <- sprintf("%s.%d.%d",
                                              subgrid.frame$ID,
                                              subrow, subcol),
                                background=colors[x.colors])
                }
            }
        }
    }
}


##-----------------------------------------------------------------------------
## Provides simple color request method depending on requested spot type.
spottype2background <- function(spottype=c("Blank",
                                           "Buffer",
                                           "NegCtrl",
                                           "PosCtrl",
                                           "Sample",
                                           "Unmarked"),
                                pos) {
    stopifnot(is.character(spottype) && length(spottype) == 1)

    ## Return color corresponding to spottype
    spottype <- match.arg(spottype)
    varname <- switch(spottype,
                      PosCtrl={
                          pos <- if (inDilutionSeries()) {
                                     match.arg(pos, c("High", "Low"))
                                 } else {
                                     "High"
                                 }
                          paste("background", tolower(spottype), tolower(pos),
                                sep=".")
                      },
                      paste("background", tolower(spottype), sep="."))

    ## Is value "cached" in global variable?
    if (exists(varname, envir=guienv())) {
        background <- getenv(varname)
    } else {
        ## Grab value from option database instead
        rsrcClass <- switch(spottype,
                            PosCtrl=paste(spottype, pos, "Background", sep=""),
                            paste(spottype, "Background", sep=""))
        rsrcName <- initLowercase(rsrcClass)
        background <- tclvalue(optiondb_get(rsrcName=rsrcName,
                                            rsrcClass=rsrcClass))
        setenv(varname, background)
    }

    return(background)
}


##-----------------------------------------------------------------------------
## In order to save, we have to convert the displayed colors (from colorgrid)
## into something meaningful. Positive controls in series will be handled by
## indexing since color alone is inadequate to determine this.
spottype <- function(x) {
    .appEntryStr("spottype")
    stopifnot(is.matrix(x))

    base <- c("Blank",
              "Buffer",
              "NegCtrl")
    names(base) <- c(spottype2background("Blank"),
                     spottype2background("Buffer"),
                     spottype2background("NegCtrl"))

    if (!inDilutionSeries()) {
        base <- c(base, "PosCtrl")
        names(base)[length(base)] <- spottype2background("PosCtrl")
    }

    spottypes <- base[as.vector(x)]

    if (inDilutionSeries()) {
        pcseriesgrid <- getenv("pcseriesgrid")
        x.grid <- which(pcseriesgrid != 0)
        spottypes[x.grid] <- "PosCtrl"
    }

    spottypes[is.na(spottypes)] <- "Sample"

    return(spottypes)
}


##-----------------------------------------------------------------------------
## Updates the PC series grid with the specified PC series information.
updateseriesgrid <- function(grid,
                             highest,
                             lowest,
                             x.series) {
    stopifnot(is.matrix(grid))
    stopifnot(is.character(highest) && length(highest) == 1)
    stopifnot(is.character(lowest) && length(lowest) == 1)
    stopifnot(is.numeric(x.series) && length(x.series) == 1)

    ## Zero any values in grid that exceed x.series
    grid[which(grid > x.series)] <- 0

    ## Get most intense spot location
    ans <- decodeButtonLabel(highest)
    hi.i <- ans[1]
    hi.j <- ans[2]
    cat("hi.i =", hi.i, "\n")
    cat("hi.j =", hi.j, "\n")

    ## Get least intense spot location
    ans <- decodeButtonLabel(lowest)
    lo.i <- ans[1]
    lo.j <- ans[2]
    cat("lo.i =", lo.i, "\n")
    cat("lo.j =", lo.j, "\n")

    ## Figure out the number of steps
    nsteps <- if (lo.i == hi.i) {
                  1 + abs(hi.j - lo.j)
              } else if (lo.j == hi.j) {
                  1 + abs(hi.i - lo.i)
              } else {
                  stop("nsteps calculation detected invalid input")
              }
    cat("nsteps =", nsteps, "\n")

    i <- if (hi.i > lo.i) {
             lo.i
         } else {
             hi.i
         }
    j <- if (hi.j > lo.j) {
             lo.j
         } else {
             hi.j
         }

    if (lo.i == hi.i) {
        seq.step <- if (hi.j > lo.j) {
                        rev(seq_len(nsteps))
                    } else {
                        seq_len(nsteps)
                    }
        cat("seq.step =", seq.step, "\n")
        for (x in seq_len(nsteps)) {
            grid[i, j+(x-1)] <- x.series + (seq.step[x] / 100)
        }
    } else {
        seq.step <- if (hi.i > lo.i) {
                        rev(seq_len(nsteps))
                    } else {
                        seq_len(nsteps)
                    }
        cat("seq.step =", seq.step, "\n")
        for (x in seq.step) {
            grid[i+(x-1), j] <- x.series + (seq.step[x] / 100)
        }
    }

    return(grid)
}


##-----------------------------------------------------------------------------
## Adds control level values to data frame based on spot type data.
addControlLevels <- function(df) {
    stopifnot(is.data.frame(df))

    x.negctrl <- which(with(df, SpotType == "Blank")  |
                       with(df, SpotType == "Buffer") |
                       with(df, SpotType == "NegCtrl"))
    df$ControlLevel[x.negctrl] <- 0

    currpc <- getenv("currpc")
    if (currpc != 0) {

        pcseries <- getenv("pcseries")
        pcseriesgrid <- getenv("pcseriesgrid")

        for (pc.series in rev(seq_len(currpc))) {
            x.grid <- which(pcseriesgrid > pc.series)
            if (!isTRUE(all.equal(pcseriesgrid[x.grid[1]],
                                  pc.series + 0.01,
                                  tolerance=0.0001))) {
                x.grid <- rev(x.grid)
            }

            cat("x.grid =", x.grid, "\n")
            show(pcseriesgrid[x.grid])

            step <- pcseries[[pc.series]]$StepRate
            intensity <- 100
            for (idx in x.grid) {
                if (df$SpotType[idx] == "PosCtrl") {
                    pc.pos <- roundup((pcseriesgrid[idx] - pc.series) * 100)
                    alias <- encodePosCtrlSubgridAlias(pc.series,
                                                       pc.pos)
                    df$SubgridAlias[idx] <- alias
                    df$ControlLevel[idx] <- intensity
                    intensity <- intensity / step
                }
            }

            pcseriesgrid[x.grid] <- 0
        }
    } else {
        x.posctrl <- which(with(df, SpotType == "PosCtrl"))
        df$ControlLevel[x.posctrl] <- 100
    }

    return(df)
}


##-----------------------------------------------------------------------------
## Labels dilution values for samples in data frame.
## Assumes all constraints have already been met.
labelDilutionAliases <- function(df, nsr=1) {
    stopifnot(is.data.frame(df))
    stopifnot(is.numeric(nsr) && length(nsr) == 1)

    saved.warn <- options(warn=1)
    on.exit(options(warn=as.integer(saved.warn)))

    ##-------------------------------------------------------------------------
    ## Determine how many spots each series can have.
    spotsPerSeries <- function(df, nseries.row) {
        if (nseries.row == 1) {
            # Add one extra so reset never triggered by reaching series maximum
            max(df$Sub.Col) + 1
        } else {
            x.sample <- which(with(df, Sub.Row == 1 & SpotType == "Sample"))
            nsamp.row <- length(x.sample)
            nsamp.row / nseries.row
        }
    }

    ## Determine number of positions in each series
    nspots.series <- spotsPerSeries(df, nsr)

    ## Label all samples in data frame
    reset <- FALSE
    for (subrow in seq_len(max(df$Sub.Row))) {
        samp.series <- as.integer(1)
        samp.pos <- as.integer(1)
        for (subcol in seq_len(max(df$Sub.Col))) {
            x.spot <- which(with(df, Sub.Row == subrow & Sub.Col == subcol))

            ## Is this spot a sample?
            if (df$SpotType[x.spot] == "Sample") {
                alias <- encodeSampleSubgridAlias(subrow,
                                                  samp.series,
                                                  samp.pos)
                df$SubgridAlias[x.spot] <- alias
                samp.pos <- as.integer(samp.pos + 1)

                ## Check if time for next series to begin
                if (samp.pos > nspots.series) {
                    reset <- TRUE
                }
            }

            ## Begin next sample dilution series
            if (reset) {
                samp.series <- as.integer(samp.series + 1)
                samp.pos <- as.integer(1)
                reset <- FALSE
            }
        }
    }

    return(df)
}


##-----------------------------------------------------------------------------
## Adds dilution values for samples to data frame.
addDilutionLevels <- function(df, step) {
    stopifnot(is.data.frame(df))
    stopifnot(is.numeric(step) && length(step) == 1)

    saved.warn <- options(warn=1)
    on.exit(options(warn=as.integer(saved.warn)))

    ##-------------------------------------------------------------------------
    ## Returns the encoded series value from the SubgridAlias for samples.
    seriesFromAlias <- function(alias) {
        decodeSampleSubgridAlias(alias)$series
    }


    ##-------------------------------------------------------------------------
    ## Returns the intensities for a particular series of samples.
    generateIntensities <- function(n, step) {
        stopifnot(is.numeric(n) && length(n) == 1)
        stopifnot(is.numeric(step) && length(step) == 1)

        intensity <- 100
        intensities <- numeric(n)
        for (i in seq_len(n)) {
            intensities[i] <- intensity
            intensity <- intensity / step
        }

        return(intensities)
    }


    ## Update subgrid cells to appropriate dilution intensities
    for (subrow in seq_len(max(df$Sub.Row))) {
        x.sample <- which(with(df, Sub.Row == subrow & SpotType == "Sample"))
        if (length(x.sample) > 0) {
            lastAliasInRow <- df$SubgridAlias[x.sample[length(x.sample)]]
            nseries <- seriesFromAlias(lastAliasInRow)
            for (series in seq_len(nseries)) {
                sample.re <- sprintf("^Sample-%d-%d-[[:digit:]]*$",
                                     subrow,
                                     series)
                aliases <- grep(sample.re,
                                df$SubgridAlias[x.sample],
                                value=TRUE)
                intensities <- generateIntensities(length(aliases), step)
                df$Dilution[match(aliases, df$SubgridAlias)] <- intensities
            }
        }
    }

    return(df)
}


##-----------------------------------------------------------------------------
## Uniquely identify each spot type within subgrid.
numberSubgridSpots <- function(df) {
    stopifnot(is.data.frame(df))

    ##-------------------------------------------------------------------------
    ## Uniquely identify each sample series within subgrid.
    numberSubgridSampleSeries <- function(df) {
        ##---------------------------------------------------------------------
        ## Returns the encoded series value from the SubgridAlias for samples.
        seriesFromAlias <- function(alias) {
            decodeSampleSubgridAlias(alias)$series
        }

        subgridnum <- as.integer(1)
        for (subrow in seq_len(max(df$Sub.Row))) {
            x.sample <- which(with(df, Sub.Row == subrow & SpotType == "Sample"))
            for (x in x.sample) {
                if (is.na(df$SubgridNum[x])) {
                    alias <- df$SubgridAlias[x]
                    spot.re <- sprintf("^Sample-%d-%d-[[:digit:]]*$",
                                       subrow,
                                       seriesFromAlias(alias))
                    x.spot <- grep(spot.re, df$SubgridAlias)

                    df$SubgridNum[x.spot] <- subgridnum
                    subgridnum <- as.integer(subgridnum + 1)
                }
            }
        }

        return(df)
    }


    ##-------------------------------------------------------------------------
    ## Uniquely identify each positive control series within subgrid.
    numberSubgridPosCtrlSeries <- function(df) {
        ##---------------------------------------------------------------------
        ## Returns the encoded series value from the SubgridAlias for positive
        ## controls.
        seriesFromAlias <- function(alias) {
            decodePosCtrlSubgridAlias(alias)$series
        }

        subgridnum <- as.integer(1)
        for (subrow in seq_len(max(df$Sub.Row))) {
            x.posctrl <- which(with(df, Sub.Row == subrow & SpotType == "PosCtrl"))
            for (x in x.posctrl) {
                if (is.na(df$SubgridNum[x])) {
                    alias <- df$SubgridAlias[x]
                    spot.re <- sprintf("^PosCtrl-%d-[[:digit:]]*$",
                                       seriesFromAlias(alias))
                    x.spot <- grep(spot.re, df$SubgridAlias)

                    df$SubgridNum[x.spot] <- subgridnum
                    subgridnum <- as.integer(subgridnum + 1)
                }
            }
        }

        return(df)
    }


    ##-------------------------------------------------------------------------
    ## Uniquely identify each standalone control within subgrid.
    numberSubgridStandaloneSpots <- function(df, x.spots) {
        subgridnum <- as.integer(1)
        for (x in x.spots) {
            df$SubgridNum[x] <- subgridnum
            subgridnum <- as.integer(subgridnum + 1)
        }

        return(df)
    }


    ##-------------------------------------------------------------------------
    ## Uniquely identify each standalone positive control within subgrid.
    numberSubgridPosCtrls <- function(df) {
        x.posctrl <- which(with(df, SpotType == "PosCtrl"))
        if (length(x.posctrl) > 0) {
            df <- numberSubgridStandaloneSpots(df, x.posctrl)
        }

        return(df)
    }


    ##-------------------------------------------------------------------------
    ## Uniquely identify each standalone negative control within subgrid.
    numberSubgridNegCtrls <- function(df) {
        x.negctrl <- which(with(df, SpotType == "NegCtrl"))
        if (length(x.negctrl) > 0) {
            df <- numberSubgridStandaloneSpots(df, x.negctrl)
        }

        x.buffer <- which(with(df, SpotType == "Buffer"))
        if (length(x.buffer) > 0) {
            df <- numberSubgridStandaloneSpots(df, x.buffer)
        }

        x.blank <- which(with(df, SpotType == "Blank"))
        if (length(x.blank) > 0) {
            df <- numberSubgridStandaloneSpots(df, x.blank)
        }

        return(df)
    }


    ## Number spots according to their type
    df <- numberSubgridSampleSeries(df)
    df <- if (inDilutionSeries()) {
              numberSubgridPosCtrlSeries(df)
          } else {
              numberSubgridPosCtrls(df)
          }
    df <- numberSubgridNegCtrls(df)

    return(df)
}


##-----------------------------------------------------------------------------
## Create subgrid dataframe from user input.
assembleSubgrid <- function(nsr, dsr) {
    .appEntryStr("assembleSubgrid")
    stopifnot(is.numeric(nsr) && length(nsr) == 1)
    stopifnot(is.numeric(dsr) && length(dsr) == 1)

    colorgrid <- getenv("colorgrid")
    nr <- nrow(colorgrid)
    nc <- ncol(colorgrid)

    subgrid.df <- data.frame(Sub.Row=as.integer(rep(1:nr, nc)),
                             Sub.Col=as.integer(rep(1:nc, each=nr)),
                             SubgridAlias=I(""),
                             SpotType=as.factor(spottype(colorgrid)),
                             SubgridNum=as.integer(NA),
                             ControlLevel=as.numeric(NA),
                             Dilution=as.numeric(NA))
    stopifnot(nlevels(subgrid.df$SpotType) <= 5)

    ## Provide initial values for SubgridAlias column
    subgrid.df$SubgridAlias <- levels(subgrid.df$SpotType)[subgrid.df$SpotType]

    ## :NOTE: following methods possibly update SubgridAlias column
    subgrid.df <- addControlLevels(subgrid.df)
    subgrid.df <- labelDilutionAliases(subgrid.df, nsr)

    ## :NOTE: following method updates SubgridNum column
    subgrid.df <- numberSubgridSpots(subgrid.df)

    ## :NOTE: following method possibly updates Dilution column
    subgrid.df <- addDilutionLevels(subgrid.df, dsr)

    ## Merge ControlLevel values for controls into Dilution column
    x.ctrl <- which(with(subgrid.df, SpotType != "Sample"))
    subgrid.df$Dilution[x.ctrl] <- subgrid.df$ControlLevel[x.ctrl]
    subgrid.df$ControlLevel <- NULL  # Remove afterwards

    ## Reorder data frame more sensibly
    return(subgrid.df[do.call(order, subgrid.df), ])
}


##-----------------------------------------------------------------------------
## Save the design to disk.
viewSubgrid <- function(df) {
    .appEntryStr("viewSubgrid")
    stopifnot(is.data.frame(df))

    ## :BUG: On Mac OS X, upon exit, the application will get a number of
    ## X11 protocol error (BadWindow) warnings. Assume this has something
    ## to do with this function being implemented in R and somehow event
    ## loop handling gets screwed up with Tcl/Tk and R both handling it.
    ## No problem on Windows. Not yet tested on Linux.
    invisible(View(df, "SubGrid Data"))
}


##-----------------------------------------------------------------------------
## Creates grid design by replicating subgrid layout for each mainrow, maincol.
createGrid <- function(subgrid.df) {
    stopifnot(is.data.frame(subgrid.df) && ncol(subgrid.df) == 6)

    ## Grab grid dimensions
    nmainrow <- as.numeric(tclvalue(getenv("nmainrow")))
    nmaincol <- as.numeric(tclvalue(getenv("nmaincol")))

    ##-------------------------------------------------------------------------
    ## Determine subgrid number for requested type.
    getSubgridNum <- function(df, spottype) {
        ans <- with(df, SubgridNum[SpotType == spottype])
        return(if (isTRUE(is.na(ans))) {
                   as.integer(0)
               } else {
                   ans
               })
    }

    ## Determine subgrid maximums
    max.subgridnum.sample  <- max(getSubgridNum(subgrid.df, "Sample"))
    max.subgridnum.posctrl <- max(getSubgridNum(subgrid.df, "PosCtrl"))
    max.subgridnum.negctrl <- max(getSubgridNum(subgrid.df, "NegCtrl"))
    max.subgridnum.buffer  <- max(getSubgridNum(subgrid.df, "Buffer"))
    max.subgridnum.blank   <- max(getSubgridNum(subgrid.df, "Blank"))

    ## Initialize grid offsets
    off.sample  <- as.integer(0)
    off.posctrl <- as.integer(0)
    off.negctrl <- as.integer(0)
    off.buffer  <- as.integer(0)
    off.blank   <- as.integer(0)

    ## Duplicate merged grid for each mainrow, maincol...
    grid.df <- NULL
    for (mr in seq_len(nmainrow)) {
        for (mc in seq_len(nmaincol)) {
            tmp.df <- cbind(Main.Row=as.integer(mr),
                            Main.Col=as.integer(mc),
                            subgrid.df,
                            GridAlias=I(""),
                            GridNum=as.integer(NA))

            ## Update GridNum column for each spottype
            x.sample <- tmp.df$SpotType == "Sample"
            tmp.df$GridNum[x.sample]  <- tmp.df$SubgridNum[x.sample] + off.sample

            x.posctrl <- tmp.df$SpotType == "PosCtrl"
            tmp.df$GridNum[x.posctrl] <- tmp.df$SubgridNum[x.posctrl] + off.posctrl

            x.negctrl <- tmp.df$SpotType == "NegCtrl"
            tmp.df$GridNum[x.negctrl] <- tmp.df$SubgridNum[x.negctrl] + off.negctrl

            x.buffer <- tmp.df$SpotType == "Buffer"
            tmp.df$GridNum[x.buffer]  <- tmp.df$SubgridNum[x.buffer] + off.buffer

            x.blank <- tmp.df$SpotType == "Blank"
            tmp.df$GridNum[x.blank]   <- tmp.df$SubgridNum[x.blank] + off.blank

            ## Create grid aliases
            tmp.df$GridAlias <- sprintf("%s-%d",
                                        as.character(tmp.df$SpotType),
                                        tmp.df$GridNum)
            tmp.df$SubgridNum <- NULL  # Remove afterwards
            tmp.df$GridNum    <- NULL  # Remove afterwards

            ## Update grid offsets
            off.sample  <- as.integer(off.sample  + max.subgridnum.sample)
            off.posctrl <- as.integer(off.posctrl + max.subgridnum.posctrl)
            off.negctrl <- as.integer(off.negctrl + max.subgridnum.negctrl)
            off.buffer  <- as.integer(off.buffer  + max.subgridnum.buffer)
            off.blank   <- as.integer(off.blank   + max.subgridnum.blank)

            grid.df <- if (is.null(grid.df)) {
                           tmp.df
                       } else {
                           rbind(grid.df, tmp.df)
                       }
        }
    }

    return(grid.df)
}


##-----------------------------------------------------------------------------
## Write the grid design to disk.
writeGridToFile <- function(grid.df, pathname) {
    .appEntryStr("writeGridToFile")
    stopifnot(is.data.frame(grid.df))
    stopifnot(is.character(pathname) && length(pathname) == 1)

    ## Attempt to save the results
    .appDebugStr(paste("Attempting to save grid as", dQuote(pathname)))

    result <- Try(write.table(grid.df,
                              file=pathname,
                              sep="\t",
                              quote=FALSE,
                              col.names=TRUE,
                              row.names=FALSE))
    if (inherits(result, "try-error")) {
         .appDebugStr("ERROR: *** save failed ***")
         ## Save to disk failed
         warnings()
         return(-1)
    } else {
        .appDebugStr("save successful.")

        ## Mark as clean
        setDocumentEdited(FALSE)
        return(0)
    }
}


##-----------------------------------------------------------------------------
## Converts from true layout to bastardized one used by labs for certain
## datasets.
truth2singlesubgrid <- function(grid.df) {
    .appEntryStr("truth2singlesubgrid")
    stopifnot(is.data.frame(grid.df))

    ## Re-arrange the row order into 'singlesubgrid' order
    orig.ord <- seq_len(nrow(grid.df))    # 1..5808
    n.mr <- max(grid.df$Main.Row)    # 4
    n.mc <- max(grid.df$Main.Col)    # 12
    n.sr <- max(grid.df$Sub.Row)     # 11
    n.sc <- max(grid.df$Sub.Col)     # 11

    nspots.mr <- n.mc * (n.sr * n.sc)         # number of spots per main row
    tmp.ord  <- ceiling(orig.ord / nspots.mr)
    tmp.ord2 <- ceiling(seq_len(nspots.mr) / (n.sr * n.sc))

    new.ord <- NULL
    for (i in seq_len(n.mr)) {
        tmp <- orig.ord[tmp.ord == i]

        mr <- NULL
        for (j in seq_len(n.mc)) {
            mr <- cbind(mr, matrix(tmp[tmp.ord2 == j], byrow=TRUE, ncol=n.sc))
        }
        new.ord <- rbind(new.ord, mr)
    }
    singlesubgrid.ord <- as.vector(t(new.ord))

    ## Single subgrid design layout
    singlesubgrid.df <- grid.df[singlesubgrid.ord, ]
    location.colnames <- 1:4
    truth.loc.df <- singlesubgrid.df[, location.colnames]
    names(truth.loc.df) <- c("Main.Row.Real",
                             "Main.Col.Real",
                             "Sub.Row.Real",
                             "Sub.Col.Real")
    sr.singlesubgrid <- rep(seq_len(n.mr * n.sr), each=(n.mc * n.sc))
    sc.singlesubgrid <- rep(seq_len(n.mc * n.sc), (n.mr * n.sr))
    singlesubgrid.df$Main.Row <- as.integer(1)
    singlesubgrid.df$Main.Col <- as.integer(1)
    singlesubgrid.df$Sub.Row  <- as.integer(sr.singlesubgrid)
    singlesubgrid.df$Sub.Col  <- as.integer(sc.singlesubgrid)

    ## Append actual location columns before returning data frame
    return(cbind(singlesubgrid.df, truth.loc.df))
}


##-----------------------------------------------------------------------------
## Save the grid design.
saveGrid <- function(subgrid.df) {
    .appEntryStr("saveGrid")
    stopifnot(is.data.frame(subgrid.df))

    value <- tclvalue(getenv("singlesubgrid.layout"))
    layoutAsSingleSubgrid <- as.logical(as.integer(value))

    defaultname <- if (!layoutAsSingleSubgrid) {
                       "slidedesign.tsv"
                   } else {
                       "slidedesign-singlesubgrid.tsv"
                   }

    pathname <- tclvalue(tkgetSaveFile(title="Save Grid",
                                       defaultextension=".tsv",
                                       initialfile=defaultname,
                                       parent=getenv("toplevel")))
    if (!nzchar(pathname)) {
        ## User canceled the save dialog
        return(-1)
    }

    grid.df <- createGrid(subgrid.df)
    if (layoutAsSingleSubgrid) {
        grid.df <- truth2singlesubgrid(grid.df)
    }

    return(writeGridToFile(grid.df, pathname))
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
        show(ls(envir=guienv()))
    }
}


##-----------------------------------------------------------------------------
## Initialize the Tcl option database with application defaults.
initOptions <- function(olist) {
    .appEntryStr("initOptions")
    stopifnot(is.list(olist))

    sapply(seq_along(olist),
           function(i, ll) {
               rsrc <- names(ll)[i]
               value <- ll[[i]]
               optiondb_add(rsrc, value, "startupFile")
           },
           olist)
}


##-----------------------------------------------------------------------------
## Creates scrolled frame used to contain subgrid area onscreen.
scrollframe_create <- function(parent) {
    stopifnot(is.tkwin(parent))

    frame <- tkframe(parent,
                     class="ScrollFrame")
    xscroll <- tkscrollbar(frame,
                           command=function(...) {
                               tkxview(vport, ...)
                           },
                           orient="horizontal")
    yscroll <- tkscrollbar(frame,
                           command=function(...) {
                               tkyview(vport, ...)
                           },
                           orient="vertical")
    vport <- tkcanvas(frame,
                      xscrollcommand=function(...) {
                          tkset(xscroll, ...)
                      },
                      yscrollcommand=function(...) {
                          tkset(yscroll, ...)
                      })

    pady <- paste("0", tclvalue(tkwinfo.reqheight(xscroll)))
    tkpack(yscroll, side="right", fill="y", pady=pady)
    tkpack(xscroll, side="bottom", fill="x")
    tkpack(vport, side="left", fill="both", expand=TRUE)

    int.frame <- tkframe(vport,
                         borderwidth=4,
                         relief="groove",
                         class="ScrollFrameInterior")
    tkcreate(vport, "window", "0 0", anchor="nw", window=int.frame$ID)
    tkbind(int.frame,
           "<Configure>",
           function() scrollframe_resize(int.frame))

    ## Save this so items can be put in it
    setenv("subgrid.frame", int.frame)

    return(frame)
}


##-----------------------------------------------------------------------------
## Resizes scrolled frame area when screen dimensions change.
scrollframe_resize <- function(iframe) {
    stopifnot(tclvalue(tkwinfo.class(iframe)) == "ScrollFrameInterior")

    w <- tkwinfo.width(iframe)
    h <- tkwinfo.height(iframe)

    vport <- tkwinfo.parent(iframe)
    stopifnot(tclvalue(tkwinfo.class(vport)) == "Canvas")
    bbox <- tkbbox(vport, "all")

    tkconfigure(vport,
                height=h,
                scrollregion=bbox,
                width=w,
                xscrollincrement="0.1i",
                yscrollincrement="0.1i")
}


##-----------------------------------------------------------------------------
## Returns portion of scrolled frame used to store user widgets.
scrollframe_interior <- function() {
    return(getenv("subgrid.frame"))
}


##-----------------------------------------------------------------------------
## Invoked upon gaining focus on bound text entry widgets.
entry_focusgained <- function(widget) {
    #.appEntryStr("entry_focusgained")
    stopifnot(is.tkwin(widget))
    stopifnot(tclvalue(tkwinfo.class(widget)) == "Entry")

    #cat("widget id =", widget$ID, "\n")
    tkselection.range(widget, 0, "end")
    #cat("exiting entry_focusgained", "\n")
}


##-----------------------------------------------------------------------------
## Invoked upon losing focus on bound text entry widgets.
entry_focuslost <- function(widget) {
    #.appEntryStr("entry_focuslost")
    stopifnot(is.tkwin(widget))
    stopifnot(tclvalue(tkwinfo.class(widget)) == "Entry")

    #cat("widget id =", widget$ID, "\n")
    if (as.logical(tkselection.present(widget))) {
        #cat("clearing selection", "\n")
        tkselection.clear(widget)
    }
    #cat("exiting entry_focuslost", "\n")
}


##-----------------------------------------------------------------------------
## Builds menu system tied to menubar usage.
buildMenus <- function(parent) {
    .appEntryStr("buildMenus")
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
        stopifnot(tclvalue(tkwinfo.class(file.menu)) == "Menu")

        ## Add menu items
        tkadd(file.menu,
              "command",
              label="Quit",
              command=appExit)
    }


    ##-------------------------------------------------------------------------
    ## Add HELP menu items.
    buildHelpMenu <- function(help.menu) {
        stopifnot(tclvalue(tkwinfo.class(help.menu)) == "Menu")

        ##---------------------------------------------------------------------
        ## Display overview dialog.
        overviewCB <- function() {
            overview <- paste("Tcl/Tk application for recording",
                              "the layout of positive and negative",
                              "controls of a slide design.")
            showinfo(title="Overview",
                     message=overview,
                     parent=getenv("toplevel"))
        }


        ##---------------------------------------------------------------------
        ## Display user guide in web browser window.
        userguideCB <- function() {
            ## :TODO: Update URL to point to SlideDesignerGUI user guide
            userguide.url <- "http://bioinformatics.mdanderson.org/Software/supercurve/"

            ## Ask web browser to display the URL
            browseURL(userguide.url)
        }


        ##---------------------------------------------------------------------
        ## Display about dialog.
        aboutCB <- function() {

            ##-----------------------------------------------------------------
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


            ##-----------------------------------------------------------------
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


            about <- paste(app.name <- "SlideDesignerGUI",
                           getAppVersionLabelstring(),
                           getTclTkVersionLabelstring(),
                           getTkWindowingSystemLabelstring(),
                           sep="\n")
            showinfo(title="About",
                     message=about,
                     parent=getenv("toplevel"))
        }


        ## Add menu items
        tkadd(help.menu,
              "command",
              label="Overview",
              command=overviewCB)
## :TODO: Disabled until user guide written...
##        tkadd(help.menu,
##              "command",
##              label="User Guide",
##              command=userguideCB)
        tkadd(help.menu,
              "command",
              label="About SlideDesignerGUI",
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
    .appEntryStr("appExit")

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

        ## Destroy toplevel
        tkdestroy(toplevel)
    }


    if (isDocumentEdited()) {
        ## Prompt user concerning possible data loss
        if (!askyesno(title="Really Quit?",
                      message="Discard subgrid modifications?",
                      default="no",
                      parent=getenv("toplevel"))) {
            cat("**quit canceled by user**", "\n")
            flush.console()
            return()
        }
    }

    ## Destroy toplevel indirectly to workaround problem with X11
    tclafter.idle(terminate)
}


##-----------------------------------------------------------------------------
## This is the only user-visible function in the file. You call this
## function to start the application.
slidedesignerGUI <- function() {
    ## Set WM_CLIENT_MACHINE property on root window
    tkwm.client('.', tclinfo.hostname())

    ## Create named font for later use
    availableFonts <- unlist(strsplit(tclvalue(tkfont.names()), " "))
    bannerFont <- "banner"
    if (!(bannerFont %in% availableFonts)) {
        #.appDebugStr(sprintf("creating %s font", sQuote(bannerFont)))
        tkfont.create(bannerFont,
                      family="helvetica",
                      size=18,
                      weight="bold")
    }

    ## Add entries to Tk option database
    local({
        ## Add "fake" resource values into options database
        initOptions(list("*posCtrlDilutionHue"="300",
                         "*sampleDilutionHue"="140"))
        sample.color <- generateSampleDilutionColors(1)
        posctrl.colors <- generatePosCtrlDilutionColors(2)
        initOptions(list("*blankBackground"="#FFFFFF",
                         "*bufferBackground"="#CCCCCC",
                         "*negCtrlBackground"="#AAAAAA",
                         "*posCtrlHighBackground"=posctrl.colors[1],
                         "*posCtrlLowBackground"=posctrl.colors[2],
                         "*sampleBackground"=sample.color,
                         "*unmarkedBackground"="#D2B48C"))

        ## Add widget resource values into options database
        unmarked <- spottype2background("Unmarked")
        initOptions(list("*BannerFrame.Label.font"=bannerFont,
                         "*BannerFrame.Label.justify"="left",
                         "*Entry.background"="white",
                         "*Entry.foreground"="black",
                         "*Entry.selectBackground"="yellow",
                         "*Entry.selectForeground"="black",
                         "*SpotFrame.Button.background"=unmarked))

        ## Handle app-defaults file(s), if any exist
        tkloadappdefaults(appdefaultsfile <- "slideDesignerGUI")
    })

    ## Create toplevel shell and pair of frames as its children
    toplevel <- tktoplevel()
    tktitle(toplevel) <- "Slide Designer"

    left.frame <- tkframe(toplevel,
                          borderwidth=4,
                          relief="groove")
    right.frame <- tkframe(toplevel,
                           borderwidth=4,
                           relief="groove")

    ## Initialize "global" variables
    initGlobals(list(altgrid=matrix(NA, nrow=1, ncol=1),   # Alternate colors
                     currpc=as.integer(0),
                     colorgrid=matrix(NA, nrow=1, ncol=1), # Current colors
                     control.steprate=tclVar("2"),
                     dilution.nseries.row=tclVar("1"),
                     dilution.steprate=tclVar("2"),
                     dirty=FALSE,
                     fonts=c(bannerFont),
                     labelstring="Press one of the buttons below.",
                     left.frame=left.frame,
                     nmainrow=tclVar("4"),
                     nmaincol=tclVar("12"),
                     nsubrow=tclVar("11"),
                     nsubcol=tclVar("11"),
                     pcseries=list(),
                     pcseriesgrid=matrix(as.numeric(0), nrow=1, ncol=1),
                     right.frame=right.frame,
                     singlesubgrid.layout=tclVar("0"),
                     subgrid.df=NULL,
                     state="initial",
                     subgrid.frame=tklabel(toplevel),      # placeholder
                     toplevel=toplevel))

    ## Create subgrid display area
    tkpack(tklabel(right.frame,
                   font=bannerFont,
                   text="Subgrid"),
           fill="x",
           side="top")
    tkpack(sframe <- scrollframe_create(right.frame),
           expand=TRUE,
           fill="both")

    ## Create area frames and separator
    tkpack(command.area <- commandArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")
    tkpack(separator    <- tkSeparator(left.frame),
           fill="x",
           pady="3m")
    tkpack(action.area  <- actionArea(left.frame),
           fill="x",
           padx="2m",
           pady="2m")

    ## Create command area
    tkpack(banner.frame <- bannerFrame(command.area),
           fill="x",
           pady="2m")

    labelstring <- paste("Specify the grid dimensions",
                         "and create the subgrid.",
                         sep="\n")
    tkpack(instruct.label <- tklabel(command.area,
                                     justify="left",
                                     text=labelstring),
           pady="3m")

    tkpack(tklabel(banner.frame,
                   text="Dimensions"))

    dim.frame <- tklabelframe(command.area,
                              text="Grid Dimensions")

    ## Main Row
    mr.label <- tklabel(dim.frame,
                        text="Main Row:")
    mr.entry <- tkentry(dim.frame,
                        textvariable=getenv("nmainrow"),
                        width="6")
    tkbind(mr.entry,
           "<FocusIn>",
           function() entry_focusgained(mr.entry))
    tkbind(mr.entry,
           "<FocusOut>",
           function() entry_focuslost(mr.entry))
    tkfocus(mr.entry)

    ## Main Column
    mc.label <- tklabel(dim.frame,
                        text="Main Col:")
    mc.entry <- tkentry(dim.frame,
                        textvariable=getenv("nmaincol"),
                        width="6")
    tkbind(mc.entry,
           "<FocusIn>",
           function() entry_focusgained(mc.entry))
    tkbind(mc.entry,
           "<FocusOut>",
           function() entry_focuslost(mc.entry))

    ## Sub Row
    sr.label <- tklabel(dim.frame,
                        text="Sub Row:")
    sr.entry <- tkentry(dim.frame,
                        textvariable=getenv("nsubrow"),
                        width="6")
    tkbind(sr.entry,
           "<FocusIn>",
           function() entry_focusgained(sr.entry))
    tkbind(sr.entry,
           "<FocusOut>",
           function() entry_focuslost(sr.entry))

    ## Sub Column
    sc.label <- tklabel(dim.frame,
                        text="Sub Col:")
    sc.entry <- tkentry(dim.frame,
                        textvariable=getenv("nsubcol"),
                        width="6")
    tkbind(sc.entry,
           "<FocusIn>",
           function() entry_focusgained(sc.entry))
    tkbind(sc.entry,
           "<FocusOut>",
           function() entry_focuslost(sc.entry))

    tkgrid(mr.label,
           mr.entry)
    tkgrid(mc.label,
           mc.entry)
    tkgrid(sr.label,
           sr.entry)
    tkgrid(sc.label,
           sc.entry)
    tkgrid.configure(mr.label,
                     mc.label,
                     sr.label,
                     sc.label,
                     sticky="e")
    tkgrid.configure(mr.entry,
                     mc.entry,
                     sr.entry,
                     sc.entry,
                     sticky="w")
    tkpack(dim.frame,
           fill="both",
           pady="3m")

    ##-------------------------------------------------------------------------
    ## Creates the subgrid if dimensions are valid.
    createSubgridCB <- function() {
        .appEntryStr("createSubgridCB")

        tkconfigure(toplevel,
                    cursor="watch")
        if (verifyGridDimensions() == 0) {
            createSubgrid()
            fillGrid()
            tkconfigure(next.button,
                        state="normal")
        }
        ## Restore previous cursor
        tkconfigure(toplevel,
                    cursor="")
    }


    ## Create action area
    subgrid.button <- tkbutton(action.area,
                               command=createSubgridCB,
                               text="Create Subgrid")
    next.button <- tkbutton(action.area,
                            command=negCtrlStepCB,
                            state="disabled",
                            text="Next")

    tkpack(subgrid.button,
           next.button,
           pady="3m")

    ## Don't bother displaying right side until subgrid created
    tkpack(left.frame,
           fill="y",
           side="left")

    tclafter.idle(tkbell())

    ## Create menus
    menubar <- tkmenu(toplevel)
    buildMenus(menubar)
    tkconfigure(toplevel,
                menu=menubar)

    ##-------------------------------------------------------------------------
    ## Set window manager's minimum width and height of toplevel such that
    ## requested size of left frame and menubar are always visible.
    wmMinSize <- function() {
        .appEntryStr("wmMinSize")

        tclupdate("idletasks")
        minwidth <- as.integer(tclvalue(tkwinfo.reqwidth(toplevel)))
        ## Add height of next button to compensate for menu size
        minheight <- as.integer(tclvalue(tkwinfo.reqheight(toplevel))) +
                     as.integer(tclvalue(tkwinfo.reqheight(next.button)))
        tkwm.minsize(toplevel, minwidth, minheight)
    }


    ## Process once mapped so window sizes are known...
    tclafter.idle(wmMinSize)

    ## Handle WM close button
    tkwm.protocol(toplevel,
                  "WM_DELETE_WINDOW",
                  function() {
                      message("[WM close: toplevel]")
                      appExit()
                  })

    ## Give R some time to process its event loop
    tclafter.idle(idleTask)

    ## Display application window
    tkwait.window(toplevel)
}

sdui <- slidedesignerGUI

