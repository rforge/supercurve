###
### SLIDEDESIGNERGUI.R
###

require(tcltk) || stop("tcltk support is missing")

tclpackage.require("Tcl", "8.4")               # Requires Tcl 8.4 or later


##
## Module Variables
##
dbg <- TRUE                                    # Toggle debugging on/off
.SDEnv <- new.env(hash=TRUE)                   # Private environment


##
## Methods
##


##-----------------------------------------------------------------------------
## Returns logical value indicating whether debugging support is enabled.
.appDebugEnabled <- function() {
    return(dbg)
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
.sdEntryStr <- function(fname) {

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
        tkmessageBox(icon="error",
                     message=as.character(result),
                     parent=getGlobal("toplevel"),
                     title="Error Occurred!",
                     type="ok")
    }

    return(result)
}


##-----------------------------------------------------------------------------
## Returns private environment for storing globals of SlideDesigner
sdenv <- function() {
    return(.SDEnv)
}


##-----------------------------------------------------------------------------
## Get global variable from private environment.
getGlobal <- function(name) {
    stopifnot(is.character(name) && length(name) == 1)

    Try(get(name, envir=sdenv()))
}


##-----------------------------------------------------------------------------
## Update value of global variable in private environment.
setGlobal <- function(name,
                      value) {
    stopifnot(is.character(name) && length(name) == 1)

    Try(assign(name, value, envir=sdenv()))
}


##-----------------------------------------------------------------------------
## :DEBUG: Displays pcseries data structure.
showpcseries <- function(text="pc series:") {
    stopifnot(is.character(text) && length(text) == 1)

    cat(text, "\n")
    pcseries <- getGlobal("pcseries")
    show(str(pcseries))
    flush.console()
}


##-----------------------------------------------------------------------------
## :DEBUG: Displays pcseriesgrid data structure.
showpcseriesgrid <- function(text="pc series grid:") {
    stopifnot(is.character(text) && length(text) == 1)

    cat(text, "\n")
    pcseriesgrid <- getGlobal("pcseriesgrid")
    show(pcseriesgrid)
    flush.console()
}


##-----------------------------------------------------------------------------
## Returns the widget ids of the argument's siblings.
getsiblings <- function(widget) {
    .sdEntryStr("getsiblings")
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
    .sdEntryStr("displayButtonAsPressed")
    stopifnot(is.tkwin(button))
    stopifnot(tclvalue(tkwinfo.class(button)) == "Button")

    tkconfigure(button,
                relief="sunken")
}


##-----------------------------------------------------------------------------
## Displays button widgets as "normal".
displayButtonAsNormal <- function(button) {
    .sdEntryStr("displayButtonAsNormal")
    stopifnot(is.tkwin(button))
    stopifnot(tclvalue(tkwinfo.class(button)) == "Button")

    tkconfigure(button,
                relief="raised")
}


##-----------------------------------------------------------------------------
## Displays argument as pressed and all sibling buttons as normal.
updateButtonVisuals <- function(widget) {
    .sdEntryStr("updateButtonVisuals")
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
    .sdEntryStr("destroychildren")
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
    .sdEntryStr("generatePosCtrlDilutionColors")

    varname <- "hue.posctrl.dilution"

    ## Is value "cached" in global variable?
    if (exists(varname, envir=sdenv())) {
        posCtrlDilutionHue <- getGlobal(varname)
    } else {
        ## Grab value from option database instead
        rsrcClass <- "PosCtrlDilutionHue"
        value <- tclvalue(optiondb_get(rsrcClass))
        posCtrlDilutionHue <- as.numeric(value)
        setGlobal(varname, posCtrlDilutionHue)
    }

    return(generateColors(n, hue=posCtrlDilutionHue))
}


##-----------------------------------------------------------------------------
## Generates a series of colors denoting samples.
## Darker colors implies stronger intensity level.
generateSampleDilutionColors <- function(n) {
    .sdEntryStr("generateSampleDilutionColors")

    varname <- "hue.sample.dilution"

    ## Is value "cached" in global variable?
    if (exists(varname, envir=sdenv())) {
        sampleDilutionHue <- getGlobal(varname)
    } else {
        ## Grab value from option database instead
        rsrcClass <- "SampleDilutionHue"
        value <- tclvalue(optiondb_get(rsrcClass))
        sampleDilutionHue <- as.numeric(value)
        setGlobal(varname, sampleDilutionHue)
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
    return(getGlobal("currpc") != 0)
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
    .sdEntryStr("pressButtonCB")
    stopifnot(is.tkwin(widget))
    stopifnot(tclvalue(tkwinfo.class(widget)) == "Button")


    ##-------------------------------------------------------------------------
    ## Changes background of selected widget to current color scheme.
    swapGrids <- function(i, j) {
        stopifnot(is.numeric(i) && length(i) == 1)
        stopifnot(is.numeric(j) && length(j) == 1)

        colorgrid <- getGlobal("colorgrid")
        altgrid <- getGlobal("altgrid")
        ## Swap colors
        current <- colorgrid[i, j]
        colorgrid[i, j] <- altgrid[i, j]
        altgrid[i, j] <- current
        ## Save changes
        setGlobal("colorgrid", colorgrid)
        setGlobal("altgrid", altgrid)
        ## Mark as modified
        evalq(dirty <- TRUE, envir=sdenv())

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

        currpc <- getGlobal("currpc")
        pcseries <- getGlobal("pcseries")
        pcseriesgrid <- getGlobal("pcseriesgrid")

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
        setGlobal("pcseries", pcseries)
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

        currpc <- getGlobal("currpc")
        pcseries <- getGlobal("pcseries")
        pcseriesgrid <- getGlobal("pcseriesgrid")
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

        colorgrid <- getGlobal("colorgrid")
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
            setGlobal("colorgrid", colorgrid)
            setGlobal("pcseries", pcseries)
showpcseriesgrid("pc series grid (before):")
            pcseriesgrid <- updateseriesgrid(pcseriesgrid,
                                             pcseries[[currpc]]$High,
                                             pcseries[[currpc]]$Low,
                                             currpc)
cat("updating pcseriesgrid", "\n")
            setGlobal("pcseriesgrid", pcseriesgrid)
show(ls(envir=sdenv()))
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
            setGlobal("colorgrid", colorgrid)
            setGlobal("pcseries", pcseries)
#showpcseriesgrid("pc series grid (before):")
cat("pc series grid (before):", "\n")
show(pcseriesgrid)
flush.console()
            pcseriesgrid <- updateseriesgrid(pcseriesgrid,
                                             pcseries[[currpc]]$High,
                                             pcseries[[currpc]]$Low,
                                             currpc)
cat("updating pcseriesgrid", "\n")
            setGlobal("pcseriesgrid", pcseriesgrid)
show(ls(envir=sdenv()))
showpcseriesgrid("pc series grid (after):")
        } else {
            ## Not allowed
            tkmessageBox(icon="error",
                         message=paste("Must have all positive controls",
                                       "in one row or in one column."),
                         parent=getGlobal("toplevel"),
                         title="Invalid Selection")
        }
    }
    #debug(markLastPC)


    ## Begin processing
    userdata <- get("userdata", envir=widget$env)
    i <- userdata$row
    j <- userdata$col

    who <- encodeButtonLabel(i, j)
    .appDebugStr(paste("who:", as.character(who)))

    switch(state <- getGlobal("state"),
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

    sr <- as.numeric(tclvalue(getGlobal("nsubrow")))
    sc <- as.numeric(tclvalue(getGlobal("nsubcol")))
    altgrid <- matrix(color, ncol=sc, nrow=sr)
    setGlobal("altgrid", altgrid)
}


##-----------------------------------------------------------------------------
## Fills the current grid to make it ready for color swapping by marking.
fillGrid <- function() {
    .sdEntryStr("fillGrid")

    sr <- as.numeric(tclvalue(getGlobal("nsubrow")))
    sc <- as.numeric(tclvalue(getGlobal("nsubcol")))
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
    setGlobal("colorgrid", grid)
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
    .sdEntryStr("displayInvalidValueDialog")
    stopifnot(is.character(fieldname) && length(fieldname) == 1)
    stopifnot(is.character(value) && length(value) == 1)
    stopifnot(is.numeric(minvalue) && length(minvalue) == 1)
    stopifnot(is.numeric(maxvalue) && length(maxvalue) == 1)

    errmsg <- sprintf("%s (%s) must be integer between %d and %d.",
                      fieldname,
                      value,
                      minvalue,
                      maxvalue)
    tkmessageBox(icon="error",
                 message=errmsg,
                 parent=getGlobal("toplevel"),
                 title="Invalid Value")
}


##-----------------------------------------------------------------------------
## Verify grid dimensions are numeric and within acceptable range.
verifyGridDimensions <- function() {
    .sdEntryStr("verifyGridDimensions")

    ## Maximum grid dimensions
    mrmax <- 1000
    mcmax <- 1000
    srmax <- 30     # This better be overkill ...
    scmax <- 30     # This better be overkill ...

    ## Verify user input
    mr.value <- tclvalue(getGlobal("nmainrow"))
    mr <- try(as.numeric(mr.value))
    if (badinteger(mr) || mr < 1 || mr > mrmax) {
        displayInvalidValueDialog("Main Row", mr.value, 1, mrmax)
        return(-1)
    }

    mc.value <- tclvalue(getGlobal("nmaincol"))
    mc <- try(as.numeric(mc.value))
    if (badinteger(mc) || mc < 1 || mc > mcmax) {
        displayInvalidValueDialog("Main Col", mc.value, 1, mcmax)
        return(-1)
    }

    sr.value <- tclvalue(getGlobal("nsubrow"))
    sr <- try(as.numeric(sr.value))
    if (badinteger(sr) || sr < 1 || sr > srmax) {
        displayInvalidValueDialog("Sub Row", sr.value, 1, srmax)
        return(-1)
    }

    sc.value <- tclvalue(getGlobal("nsubcol"))
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
    .sdEntryStr("createSubgrid")

    ## Grab subgrid dimensions
    sr <- as.numeric(tclvalue(getGlobal("nsubrow")))
    sc <- as.numeric(tclvalue(getGlobal("nsubcol")))

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
    right.frame <- getGlobal("right.frame")
    ismapped <- as.logical(tkwinfo.ismapped(right.frame))
    if (!ismapped) {

        ##---------------------------------------------------------------------
        ## Update window manager's geometry to ensure display of right frame.
        wmGeometry <- function() {
            .sdEntryStr("wmGeometry")

            tclupdate("idletasks")
            toplevel <- getGlobal("toplevel")
            #reqwidth <- tclvalue(tkwinfo.reqwidth(right.frame))
            #cat("right.frame reqwidth:", reqwidth, "\n")
            newwidth <- as.integer(tclvalue(tkwinfo.reqwidth(toplevel)))
            height <- as.integer(tclvalue(tkwinfo.height(toplevel)))
            geometry <- sprintf("%dx%d",
                                newwidth,
                                height)
            #cat("new proposed geometry:", geometry, "\n")
            #flush.console()
            tkwm.geometry(toplevel, geometry)
        }


        ##---------------------------------------------------------------------
        ## Update window manager's minimum width of toplevel such that some
        ## portion of right frame is always visible.
        wmMinSizeResetWidth <- function() {
            .sdEntryStr("wmMinSizeResetWidth")

            tclupdate("idletasks")
            toplevel <- getGlobal("toplevel")
            left.frame <- getGlobal("left.frame")
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

    setGlobal("pcseriesgrid", matrix(0.0, ncol=sc, nrow=sr))
    setGlobal("state", "gridmade")

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
    .sdEntryStr("createRow")
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
    .sdEntryStr("negCtrlStepCB")

    state <- getGlobal("state")
    if (state == "initial") {
        ## Grid not yet made
        tkmessageBox(icon="error",
                     message="You have to create the grid first!",
                     parent=getGlobal("toplevel"),
                     title="No Grid Found")
        return(-1)
    } else if (state != "gridmade") {
        ## This is not supposed to happen
        errmsg <- sprintf("Unexpected state (%s) in %s.",
                          state,
                          "negCtrlStepCB")
        tkmessageBox(icon="error",
                     message=errmsg,
                     parent=getGlobal("toplevel"),
                     title="Internal Error")
        return(-1)
    }

    ## Remove existing components of left frame
    left.frame <- getGlobal("left.frame")
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
        .sdEntryStr("markBlankCB")
        stopifnot(is.tkwin(widget))

        setGlobal("state", "blank")
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
        .sdEntryStr("markBufferCB")
        stopifnot(is.tkwin(widget))

        setGlobal("state", "buffer")
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
        .sdEntryStr("markNegCtrlCB")
        stopifnot(is.tkwin(widget))

        setGlobal("state", "nc")
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
        question <- "Are positive controls in a dilution series?"
        response <- tclVar(tkmessageBox(default="no",
                                        icon="question",
                                        message=question,
                                        parent=getGlobal("toplevel"),
                                        title="Decide",
                                        type="yesno"))
        cat("response =", tclvalue(response), "\n")
        if (tclvalue(response) == "no") {
            posCtrlStepCB()
        } else {
            setGlobal("state", "addpc")
            posCtrlSeriesSetupStepCB()
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
    setGlobal("state", "ready2mark")
}


##-----------------------------------------------------------------------------
## This is pure control-flow, since the next steps depend on whether we have
## one level of positive control, or many.
numPosCtrlLevelsStepCB.unused <- function() {
    .sdEntryStr("numPosCtrlLevelsStepCB")

    if (!(getGlobal("state") %in% c("ready2mark", "blank", "buffer", "nc"))) {
        errmsg <- sprintf("Unexpected state (%s) in %s.",
                          getGlobal("state"),
                          "numPosCtrlLevelsStepCB")
        tkmessageBox(icon="error",
                     message=errmsg,
                     parent=getGlobal("toplevel"),
                     title="Internal Error")
    }

    ## Need this to prevent button presses from changing things right now...
    setGlobal("state", "waiting")

    ## Remove existing components of left frame
    left.frame <- getGlobal("left.frame")
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
            setGlobal("state", "addpc")
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
    .sdEntryStr("posCtrlSeriesSetupStepCB")

    if (!(getGlobal("state") %in% c("addpc", "lastpc"))) {
        errmsg <- sprintf("Unexpected state (%s) in %s.",
                          getGlobal("state"),
                          "posCtrlSeriesSetupStepCB")
        tkmessageBox(icon="error",
                     message=errmsg,
                     parent=getGlobal("toplevel"),
                     title="Internal Error")
    }

    ## Need this to prevent button presses from changing things right now...
    setGlobal("state", "waiting")

    ## Remove existing components of left frame
    left.frame <- getGlobal("left.frame")
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

    csr.label <- tklabel(csr.frame,
                         text="Control StepRate:")
    csr.entry <- tkentry(csr.frame,
                         textvariable=getGlobal("control.steprate"),
                         width="6")
    tkbind(csr.entry, "<FocusIn>",  function() entry_focusgained(csr.entry))
    tkbind(csr.entry, "<FocusOut>", function() entry_focuslost(csr.entry))
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
            .sdEntryStr("incrNumPC")

            evalq(currpc <- currpc + 1, envir=sdenv())

            currpc <- getGlobal("currpc")
            pcseries <- getGlobal("pcseries")
            pcseries[[currpc]] <- list(High="", Low="", StepRate=csr)

            setGlobal("pcseries", pcseries)
        }

        csr.value <- tclvalue(getGlobal("control.steprate"))
        csr <- try(as.numeric(csr.value))
        csrmax <- 5 # Maximum control steprate
        if (badinteger(csr) || csr < 1 || csr > csrmax) {
            displayInvalidValueDialog("Control step rate", csr.value, 1, csrmax)
            return(-1)
        }

        incrNumPC(csr)
        if (getGlobal("currpc") > 1) {
            tkmessageBox(icon="warning",
                         message=paste("Ensure positive control series",
                                       "do not overlap."),
                         parent=getGlobal("toplevel"),
                         title="Danger, Will Robinson!")
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
    .sdEntryStr("posCtrlStepCB")

    ## Remove existing components of left frame
    left.frame <- getGlobal("left.frame")
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
        .sdEntryStr("markPosCtrlCB")
        stopifnot(is.tkwin(widget))

        setGlobal("state", "pc")
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
    .sdEntryStr("posCtrlSeriesStepCB")

    ## Remove existing components of left frame
    left.frame <- getGlobal("left.frame")
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
        .sdEntryStr("markPosCtrlHighCB")
        stopifnot(is.tkwin(widget))

        setGlobal("state", "firstpc")
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
        .sdEntryStr("markPosCtrlLowCB")
        stopifnot(is.tkwin(widget))

        currpc <- getGlobal("currpc")
        pcseries <- getGlobal("pcseries")

        if (nzchar(pcseries[[currpc]]$High)) {
            cat("**posctrllo**", "\n")
            setGlobal("state", "lastpc")
            labelstring <- "Marking least intense PC..."
            tkconfigure(instruct.label,
                        text=labelstring)
            updateButtonVisuals(widget)
            fillGrid()
            makeAltGrid(spottype2background("PosCtrl", "Low"))
        } else {
            tkmessageBox(icon="error",
                         message=paste("You have to mark the most",
                                       "intense positive control first!"),
                         parent=getGlobal("toplevel"),
                         title="Incomplete Series")
        }
    }


    ##-------------------------------------------------------------------------
    ## Verify series is completely specified before adding any more.
    proceed <- function() {
        .appDebugStr("**checking**")
        currpc <- getGlobal("currpc")
        pcseries <- getGlobal("pcseries")

        if (nzchar(pcseries[[currpc]]$High) &&
            nzchar(pcseries[[currpc]]$Low)) {
            posCtrlSeriesSetupStepCB()
        } else {
            tkmessageBox(icon="error",
                         message=paste("You have to finish marking",
                                       "the series first!"),
                         parent=getGlobal("toplevel"),
                         title="Incomplete Series")
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
    .sdEntryStr("posCtrlSeriesHighStepCB")

    ## Remove existing components of left frame
    left.frame <- getGlobal("left.frame")
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
        .sdEntryStr("markPosCtrlHighCB")
        stopifnot(is.tkwin(widget))

        setGlobal("state", "firstpc")
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

        currpc <- getGlobal("currpc")
        pcseries <- getGlobal("pcseries")

        if (nzchar(pcseries[[currpc]]$High)) {
            setGlobal("state", "waiting")
            posCtrlSeriesLowStepCB()
        } else {
            tkmessageBox(icon="error",
                         message=paste("You have to mark the most",
                                       "intense positive control first!"),
                         parent=getGlobal("toplevel"),
                         title="Incomplete Series")
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
    .sdEntryStr("posCtrlSeriesLowStepCB")

    ## Remove existing components of left frame
    left.frame <- getGlobal("left.frame")
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
        .sdEntryStr("markPosCtrlLowCB")
        stopifnot(is.tkwin(widget))

        setGlobal("state", "lastpc")
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
        currpc <- getGlobal("currpc")
        pcseries <- getGlobal("pcseries")

        if (nzchar(pcseries[[currpc]]$High) &&
            nzchar(pcseries[[currpc]]$Low)) {
            posCtrlSeriesSetupStepCB()
        } else {
            tkmessageBox(icon="error",
                         message=paste("You have to finish marking",
                                       "the series first!"),
                         parent=getGlobal("toplevel"),
                         title="Incomplete Series")
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
    .sdEntryStr("assembleStepCB")

    ## Remove existing components of left frame
    left.frame <- getGlobal("left.frame")
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

    dsr.frame <- tkframe(command.area)

    dsr.label <- tklabel(dsr.frame,
                         text="Dilution StepRate:")
    dsr.entry <- tkentry(dsr.frame,
                         textvariable=getGlobal("dilution.steprate"),
                         width="6")
    cat("dsr.entry id = ", dsr.entry$ID, "\n")
    tkbind(dsr.entry, "<FocusIn>",  function() entry_focusgained(dsr.entry))
    tkbind(dsr.entry, "<FocusOut>", function() entry_focuslost(dsr.entry))
    tkfocus(dsr.entry)

    tkgrid(dsr.label,
           dsr.entry)
    tkgrid.configure(dsr.label,
                     sticky="e") 
    tkgrid.configure(dsr.entry,
                     sticky="w") 
    tkpack(dsr.frame,
           pady="3m")


    ##-------------------------------------------------------------------------
    ## Assembles subgrid if user input valid.
    doAssembleStepCB <- function() {

        ## Make sure steprate is set and in the correct range
        dsr.value <- tclvalue(getGlobal("dilution.steprate"))
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
        subgrid.df <- Try(assembleSubgrid(dsr))
        if (inherits(subgrid.df, "try-error")) {
            ## Assembly failed
            warnings()
            return(-1)
        }

        assign("subgrid.df", subgrid.df, envir=sdenv())
        colorSamplesByDilution(subgrid.df)
        tkconfigure(next.button,
                    state="normal")
        setGlobal("state", "assembled")

        return(0)
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
    .sdEntryStr("finalStepCB")

    ## Remove existing components of left frame
    left.frame <- getGlobal("left.frame")
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

    ## Create action area
    view.button <- tkbutton(action.area,
                            command=function() {
                                viewSubgrid(getGlobal("subgrid.df"))
                            },
                            text="View Subgrid")
    save.button <- tkbutton(action.area,
                            command=function() {
                                saveGrid(getGlobal("subgrid.df"))
                            },
                            text="Save Grid As...")
    tkpack(view.button,
           save.button,
           pady="1m")

    setGlobal("state", "done")
}


##-----------------------------------------------------------------------------
## Modify background color of subgrid widgets representing samples to be based
## on dilution intensity.
colorSamplesByDilution <- function(df) {
    stopifnot(is.data.frame(df))

    ##-------------------------------------------------------------------------
    ## Returns the encoded group value from the SampleAlias for samples.
    groupFromAlias <- function(alias) {
        stopifnot(is.character(alias) && length(alias) == 1)
        return(as.integer(unlist(strsplit(alias, "-", fixed=TRUE))[3]))
    }


    ## Update subgrid button widget backgrounds to appropriate dilution colors
    subgrid.frame <- scrollframe_interior()
    for (subrow in seq_len(max(df$Sub.Row))) {
        x.sample <- which(with(df, Sub.Row==subrow & SpotType=="Sample"))
        if (length(x.sample) > 0) {
            lastAliasInRow <- df$SampleAlias[x.sample[length(x.sample)]]
            ngroups <- groupFromAlias(lastAliasInRow)
            for (group in seq_len(ngroups)) {
                sample.re <- sprintf("^Sample-%d-%d-[[:digit:]]*$",
                                     subrow,
                                     group)
                aliases <- grep(sample.re,
                                df$SampleAlias[x.sample],
                                value=TRUE)
                colors <- generateSampleDilutionColors(length(aliases))
                x.which <- which(df$SampleAlias[x.sample] %in% aliases)
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
    if (exists(varname, envir=sdenv())) {
        background <- getGlobal(varname)
    } else {
        ## Grab value from option database instead
        rsrcClass <- switch(spottype,
                            PosCtrl=paste(spottype, pos, "Background", sep=""),
                            paste(spottype, "Background", sep=""))

        background <- tclvalue(optiondb_get(rsrcClass))
        setGlobal(varname, background)
    }

    return(background)
}


##-----------------------------------------------------------------------------
## In order to save, we have to convert the displayed colors (from colorgrid)
## into something meaningful. Positive controls in series will be handled by
## indexing since color alone is inadequate to determine this.
spottype <- function(x) {
    .sdEntryStr("spottype")
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
        pcseriesgrid <- getGlobal("pcseriesgrid")
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

    currpc <- getGlobal("currpc")
    if (currpc != 0) {

        pcseries <- getGlobal("pcseries")
        pcseriesgrid <- getGlobal("pcseriesgrid")

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
                    df$SampleAlias[idx] <- sprintf("PosCtrl-%d-%d",
                                                   pc.series,
                                                   pc.pos)
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
## Adds dilution values for samples to data frame.
addDilutionLevels <- function(df, step) {
    stopifnot(is.data.frame(df))
    stopifnot(is.numeric(step) && length(step) == 1)

    saved.warn <- options(warn=1)
    on.exit(options(warn=as.integer(saved.warn)))

    ## For each row, col pair do:
    ## - If SpotType==Sample, Dilution=intensity and divide intensity by step.
    ## - If SpotType!=Sample, reset intensity (to maximum).

    for (subrow in seq_len(max(df$Sub.Row))) {
        intensity <- 100
        samp.series <- as.integer(1)
        samp.pos <- as.integer(1)
        used <- FALSE
        for (subcol in seq_len(max(df$Sub.Col))) {
            idx <- which(with(df, Sub.Row == subrow & Sub.Col == subcol))

            if (df$SpotType[idx] == "Sample") {
                df$SampleAlias[idx] <- sprintf("Sample-%d-%d-%d",
                                               subrow,      
                                               samp.series,       
                                               samp.pos)
                df$Dilution[idx] <- intensity
                samp.pos <- as.integer(samp.pos + 1)
                intensity <- intensity / step
                used <- TRUE
            } else {
                if (used) {
                    samp.series <- as.integer(samp.series + 1)
                    samp.pos <- as.integer(1)
                    intensity <- 100    # reset
                    used <- FALSE
                }
            }
        }
    }

    return(df)
}


##-----------------------------------------------------------------------------
## Create subgrid dataframe from user input.
assembleSubgrid <- function(dsr) {
    .sdEntryStr("assembleSubgrid")
    stopifnot(is.numeric(dsr) && length(dsr) == 1)

    colorgrid <- getGlobal("colorgrid")
    nr <- nrow(colorgrid)
    nc <- ncol(colorgrid)

    subgrid.df <- data.frame(Sub.Row=as.integer(rep(1:nr, nc)),
                             Sub.Col=as.integer(rep(1:nc, each=nr)),
                             SampleAlias=I(""),
                             SpotType=as.factor(spottype(colorgrid)),
                             ControlLevel=as.numeric(NA),
                             Dilution=as.numeric(NA))
    stopifnot(nlevels(subgrid.df$SpotType) <= 5)

    ## Provide initial values for SampleAlias column
    subgrid.df$SampleAlias <- levels(subgrid.df$SpotType)[subgrid.df$SpotType]

    ## :NOTE: following methods possibly update SampleAlias column
    subgrid.df <- addControlLevels(subgrid.df)
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
    .sdEntryStr("viewSubgrid")
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
    stopifnot(is.data.frame(subgrid.df) && ncol(subgrid.df) == 5)

    ## Grab grid dimensions
    nmainrow <- as.numeric(tclvalue(getGlobal("nmainrow")))
    nmaincol <- as.numeric(tclvalue(getGlobal("nmaincol")))

    ## Duplicate merged grid for each mainrow, maincol...
    grid.df <- NULL
    for (mr in seq_len(nmainrow)) {
        for (mc in seq_len(nmaincol)) {
            tmp.df <- cbind(Main.Row=as.integer(mr),
                            Main.Col=as.integer(mc),
                            subgrid.df)

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
    .sdEntryStr("writeGridToFile")
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
        evalq(dirty <- FALSE, envir=sdenv())
        return(0)
    }
}


##-----------------------------------------------------------------------------
## Save the grid design.
saveGrid <- function(subgrid.df) {
    .sdEntryStr("saveGrid")
    stopifnot(is.data.frame(subgrid.df))

    pathname <- tclvalue(tkgetSaveFile(defaultextension=".tsv",
                                       initialfile="slidedesign.tsv",
                                       parent=getGlobal("toplevel"),
                                       title="Save Grid"))
    if (!nzchar(pathname)) {
        ## User canceled the save dialog
        return(-1)
    }

    return(writeGridToFile(createGrid(subgrid.df), pathname))
}


##-----------------------------------------------------------------------------
## Save global variables in private environment.
initGlobals <- function(glist) {
    .sdEntryStr("initGlobals")
    stopifnot(is.list(glist))

    sapply(seq_along(glist),
           function(i, ll) {
               setGlobal(names(ll)[i], ll[[i]])
           },
           glist)

    if (.appDebugEnabled()) {
        show(ls(envir=sdenv()))
    }
}


##-----------------------------------------------------------------------------
## Adds entry into Tcl options database.
optiondb_add <- function(pattern,
                         value) {
    #.sdEntryStr("optiondb_add")
    stopifnot(is.character(pattern) && length(pattern) == 1)
    stopifnot(!missing(value))

    #cat("option", "add", pattern, value, "startupFile", "\n")
    tcl("option", "add", pattern, value, "startupFile")
}


##-----------------------------------------------------------------------------
## Fetches value from Tcl options database.
optiondb_get <- function(rsrcClass) {
    #.sdEntryStr("optiondb_get")
    stopifnot(is.character(rsrcClass) && length(rsrcClass) == 1)

    rsrcName <- initLowercase(rsrcClass)
    #cat("option", "get", ".", rsrcName, rsrcClass, "\n")
    tcl("option", "get", ".", rsrcName, rsrcClass)
}


##-----------------------------------------------------------------------------
## Initialize the Tk option database with application defaults.
initOptions <- function(olist) {
    .sdEntryStr("initOptions")
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
                         relief="groove")
    tkcreate(vport, "window", "0 0", anchor="nw", window=int.frame$ID)
    tkbind(int.frame, "<Configure>", function() scrollframe_resize(int.frame))

    ## Save this so items can be put in it
    setGlobal("subgrid.frame", int.frame)

    return(frame)
}


##-----------------------------------------------------------------------------
## Resizes scrolled frame area when screen dimensions change.
scrollframe_resize <- function(iframe) {
    stopifnot(tclvalue(tkwinfo.class(iframe)) == "Frame")

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
    return(getGlobal("subgrid.frame"))
}


##-----------------------------------------------------------------------------
## Invoked upon gaining focus on bound text entry widgets.
entry_focusgained <- function(widget) {
    #.sdEntryStr("entry_focusgained")
    stopifnot(is.tkwin(widget))
    stopifnot(tclvalue(tkwinfo.class(widget)) == "Entry")

    #cat("widget id =", widget$ID, "\n")
    tkselection.range(widget, 0, "end")
    #cat("exiting entry_focusgained", "\n")
}


##-----------------------------------------------------------------------------
## Invoked upon losing focus on bound text entry widgets.
entry_focuslost <- function(widget) {
    #.sdEntryStr("entry_focuslost")
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
    .sdEntryStr("buildMenus")
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

    ##---------------------------------------------------------------------
    ## Add FILE menu items.
    buildFileMenu <- function(file.menu) {
        stopifnot(tclvalue(tkwinfo.class(file.menu)) == "Menu")

        ## Add menu items
        tkadd(file.menu,
              "command",
              label="Quit",
              command=appExit)
    }


    ##---------------------------------------------------------------------
    ## Add HELP menu items.
    buildHelpMenu <- function(help.menu) {
        stopifnot(tclvalue(tkwinfo.class(help.menu)) == "Menu")

        ##-----------------------------------------------------------------
        ## Display overview dialog.
        overviewCB <- function() {
            overview <- paste("Tcl/Tk application for recording",
                              "the layout of positive and negative",
                              "controls of a slide design.")
            tkmessageBox(icon="info",
                         message=overview,
                         parent=getGlobal("toplevel"),
                         title="Overview")
        }


        ##-----------------------------------------------------------------
        ## Display about dialog.
        aboutCB <- function() {

            ##-------------------------------------------------------------
            ## Returns application version string.
            getAppVersionLabelstring <- function(default="NA") {
                stopifnot(is.character(default) && length(default) == 1)

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


            about <- paste(app.name <- "SlideDesignerGUI",
                           getAppVersionLabelstring(),
                           getTclTkVersionLabelstring(),
                           sep="\n")
            tkmessageBox(icon="info",
                         message=about,
                         parent=getGlobal("toplevel"),
                         title="About")
        }


        ## Add menu items
        tkadd(help.menu,
              "command",
              label="Overview",
              command=overviewCB)
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
    .sdEntryStr("appExit")

    if (dirty <- getGlobal("dirty")) {
        ## Prompt user concerning possible data loss
        question <- "Discard subgrid modifications?"
        response <- tclVar(tkmessageBox(icon="question",
                                        message=question,
                                        parent=getGlobal("toplevel"),
                                        title="Really Quit?",
                                        type="yesno"))
        cat("response =", tclvalue(response), "\n")
        if (tclvalue(response) == "no") {
            cat("quit canceled by user", "\n")
            flush.console()
            return()
        }
    }

    ##-------------------------------------------------------------------------
    ## Terminate the application.
    terminate <- function() {
        .sdEntryStr("terminate")

        ## Unmap toplevel
        tkwm.withdraw(toplevel <- getGlobal("toplevel"))

        tkfont.delete(bannerFont <- "banner")
        tclupdate("idletasks")
        tkdestroy(toplevel)
    }


    ## Destroy toplevel indirectly to workaround problem with X11
    tclafter.idle(terminate)
}


##-----------------------------------------------------------------------------
## This is the only user-visible function in the file. You call this
## function to start the application.
slideDesignerGUI <- function() {
    bannerFont <- "banner"

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
    })

    ## Create named font for later use
    if (!(bannerFont %in% unlist(strsplit(tclvalue(tkfont.names()), " ")))) {
        .appDebugStr(sprintf("creating %s font", sQuote(bannerFont)))
        tkfont.create(bannerFont,
                      family="helvetica",
                      size=18,
                      weight="bold")
    } else {
        .appDebugStr(sprintf("%s font already exists", sQuote(bannerFont)))
    }

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
                     dilution.steprate=tclVar("2"),
                     dirty=FALSE,
                     labelstring="Press one of the buttons below.",
                     left.frame=left.frame,
                     nmainrow=tclVar("4"),
                     nmaincol=tclVar("12"),
                     nsubrow=tclVar("8"),
                     nsubcol=tclVar("8"),
                     pcseries=list(),
                     pcseriesgrid=matrix(as.numeric(0), nrow=1, ncol=1),
                     right.frame=right.frame,
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
    mr.label <- tklabel(dim.frame,
                        text="Main Row:")
    mr.entry <- tkentry(dim.frame,
                        textvariable=getGlobal("nmainrow"),
                        width="6")
    tkbind(mr.entry, "<FocusIn>",  function() entry_focusgained(mr.entry))
    tkbind(mr.entry, "<FocusOut>", function() entry_focuslost(mr.entry))
    tkfocus(mr.entry)

    mc.label <- tklabel(dim.frame,
                        text="Main Col:")
    mc.entry <- tkentry(dim.frame,
                        textvariable=getGlobal("nmaincol"),
                        width="6")
    tkbind(mc.entry, "<FocusIn>",  function() entry_focusgained(mc.entry))
    tkbind(mc.entry, "<FocusOut>", function() entry_focuslost(mc.entry))

    sr.label <- tklabel(dim.frame,
                        text="Sub Row:")
    sr.entry <- tkentry(dim.frame,
                        textvariable=getGlobal("nsubrow"),
                        width="6")
    tkbind(sr.entry, "<FocusIn>",  function() entry_focusgained(sr.entry))
    tkbind(sr.entry, "<FocusOut>", function() entry_focuslost(sr.entry))

    sc.label <- tklabel(dim.frame,
                        text="Sub Col:")
    sc.entry <- tkentry(dim.frame,
                        textvariable=getGlobal("nsubcol"),
                        width="6")
    tkbind(sc.entry, "<FocusIn>",  function() entry_focusgained(sc.entry))
    tkbind(sc.entry, "<FocusOut>", function() entry_focuslost(sc.entry))

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
        .sdEntryStr("createSubgridCB")

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

    cat("windowing system:", tclvalue(tktk.windowingsystem()), "\n")

    ## Create menus
    menubar <- tkmenu(toplevel)
    buildMenus(menubar)
    tkconfigure(toplevel,
                menu=menubar)

    ##-------------------------------------------------------------------------
    ## Set window manager's minimum width and height of toplevel such that
    ## requested size of left frame and menubar are always visible.
    wmMinSize <- function() {
        .sdEntryStr("wmMinSize")

        tclupdate("idletasks")
        minwidth <- as.integer(tclvalue(tkwinfo.reqwidth(toplevel)))
        ## Add height of next button to compensate for menu size
        minheight <- as.integer(tclvalue(tkwinfo.reqheight(toplevel))) +
                     as.integer(tclvalue(tkwinfo.reqheight(next.button)))
        tkwm.minsize(toplevel, minwidth, minheight)
    }


    ## Process once mapped so window sizes are known...
    tclafter.idle(wmMinSize)

    ## Map window manager's close button to exit function
    tkwm.protocol(toplevel, "WM_DELETE_WINDOW", appExit)

    ## Give R some time to process its event loop
    tclafter.idle(idleTask)

    ## Display application window
    tkwait.window(toplevel)
}

sdui <- slideDesignerGUI

