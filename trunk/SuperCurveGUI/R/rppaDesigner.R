require(tcltk)

##################################################################
# so we can track which functions are called by various events

dbg <- tclVar(0)

# this function simply does not work. Reading tclvalue(dbg) always
# returns an NA, even thnough the value was explcitly set.
.rdDebug <- function() {
  x <- tclvalue(dbg)
  x <- as.numeric(x)
  if (is.na(x)) {
    x <- -1
  }
  x
}

hues <- c(0, 120, 240, 60, 180) # useful color choices

# How do we do this without writing crap into the global environment?
rdEnviron <- .GlobalEnv

##################################################################
# This function destroys all children of the specified frame, but leaves
# the frame itself intact so it can be refilled. It would be nice if
# some such function already existed in Tcl/Tk.
cleanup <- function(frame) {
  if(.rdDebug() > 1) cat('cleanup\n')
  nsub <- get("num.subwin", env=frame$env)
  while (nsub > 0) {
    child <- paste(frame$ID, nsub, sep='.')
    tcl("destroy", child)
    nsub <- nsub-1
  }
  assign("num.subwin", 0, env=frame$env)
}

##################################################################
# This block of functions handles the grid display, which involves
# changing the displayed color of the buttons.

# variables to hold the state of the subgrid
assign('mygrid', matrix(NA, 1, 1), rdEnviron)  # current colors
assign('altgrid', matrix(NA, 1, 1), rdEnviron) # alternative colors

# Used to mark/paint the buttons in the right frame. Works for marking
# blanks, buffer, negative controls, single-level positive controls, or
# dilution series of positive controls..
#
# This may be the most complicated part of the code, since it needs to
# know how to change colors depending on the current state. The main switch
# has three branches:
#   firstpc                  (start marking dilution series)
#   lastpc                   (stop marking dilution series)
#   blank, buffer, nc, or pc (marking  a single level)
pressButton <- function() {
  swapGrids <- function(i, j) {
    g1 <- mygrid
    g2 <- altgrid
    current <- g1[i,j]
    g1[i,j] <- g2[i,j]
    g2[i,j] <- current
    assign('mygrid', g1, rdEnviron)
    assign('altgrid', g2, rdEnviron)
    # for some reason, you can apparently only update the R variables
    # 'altgrid' and 'mygrid' by an explicit asignment. Writing into the
    # matrices directly does not work. I think it is a timing issue....
    id <- paste(right.frame$ID, i, j, sep='.')
    tkconfigure(id, background=mygrid[i,j])
  }

  who <- tclvalue(pressed)
  button <- strsplit(who, '-')[[1]]
  i <- as.numeric(button[[2]])
  j <- as.numeric(button[[4]])
  if (tclvalue(state) == "firstpc") {       # start dilution series
    # only allowed to mark one of these
    # :KRC: next version should allow arbitrarily many,
    # but require matching low intensity spots
    high <- as.character(tclvalue(highest))
    if (.rdDebug() > 0) cat(paste("Highest:", high, "\n"))
    if (high == '') {         # nothing marked yet
      swapGrids(i, j)
      tclvalue(highest) <- who
    } else if (high == who) { # remove an existing mark
      swapGrids(i,j)
      tclvalue(highest) <- ''
    } else {                  # mark new, so unmark old
      if (.rdDebug() > 0) cat("old for new\n")
      swapGrids(i,j)
      button <- strsplit(high, '-')[[1]]
      i <- as.numeric(button[[2]])
      j <- as.numeric(button[[4]])
      swapGrids(i,j)
      tclvalue(highest) <- who
    }
  } else if (tclvalue(state) == "lastpc") { # finish dilution series
    low <- tclvalue(lowest)
    if (.rdDebug() > 0) cat(paste("Lowest:", low, "\n"))
    if (low != '') {
      tkmessageBox(message="Changing your mind here is not (yet) allowed")
      return(-1)
    }
    tclvalue(lowest) <- who
    high <- as.character(tclvalue(highest))
    temp <- strsplit(high, '-')[[1]]
    hi <- as.numeric(temp[[2]])
    hj <- as.numeric(temp[[4]])
    if (i == hi) { # all in one row
      if (.rdDebug() > 0) cat("  one row\n")
      n <- 1 + abs(j-hj)
      colors <- hcl(300, 85, seq(30, 100, length=n))
      g <- mygrid
      for (p in 1:n) {
        sign <- abs(hj-j)/(hj-j)
        id <- paste(right.frame$ID, i, y <- hj - sign*(p-1), sep='.')
        if (.rdDebug() > 0) cat(paste("sign =", sign, ";  id =", id, "\n"))
        tkconfigure(id, background=colors[p])
        g[i,y] <- colors[p]
      }
      assign('mygrid', g, rdEnviron)
    } else if (j == hj) { # all in one column
      if (.rdDebug() > 0) cat("  one col\n")
      n <- 1 + abs(i-hi)
      colors <- hcl(300, 85, seq(30, 100, length=n))
      g <- mygrid
      for (p in 1:n) {
        sign <- abs(hi-i)/(hi-i)
        id <- paste(right.frame$ID, x <- hi - sign*(p-1), j, sep='.')
        if (.rdDebug() > 0) cat(paste("sign =", sign, ";  id =", id, "\n"))
        tkconfigure(id, background=colors[p])
        g[x, j] <- colors[p]
      }
      assign('mygrid', g, rdEnviron)
    } else { # we do not allow this
      tkmessageBox(message=paste("Must have all positive controls in",
                     " one row or in one column."))
    }
  } else if (tclvalue(state) %in% c('blank', 'buffer', 'nc', 'pc')) {
    swapGrids(i, j)
  }
  # otherwise, we simply ignore the press
}
  
# sets up the 'alternative color" grid.
makeAltGrid <- function(color) {
  nr <- as.numeric(tclvalue("nr"))
  nc <- as.numeric(tclvalue("nc"))
  grid <- matrix(color, ncol=nc, nrow=nr)
  assign('altgrid', grid, rdEnviron) # why do we need to do it this way?
}

# fills the current grid to make it ready for color swapping by marking.
fillGrid <- function() {
  nr <- as.numeric(tclvalue("nr"))
  nc <- as.numeric(tclvalue("nc"))
  grid <- matrix(NA, ncol=nc, nrow=nr)
  for (i in 1:nr) {
    for (j in 1:nc) {
      loc <- paste(right.frame$ID, i, j, sep='.')
      grid[i,j] <- as.character(tkcget(loc, "-background"))
    }
  }
  assign('mygrid', grid, rdEnviron) # again, why?
}

# :KRC: Do we need both tcl variables?
# swaps between previuos color and 'blank' color (white)
markBlank <- function() {
  tclvalue(state) <- "blank"
  tclvalue(labeler) <- "Marking blank spots..."
  fillGrid()
  makeAltGrid("#FFFFFF")
}

# swaps between previous color and 'buffer' color (light gray)
markBuffer <- function() {
  tclvalue(state) <- "buffer"
  tclvalue(labeler) <- "Marking buffer spots..."
  fillGrid()
  makeAltGrid("#CCCCCC")
}

# swaps between previous color and 'negative control' color (medium gray)
markNC <- function() {
  tclvalue(state) <- "nc"
  tclvalue(labeler) <- "Marking negative controls..." 
  fillGrid()
  makeAltGrid("#AAAAAA")
}

# swaps between previous color and 'positive control' color (dark magenta)
markPC <- function() {
  tclvalue(state) <- "pc"
  tclvalue(labeler) <- "Marking positive controls..."
  fillGrid()
  makeAltGrid("#890099")
}

# swaps between previous color and 'positive control' color (dark magenta)
markHighPC <- function() {
  tclvalue(state) <- "firstpc"
  tclvalue(labeler) <- "Marking most intense PC..."
  fillGrid()
  makeAltGrid("#890099")
}

# swaps between previous color and 'positive control' color (magenta)
# also has other indirect effects implemented through 'pressButton'.
markLowPC <- function() {
  tclvalue(state) <- "lastpc"
  tclvalue(labeler) <- "Marking least intense PC..."
  fillGrid()
  makeAltGrid("#FFDEFF")
}

##################################################################
# This function paints the subgrid layout in the right-hand frame. It is
# invoked when the user presses the "Create Subgrid" button. Note that
# the overall design of the application has a single toplevel window
# divided into 'left.frame' and 'right.frame' parts. All logical flow control
# in the application is handled in the left frame. Also, the right frame
# starts out invisible until the user specifies the dimensions.
createSubgrid <- function() {
  if(.rdDebug() > 0) cat('createSubgrid\n')
  mod <- function(i, n) {
    # we modify the usual '%%' operator so it can be used to index into
    # arrays, which start at position 1 in R.
    x <- i%%n
    x[x==0] <- n
    x
  }
  # make sure dimensions are set and in the correct range
  nr <- try(as.numeric(tclvalue("nr")))
  nc <- try(as.numeric(tclvalue("nc")))
  nmax <- 30 # this better be overkill ...
  if (inherits(nr, "try-error") || inherits(nc, "try-error")) {
    tkmessageBox(message=paste("Number of rows (", nr,
                   ") and columns (", nc, ") must",
                   " be between 1 and ", nmax, '.', sep=''))    
    return(-1)
  }
  if (is.na(nr) || is.na(nc) || nr < 0 || nc < 0 || nr > nmax || nc > nmax) {
    tkmessageBox(message=paste("Number of rows (", nr,
                   ") and columns (", nc, ") must",
                   " be between 1 and ", nmax, '.', sep=''))
    return(-1)
  }
  # now do the actual work
  cleanup(right.frame) # remove previous subgrid in case user changes dimensions.
  for (i in 1:nr) {              # for the desired number of rows
    temp <- tkframe(right.frame) # make an internal frame to hold the row
    createRow(temp, nc, paste("Row", i, "Col", sep='-'),
          hue=hues[mod(i, length(hues))]) # fill row with correct number of columns
    tkpack(temp)                 # display the row
  }
  tclvalue(state) <- "gridmade"
}

# The key trick here is to combine 'substitute' and 'eval' in order
# to put the values of variables into the command invoked when you
# press the button. I really have no idea why this works....
createRow <- function(parent, n, tag="S", hue=0) {
  if(.rdDebug() > 1) cat('createRow\n')
  colors <- hcl(hue, 85, seq(30, 100, length=n))
  for (i in 1:n) {
    myname <- paste(tag, i, sep='-')
    cmd <- substitute(function() {
      tclvalue(pressed) <- myself
      pressButton()
    }, list(myself=myname))
    x <- tkbutton(parent, text=myname, command=eval(cmd),
                  background=colors[i])
    tkpack(x, side='left', padx="2m", pady="1m")
  }
}


##################################################################
# After setting the grid size, mark the different kinds of negative controls
step2 <- function() {
  if(.rdDebug() > 0) cat('step2\n')
  if (tclvalue(state) == "initial") { # grid not yet made
    tkmessageBox(message="You have to create the grid first!")
  } else if (tclvalue(state) == "gridmade") {
    cleanup(left.frame) # remove the old command buttons
    tkpack(
           tkbutton(left.frame, text="Mark Blank Spots",
                    background="#FFFFFF", command=markBlank),
           tkbutton(left.frame, text="Mark Buffer Spots",
                    background="#CCCCCC", command=markBuffer),
           tkbutton(left.frame, text="Mark Negative Controls",
                    background="#AAAAAA", command=markNC),
           pady='3'
           )
    tkpack(tklabel(left.frame, textvariable=labeler))
    tkpack(but <- tkbutton(left.frame, text="Next Step", command=step3), pady='8')
    tkbind(but, "<Return>", step3)
    tclvalue(state) <- "ready2mark"
  } else { # this is not supposed to happen
    tkmessageBox(message="I seem to have gotten myself into an impossible state.")
  }
}

##################################################################
# This is pure control-flow, since the next steps depend on whether
# we have one level of positive control, or many.
step3 <- function() {
  if(.rdDebug() > 0) cat('step3\n')
  if (!(tclvalue(state) %in% c('ready2mark', 'blank', 'buffer', 'nc'))) {
    tkmessageBox(message="I seem to have gotten myself into an impossible state.")
  }
  # need this to prevent button presses from changing things right now ...
  tclvalue(state) <- "waiting" 
  cleanup(left.frame)
  tkpack(
         tklabel(left.frame, text="Are positive controls in a dilution series?"),
         tkradiobutton(left.frame, text="No",  variable="multi", value=0),
         tkradiobutton(left.frame, text="Yes", variable="multi", value=1),
         pady=3
         )
  tkpack(but <- tkbutton(left.frame, text="Next Step", command=step4), pady='12')
  tkbind(but, "<Return>", step4)
  tcl("set", "multi", 0) # explicitly make 'No' the default
}

##################################################################
# Key point is to switch based on whether (a) we have one kind of positive
# control or (b) they live in a dilution series of their own.
step4 <- function() {
  if(.rdDebug() > 0) cat('step4\n')
  cleanup(left.frame)
  multi <- tclvalue("multi")
  if (multi > 0) { # there is a dilution series
    markHighPC()
    tkpack(
           tkbutton(left.frame, text="Mark Highest Positive Control",
                    background="#890099"),
           tklabel(left.frame, textvariable=labeler),
           but <- tkbutton(left.frame, text="Next Step", command=remainder),
           pady='8'
           )
    tkbind(but, "<Return>", remainder)
  } else {         # there is only one level
    markPC()
    tkpack(
           tkbutton(left.frame, text="Mark Positive Controls", background="#890099"),
           tklabel(left.frame, textvariable=labeler),
           but <- tkbutton(left.frame, text="Next Step", command=step5),
           pady='8'
           )
    tkbind(but, "<Return>", step5)
  }
}

# We only get here in the dilution series case, aftter marking the
# high-intensity starting point
remainder <- function() {
  if (.rdDebug() > 0) cat("remainder\n")
  if (tclvalue("multi") == 0) {
    tkmessageBox(message="This cannot happen")
    return(-1)
  }
  if (tclvalue(highest) == "") {
    tkmessageBox(message=paste("You have to mark the most",
                    "intense positive control first!"))
    return(-1)
  }
  cleanup(left.frame)
  markLowPC()
  tkpack(
         tkbutton(left.frame, text="Mark Lowest Positive Control",
                  background="#FFDEFF"),
         tklabel(left.frame, textvariable=labeler),
         but <- tkbutton(left.frame, text="Next Step", command=step5),
         pady='8'
         )
  tkbind(but, "<Return>", step5)
}

##################################################################
# Here is rte final step, where we can choose to save the results.
step5 <- function() {
  if(.rdDebug() > 0) cat('step5\n')
  cleanup(left.frame)
  tclvalue(state) <- "finished1"
  tkpack(
         s <- tkbutton(left.frame, text="Save and Close", command=saveIt),
         q <- tkbutton(left.frame, text="Quit Without Saving", command=finished),
         pady='8'
         )
  tkbind(s, "<Return>", saveIt)
  tkbind(q, "<Return>", finished)
}

# in order to save, we have to convert the displayed colors
# (from mygrid) into something meaningful.
lookup <- function(x) {
  base <- c(
            "#FFFFFF"="Blank",
            "#CCCCCC"="Buffer",
            "#AAAAAA"="NegCon"
            )
  multi <- as.numeric(tclvalue("multi"))
  if (multi > 0) {  # need to handle the dilution sereis of positive controls
    # get most intense spot location
    temp <- strsplit(tclvalue(highest), '-')[[1]]
    hi <- as.numeric(temp[[2]])
    hj <- as.numeric(temp[[4]])
    # get least intense spot location
    temp <- strsplit(tclvalue(lowest), '-')[[1]]
    i <- as.numeric(temp[[2]])
    j <- as.numeric(temp[[4]])
    # figure out the number of steps
    if (i == hi) {
      n <- 1 + abs(hj-j)
    } else if (j == hj) {
      n <- 1 + abs(hi-i)
    } else {
      tkmessageBox(message="This cannot happen")
      return(-1)
    }
    # regenerate the color list based on its length
    colors <- hcl(300, 85, seq(30, 100, length=n))
    labels <- paste("PosCon", 1:n, sep="")
    names(labels) <- colors
    base <- c(base, labels)
  } else {
    base$"#890099" <- "PosCon1"
  }
  if(.rdDebug() > 0) assign('base', base, rdEnviron)
  val <- base[x]
  val[is.na(val)] <- "Sample"
  val
}

# Some of this is really painful, since the poorly documented interface
# appears not to work for the kind of list that needs to get passed as
# the '-filetypes' argument of the tkgetSaveFile dialog box. So, instead
# of using the higher level R intyerface, we have to go back down to the
# low-level interface through '.Tcl'.
saveIt <- function() {
  if(.rdDebug() > 0) cat('saveIt\n')
  .Tcl( "set types {{{Text Files} {.txt}} {{All Files} {*}}}" )
  cmd <- 'tk_getSaveFile -filetypes $types'
  where <- as.character(.Tcl(cmd))
  if (length(where)==0 || where == '') { # user canceled the save dialog
    return(-1)
  } else { # ready to actually save the results
    if (.rdDebug() > 0) cat(paste("Saving to", where, "\n"))
    nr <- nrow(mygrid)
    nc <- ncol(mygrid)
    stuff <- data.frame(SubRow=rep(1:nr, nc),
                        SubCol=rep(1:nc, each=nr),
                        SpotType=lookup(as.vector(mygrid)))
    # :KRC: should we add some error checking?
    write.table(stuff, file=where, sep="\t", quote=FALSE, col.names=NA)
  }
  finsihed()
}

# if we "quit without saving" ...
finished <- function() {
  if (.rdDebug() > 0) cat("finished\n")
  tkdestroy(top)
}

##################################################################
# This is the only user-visible funciton in the file. You call this
# function to start the application.
rd <- function() {
  assign('top', tktoplevel(), rdEnviron)
  tktitle(top) <- "RPPA Designer"
  assign('state', tclVar("initial"), rdEnviron)
  assign('pressed', tclVar(""), rdEnviron)
  assign('labeler', tclVar("Waiting..."), rdEnviron)
  assign('highest', tclVar(""), rdEnviron)
  assign('lowest', tclVar(""), rdEnviron)
  assign('left.frame', tkframe(top, relief='groove', borderwidth=4), rdEnviron)
  assign('right.frame', tkframe(top, relief='groove', borderwidth=4),rdEnviron)
  tkpack(left.frame, right.frame, side='left')
  tkpack(lu <- tkframe(left.frame),
         lm <- tkframe(left.frame),
         ll <- tkframe(left.frame))
  tkpack(tklabel(lu, text="     Number of rows:"),
         tkentry(lu, width=6, textvariable="nr"),
         side='left')
  tkpack(tklabel(lm, text="Number of columns:"),
         tkentry(lm, width=6, textvariable="nc"),
         side='left')
  tkpack(cr <- tkbutton(ll, text="Create Subgrid", command=createSubgrid),
         ok <- tkbutton(ll, text="Next Step", command=step2),
         pady='3')
  tkbind(cr, "<Return>", createSubgrid) # note the undocumented syntax ...
  tkbind(ok, "<Return>", step2)
  tkwait.window(top)
}
