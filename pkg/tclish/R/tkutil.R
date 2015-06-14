###
### $Id$
### Methods implementing Tk routines not in tcltk package
###

require(tcltk) || stop("tcltk support is missing")


##
## Public Methods
##

##-----------------------------------------------------------------------------
## Create menu widgets treated specially by different platforms.
## Provided access to special Apple system menu (platform-specific).
tkapplemenu <- function(parent,
                        ...) {
    .tkspecialmenu(parent, special="apple", ...)
}


##-----------------------------------------------------------------------------
## Create menu widgets treated specially by different platforms.
## Provided access to special help menu (platform-specific).
tkhelpmenu <- function(parent,
                       ...) {
    .tkspecialmenu(parent, special="help", ...)
}


##-----------------------------------------------------------------------------
## Delete image.
tkimage.delete <- function(...) {
    tcl("image", "delete", ...)
}


##-----------------------------------------------------------------------------
## Create labelframe widget.
tklabelframe <- function(parent,
                         ...) {
    stopifnot(is.tkwin(parent))

    tkwidget(parent, "labelframe", ...)
}


##-----------------------------------------------------------------------------
## Create option menu widget since none is explicitly provided by Tk.
tkOptionMenu <- function(parent,
                         variable,
                         ...) {
    stopifnot(is.tkwin(parent))
    stopifnot(is.tclVar(variable))

    tkwidget(parent,
             "tk_optionMenu",
             variable,
             ...)
}


##-----------------------------------------------------------------------------
## Create simulated separator widget since none is explicitly provided by Tk.
tkSeparator <- function(parent,
                        ...) {
    stopifnot(is.tkwin(parent))

    tkframe(parent,
            borderwidth="1",
            class="Separator",
            height="2",
            relief="sunken")
}


##-----------------------------------------------------------------------------
## Create spinner widget.
tkspinbox <- function(parent,
                      ...) {
    stopifnot(is.tkwin(parent))

    tkwidget(parent, "spinbox", ...)
}


##-----------------------------------------------------------------------------
## Create menu widgets treated specially by different platforms.
## Provided access to special Windows system menu (platform-specific).
tksystemmenu <- function(parent,
                         ...) {
    .tkspecialmenu(parent, special="system", ...)
}


##-----------------------------------------------------------------------------
## Provides a few miscellaneous entry points into Tk library.
tktk <- function(...) {
    tcl("tk", ...)
}


##-----------------------------------------------------------------------------
## Queries or sets the application name, used by Tk 'send' command.
tktk.appname <- function(...) {
    tcl("tk", "appname", ...)
}


##-----------------------------------------------------------------------------
## Queries or sets the caret location for the display specified window.
tktk.caret <- function(...) {
    tcl("tk", "caret", ...)
}


##-----------------------------------------------------------------------------
## Queries or sets the current scaling factor used by Tk to convert between
## physical units and pixels.
tktk.scaling <- function(...) {
    tcl("tk", "scaling", ...)
}


##-----------------------------------------------------------------------------
## Queries or sets the state of whether Tk should use XIM (X Input Methods)
## for filtering events. Resulting state is returned. If XIM support not
## available, return 0.
tktk.useinputmethods <- function(...) {
    tcl("tk", "useinputmethods", ...)
}


##-----------------------------------------------------------------------------
## Returns the current windowing system: x11, win32, classic, or aqua.
tktk.windowingsystem <- function(...) {
    tcl("tk", "windowingsystem", ...)
}


##-----------------------------------------------------------------------------
## Return decimal string giving number of cells in colormap for window.
tkwinfo.cells <- function(...) {
    tcl("winfo", "cells", ...)
}


##-----------------------------------------------------------------------------
## Return string containing path names of all children of window.
tkwinfo.children <- function(...) {
    tcl("winfo", "children", ...)
}


##-----------------------------------------------------------------------------
## Return class name for window.
tkwinfo.class <- function(...) {
    tcl("winfo", "class", ...)
}


##-----------------------------------------------------------------------------
## Return 1 if colormap for window is known to be full; otherwise, 0.
tkwinfo.colormapfull <- function(...) {
    tcl("winfo", "colormapfull", ...)
}


##-----------------------------------------------------------------------------
## Return path name for window containing point given by rootX and rootY. If
## no window contains the point, an empty string is returned.
tkwinfo.containing <- function(...) {
    tcl("winfo", "containing", ...)
}


##-----------------------------------------------------------------------------
## Return decimal string giving depth of window (number of bits/pixel).
tkwinfo.depth <- function(...) {
    tcl("winfo", "depth", ...)
}


##-----------------------------------------------------------------------------
## Return 1 if window named window exists; otherwise, 0.
tkwinfo.exists <- function(...) {
    tcl("winfo", "exists", ...)
}


##-----------------------------------------------------------------------------
## Return floating-point value giving number of pixels in window corresponding
## to distance.
tkwinfo.fpixels <- function(...) {
    tcl("winfo", "fpixels", ...)
}


##-----------------------------------------------------------------------------
## Returns string giving window's geometry in pixels as WIDTHxHEIGHT+X+Y.
tkwinfo.geometry <- function(...) {
    tcl("winfo", "geometry", ...)
}


##-----------------------------------------------------------------------------
## Returns decimal string giving window's height in pixels.
tkwinfo.height <- function(...) {
    tcl("winfo", "height", ...)
}


##-----------------------------------------------------------------------------
## Return hexidecimal string giving low-level platform-specific identifier for
## window. On Un*x, this is the X Window identifier. On Windows, this is the
## Windows HWND. Meaningless on Macintosh.
tkwinfo.id <- function(...) {
    tcl("winfo", "id", ...)
}


##-----------------------------------------------------------------------------
## Return 1 if window is currently mapped; otherwise, 0.
tkwinfo.ismapped <- function(...) {
    tcl("winfo", "ismapped", ...)
}


##-----------------------------------------------------------------------------
## Return name of geometry manager responsible for window, or empty string if
## window isn't managed.
tkwinfo.manager <- function(...) {
    tcl("winfo", "manager", ...)
}


##-----------------------------------------------------------------------------
## Return window's name (name within parent, not the full path).
tkwinfo.name <- function(...) {
    tcl("winfo", "name", ...)
}


##-----------------------------------------------------------------------------
## Return the path name of window's parent.
tkwinfo.parent <- function(...) {
    tcl("winfo", "parent", ...)
}


##-----------------------------------------------------------------------------
## Return integer number of pixels in window corresponding to distance.
tkwinfo.pixels <- function(...) {
    tcl("winfo", "pixels", ...)
}


##-----------------------------------------------------------------------------
## If mouse pointer is on same screen as window, return pointer's x-coordinate,
## measured in pixels in the screen's root window; otherwise, -1.
tkwinfo.pointerx <- function(...) {
    tcl("winfo", "pointerx", ...)
}


##-----------------------------------------------------------------------------
## If mouse pointer is on same screen as window, return pointer's x and y
## coordinates, measured in pixels in the screen's root window; otherwise, both
## coordinates are -1.
tkwinfo.pointerxy <- function(...) {
    tcl("winfo", "pointerxy", ...)
}


##-----------------------------------------------------------------------------
## If mouse pointer is on same screen as window, return pointer's y-coordinate,
## measured in pixels in the screen's root window; otherwise, -1.
tkwinfo.pointery <- function(...) {
    tcl("winfo", "pointery", ...)
}


##-----------------------------------------------------------------------------
## Returns decimal string giving window's requested height in pixels.
tkwinfo.reqheight <- function(...) {
    tcl("winfo", "reqheight", ...)
}


##-----------------------------------------------------------------------------
## Returns decimal string giving window's requested width in pixels.
tkwinfo.reqwidth <- function(...) {
    tcl("winfo", "reqwidth", ...)
}


##-----------------------------------------------------------------------------
## Returns decimal string giving screen's height in pixels.
tkwinfo.screenheight <- function(...) {
    tcl("winfo", "screenheight", ...)
}


##-----------------------------------------------------------------------------
## Returns decimal string giving screen's width in pixels.
tkwinfo.screenwidth <- function(...) {
    tcl("winfo", "screenwidth", ...)
}


##-----------------------------------------------------------------------------
## Returns path name of toplevel window containing window.
tkwinfo.toplevel <- function(...) {
    tcl("winfo", "toplevel", ...)
}


##-----------------------------------------------------------------------------
## Return 1 if window and all its ancestors up through the nearest toplevel
## window are mapped; otherwise, 0.
tkwinfo.viewable <- function(...) {
    tcl("winfo", "viewable", ...)
}


##-----------------------------------------------------------------------------
## Returns one of following strings to indicate visual class for window:
## directcolor, grayscale, pseudocolor, staticcolor, staticgray, or truecolor.
tkwinfo.visual <- function(...) {
    tcl("winfo", "visual", ...)
}


##-----------------------------------------------------------------------------
## Returns decimal string giving window's width in pixels.
tkwinfo.width <- function(...) {
    tcl("winfo", "width", ...)
}


##-----------------------------------------------------------------------------
## Returns decimal string giving x-coordinate, in window's parent, of
## upper-left corner of window's border.
tkwinfo.x <- function(...) {
    tcl("winfo", "x", ...)
}


##-----------------------------------------------------------------------------
## Returns decimal string giving y-coordinate, in window's parent, of
## upper-left corner of window's border.
tkwinfo.y <- function(...) {
    tcl("winfo", "y", ...)
}


##
## Private Methods
##

##-----------------------------------------------------------------------------
## Create menu widgets treated specially by different platforms.
## Method provided by the tcltk package does not take advantage of
## platform-specific resources.
.tkspecialmenu <- function(parent,
                           special=c("apple",
                                     "help",
                                     "system"),
                           ...) {
    stopifnot(is.tkwin(parent))

    ##-------------------------------------------------------------------------
    tkcascade <- function(parent,
                          type,
                          special,
                          ...) {
        ID <- paste(parent$ID, special, sep=".")
        #evalq(num.subwin <- num.subwin + 1, parent$env)
        parent$env$num.subwin <- parent$env$num.subwin + 1
        win <- .Tk.newwin(ID)
        assign(ID, win, envir = parent$env)
        assign("parent", parent, envir = win$env)
        tcl(type, win, ...)
        win
    }

    special <- match.arg(special)
    switch(.Platform$OS.type,
           unix=function() {
                    if (special == "help") {
                        tkcascade(parent, "menu", special, ...)
                    } else {
                        stop(sprintf("invalid special menu %s for %s OS",
                                     special,
                                     .Platform$OS.type))
                    }
                },
           windows=function() {
                    if (special != "apple") {
                        tkcascade(parent, "menu", special, ...)
                    } else {
                        stop(sprintf("invalid special menu %s for %s OS",
                                     special,
                                     .Platform$OS.type))
                    }
                },
           stop("invalid os type"))
    tkcascade(parent, "menu", special, ...)
}

