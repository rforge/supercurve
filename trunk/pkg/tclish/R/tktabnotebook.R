###
### TKTABNOTEBOOK.R
###

options(warn=1)
require(tcltk) || stop("tcltk support is missing")


##
## Private Methods
##

##-----------------------------------------------------------------------------
.tabnotebook_canvas <- function(tabnotebook) {
    ## Check arguments
    stopifnot(is.tkwin(tabnotebook))
    stopifnot(tclvalue(tkwinfo.class(tabnotebook)) == "Tabnotebook")

    ## Begin processing
    return(evalq(userdata$Canvas, envir=tabnotebook$env))
}


##-----------------------------------------------------------------------------
.tabnotebook_notebook <- function(tabnotebook) {
    ## Check arguments
    stopifnot(is.tkwin(tabnotebook))
    stopifnot(tclvalue(tkwinfo.class(tabnotebook)) == "Tabnotebook")

    ## Begin processing
    return(evalq(userdata$Notebook, envir=tabnotebook$env))
}


##-----------------------------------------------------------------------------
## Clears canvas and draws a tab for each page in the notebook.
.tabnotebook_refresh <- function(tabnotebook) {
    ## Check arguments
    stopifnot(is.tkwin(tabnotebook))
    stopifnot(tclvalue(tkwinfo.class(tabnotebook)) == "Tabnotebook")

    ## Begin processing
    tabarea <- .tabnotebook_canvas(tabnotebook)
    notebook <- .tabnotebook_notebook(tabnotebook)

    ## Delete all items
    tkdelete(tabarea, "all")

    ## Grab values from option database
    value <- tclvalue(optiondb_get(tabnotebook, "margin", "Margin"))
    margin <- as.numeric(value)
    color <- tclvalue(optiondb_get(tabnotebook, "tabColor", "Color"))
    font <- tclvalue(optiondb_get(tabnotebook, "tabFont", "Font"))

    userdata <- get("userdata", envir=tabnotebook$env)
    stopifnot(is.list(userdata))

    x <- as.integer(2)
    maxh <- as.integer(0)

    ## Draw tabs in order from left to right
    for (name in userdata$Tabs) {

        ## Create text item for tab
        id <- tkcreate(tabarea,
                       "text",
                       (x + margin + 2),
                       (-0.5 * margin),
                       anchor="sw",
                       text=name,
                       font=font,
                       tags=name)

        ## Determine size of text object
        values <- unlist(strsplit(tclvalue(tkbbox(tabarea, id)), " "))
        bbox <- as.numeric(values)

        wd <- bbox[3] - bbox[1]
        ht <- bbox[4] - bbox[2]
        if (ht > maxh) {
            maxh <- ht
        }

        ## Create polygon item for tab
        tkcreate(tabarea,
                 "polygon",
                 0, 0,
                 x, 0,
                 (x + margin), (-ht - margin),
                 (x + margin + wd), (-ht - margin),
                 (x + wd + 2 * margin), 0,
                 2000, 0,
                 2000, 10,
                 0, 10,
                 outline="black",
                 fill=color,
                 tags=sprintf("%s tab tab-%s", name, name))

        ## TAG usage guide:
        ## tab        : applies to all tab polygons
        ## tab-<name> : applies to specific tab polygon
        ## <name>     : applies to both text and polygon (whole tab)

        ## Put text item back on top of tab
        tkitemraise(tabarea, id)

        ## Click on tab invokes code to display its associated page
        dobind <- function(tabname) {
            tkitembind(tabarea,
                       tabname,
                       "<ButtonPress-1>",
                       function() {
                           tabnotebook_display(tabnotebook, tabname)
                       })
        }
        dobind(name)

        ## Shift x to right (position for start edge of next tab)
        x <- x + wd + 2 * margin
    }

    ## Compute overall height of tab set and move all tabs down into position
    height <- maxh + 2 * margin
    tkmove(tabarea, "all", 0, height)

    ## Set size just large enough to display final tab set
    tkconfigure(tabarea,
                width=x,
                height=height + 4)

    ## Display page for currently selected tab (or first one if none selected)
    if (nzchar(userdata$Current)) {
        tabnotebook_display(tabnotebook, userdata$Current)
    } else {
        tabnotebook_display(tabnotebook, userdata$Tabs[1])
    }

    ## Reset pending update so later updates are allowed
    userdata$Pending <- ""
    assign("userdata", userdata, envir=tabnotebook$env)
}


##
## Public Methods
##

##-----------------------------------------------------------------------------
tabnotebook_create <- function(parent) {
    ## Check arguments
    stopifnot(is.tkwin(parent))

    ## Begin processing
    tabnotebook <- tkframe(parent,
                           class="Tabnotebook")
    tabarea <- tkcanvas(tabnotebook,
                        highlightthickness=0)
    tkpack(tabarea,
           fill="x")

    notebook <- notebook_create(tabnotebook)
    tkpack(notebook,
           expand=TRUE,
           fill="both")

    userdata <- list(Current=as.character(""),
                     Pending=as.character(""),
                     Tabs=vector("character", length=0),
                     Canvas=tabarea,
                     Notebook=notebook)
    assign("userdata", userdata, envir=tabnotebook$env)

    return(tabnotebook)
}


##-----------------------------------------------------------------------------
tabnotebook_display <- function(tabnotebook,
                                tabname) {
    ## Check arguments
    stopifnot(is.tkwin(tabnotebook))
    stopifnot(tclvalue(tkwinfo.class(tabnotebook)) == "Tabnotebook")
    stopifnot(is.character(tabname) || is.numeric(tabname))

    ## Begin processing
    tabarea <- .tabnotebook_canvas(tabnotebook)
    notebook <- .tabnotebook_notebook(tabnotebook)

    ## Bring up notebook page
    notebook_display(notebook, tabname)

    ## Highlight current tab and raise it to foreground
    normal <- tclvalue(optiondb_get(tabnotebook, "tabColor", "Color"))
    tkitemconfigure(tabarea, "tab", fill=normal)
    active <- tclvalue(optiondb_get(tabnotebook, "activeTabColor", "Color"))
    activeTab <- sprintf("tab-%s", tabname)
    tkitemconfigure(tabarea, activeTab, fill=active)
    tkitemraise(tabarea, tabname)

    userdata <- get("userdata", envir=tabnotebook$env)
    stopifnot(is.list(userdata))
    userdata$Current <- tabname
    assign("userdata", userdata, envir=tabnotebook$env)
}


##-----------------------------------------------------------------------------
tabnotebook_page <- function(tabnotebook,
                             tabname) {
    ## Check arguments
    stopifnot(is.tkwin(tabnotebook))
    stopifnot(tclvalue(tkwinfo.class(tabnotebook)) == "Tabnotebook")
    stopifnot(is.character(tabname))

    ## Begin processing
    notebook <- .tabnotebook_notebook(tabnotebook)

    page <- notebook_page(notebook, tabname)

    userdata <- get("userdata", envir=tabnotebook$env)
    stopifnot(is.list(userdata))

    tabno <- length(userdata$Tabs) + 1
    userdata$Tabs[tabno] <- tabname

    ## Refresh contents of tabarea if no update is currently pending
    if (!nzchar(userdata$Pending)) {
        ## Done as idle task to avoid unnecessary tab regeneration
        userdata$Pending <- tclvalue(tclafter.idle(function() {
                                              .tabnotebook_refresh(tabnotebook)
                                          }))
    }
    assign("userdata", userdata, envir=tabnotebook$env)

    return(page)
}


##
## Tcl resource database
##

if (getPackageName() == ".GlobalEnv") {
    optiondb_add("*Tabnotebook.Canvas.background", "#666666", "widgetDefault")
    optiondb_add("*Tabnotebook.margin", 6, "widgetDefault")
    optiondb_add("*Tabnotebook.tabColor", "#a6a6a6", "widgetDefault")
    optiondb_add("*Tabnotebook.activeTabColor", "#d9d9d9", "widgetDefault")
    optiondb_add("*Tabnotebook.tabFont",
                 "-*-helvetica-bold-r-normal--*-120-*", "widgetDefault")
}

