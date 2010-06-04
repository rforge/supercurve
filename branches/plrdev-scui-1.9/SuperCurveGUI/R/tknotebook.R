##
### TKNOTEBOOK.R
###

options(warn=1)
require(tcltk) || stop("tcltk support is missing")


##
## Private Methods
##

##-----------------------------------------------------------------------------
## Set size of notebook to same as greatest width/height of its pages.
.notebook_fix_size <- function(notebook) {
    ## Check arguments
    stopifnot(is.tkwin(notebook))
    stopifnot(tclvalue(tkwinfo.class(notebook)) == "Notebook")

    ## Begin processing

    ## Force packer to do its job now so dimensions are meaningful
    tclupdate("idletasks")

    ## Determine greatest width/height using all pages
    maxw <- as.integer(0)
    maxh <- as.integer(0)
    pages <- evalq(userdata$Pages, envir=notebook$env)
    for (page in pages) {
        w <- as.integer(tkwinfo.reqwidth(page))
        if (w > maxw) {
            maxw <- w
        }

        h <- as.integer(tkwinfo.reqheight(page))
        if (h > maxh) {
            maxh <- h
        }
    }

    ## Set maximum width/height of notebook, compensating for border width
    bw <- as.integer(tkcget(notebook, "-borderwidth"))
    tkconfigure(notebook,
                width=(maxw + (2 * bw)),
                height=(maxh + (2 * bw)))
}


##
## Public Methods
##

##-----------------------------------------------------------------------------
notebook_create <- function(parent) {
    ## Check arguments
    stopifnot(is.tkwin(parent))

    ## Begin processing
    notebook <- tkframe(parent,
                        class="Notebook")
    ## Prevent notebook from changing size
    tkpack.propagate(notebook, FALSE)

    userdata <- list(Current=as.character(""),
                     Pages=vector("character", length=0),
                     Titles=vector("character", length=0))
    assign("userdata", userdata, envir=notebook$env)

    return(notebook)
}


##-----------------------------------------------------------------------------
notebook_display <- function(notebook,
                             pagename) {
    ## Check arguments
    stopifnot(is.tkwin(notebook))
    stopifnot(tclvalue(tkwinfo.class(notebook)) == "Notebook")
    stopifnot(is.character(pagename) || is.numeric(pagename))

    ## Begin processing
    userdata <- get("userdata", envir=notebook$env)
    stopifnot(is.list(userdata))

#cat("entering notebook_display -", pagename, "\n")
#show(str(userdata))

    if (is.character(pagename)) {
        for (pageno in seq_along(userdata$Pages)) {
            if (identical(pagename, userdata$Titles[pageno])) {
                break
            }
        }
    } else if (is.numeric(pagename)) {
        pageno <- pagename
    }

    page <- if (pageno > 0 && pageno <= length(userdata$Pages)) {
                userdata$Pages[pageno]
            } else {
                ""
            }

    if (!nzchar(page)) {
        stop(sprintf("invalid notebook page %s",
                     dQuote(pagename)))
    }

    ## Set notebook to fixed size
    .notebook_fix_size(notebook)

    ## Unmap current page
    if (nzchar(userdata$Current)) {
        tkpack.forget(userdata$Current)
    }

    ## Map requested page
    tkpack(page,
           expand=TRUE,
           fill="both")

    userdata$Current <- page
    assign("userdata", userdata, envir=notebook$env)
#cat("exiting notebook_display", "\n")
#show(str(userdata))
}


##-----------------------------------------------------------------------------
notebook_page <- function(notebook,
                          pagename) {
    ## Check arguments
    stopifnot(is.tkwin(notebook))
    stopifnot(tclvalue(tkwinfo.class(notebook)) == "Notebook")
    stopifnot(is.character(pagename))

    ## Begin processing
    page <- tkframe(notebook)

    userdata <- get("userdata", envir=notebook$env)
    stopifnot(is.list(userdata))

    if (pagename %in% userdata$Titles) {
        message(sprintf("notebook page name %s is not unique",
                        dQuote(pagename)))
        warning("changing notebook page by name may not display desired page")
    }

    pageno <- length(userdata$Pages) + 1
    userdata$Pages[pageno] <- page$ID
    userdata$Titles[pageno] <- pagename
    assign("userdata", userdata, envir=notebook$env)

    if (pageno == 1) {
        ## Display first page by default
        tclafter.idle(function() {
                          notebook_display(notebook, pageno)
                      })
    }

    return(page)
}


##
## Tcl resource database
##

optiondb_add("*Notebook.borderWidth", 2, "widgetDefault")
optiondb_add("*Notebook.relief", "sunken", "widgetDefault")

