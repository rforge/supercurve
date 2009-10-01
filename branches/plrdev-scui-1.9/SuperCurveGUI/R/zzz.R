###
### ZZZ.R
###


##
## Package/Namespace Hooks
##

##-----------------------------------------------------------------------------
.onAttach <- function(libname, pkgname) {
    verbose <- getOption("verbose")
    if (verbose) {
        local({
            libraryPkgName <- function(pkgname, sep="_") {
                unlist(strsplit(pkgname, sep, fixed=TRUE))[1]
            }
            packageDescription <- function(pkgname) {
                fieldnames <- c("Title", "Version")
                descfile <- file.path(libname, pkgname, "DESCRIPTION")
                desc <- as.list(read.dcf(descfile, fieldnames))
                names(desc) <- fieldnames
                return(desc)
            }

            desc <- packageDescription(pkgname)
            msg <- sprintf("%s, version %s",
                           desc$Title, desc$Version)
            packageStartupMessage(msg)
            msg <- sprintf("Type library(help=%s) to see package documentation",
                           libraryPkgName(pkgname))
            packageStartupMessage(msg)
        })
    }
}


##-----------------------------------------------------------------------------
## Initialize Tcl resource database
.onLoad <- function(libname, pkgname) {
    optiondb_add("*Notebook.borderWidth", 2, "widgetDefault")
    optiondb_add("*Notebook.relief", "sunken", "widgetDefault")
    optiondb_add("*Tabnotebook.Canvas.background", "#666666", "widgetDefault")
    optiondb_add("*Tabnotebook.margin", 6, "widgetDefault")
    optiondb_add("*Tabnotebook.tabColor", "#a6a6a6", "widgetDefault")
    optiondb_add("*Tabnotebook.activeTabColor", "#d9d9d9", "widgetDefault")
    optiondb_add("*Tabnotebook.tabFont",
                 "-*-helvetica-bold-r-normal--*-120-*", "widgetDefault")
}

