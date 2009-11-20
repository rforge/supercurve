###
### ZZZ.R
###


##
## Package/Namespace Hooks
##

##-----------------------------------------------------------------------------
.onAttach <- function(libname, pkgname) {
    if (getOption("verbose")) {
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
            packageStartupMessage(paste(desc$Title, desc$Version))
            packageStartupMessage(
                sprintf("Type library(help=%s) to see package documentation",
                        libraryPkgName(pkgname)))
        })
    }
}

