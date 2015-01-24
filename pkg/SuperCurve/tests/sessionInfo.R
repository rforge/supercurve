###
### $Id$
###


options(warn=1)
pkgname <- "SuperCurve"
suppressPackageStartupMessages(library(pkgname, character.only=TRUE))


##-----------------------------------------------------------------------------
## Returns character of package names corresponding to "Suggests:" field
suggestedPackages <- function(pkgname) {
    ## Mung suggested packages metadata to remove tab and newline characters
    suggests <- packageDescription(pkgname)$Suggests
    suggests <- gsub('\n', ' ', suggests)
    suggests <- gsub('\t', '',  suggests)

    unlist(strsplit(suggests, ', '))
}


## Load packages for all possible processing options
invisible(sapply(suggestedPackages(),
                 function(pkgname) {
                     suppressPackageStartupMessages(
                         require(pkgname, character.only=TRUE))
                 }))

show(sessionInfo())

