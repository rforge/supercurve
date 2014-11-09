###
### $Id$
###


options(warn=1)
pkgname <- "SuperCurve"
suppressPackageStartupMessages(library(pkgname, character.only=TRUE))

## Load packages for all possible processing options
suggested <- unlist(strsplit(packageDescription(pkgname)$Suggests, ', '))
invisible(sapply(suggested,
                 function(pkgname) {
                     suppressPackageStartupMessages(
                         require(pkgname, character.only=TRUE))
                 }))

show(sessionInfo())

