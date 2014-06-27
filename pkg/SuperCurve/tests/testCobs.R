###
### $Id$
###


if (!nzchar(Sys.getenv("SUPERCURVE_FULL_TEST"))) {
    cat(">>>>                <<<<", "\n")
    cat(">>>>  Test skipped  <<<<", "\n")
    cat(">>>>                <<<<", "\n")
    message(sprintf("To run all package tests, define %s environment variable",
                    dQuote("SUPERCURVE_FULL_TEST")))
    q("no")
}
options(warn=1)
library(SuperCurve)
library(robustbase)

extdata.dir <- system.file("extdata", package="SuperCurve")

######################################
## load the tumor data
home <- file.path(extdata.dir, "rppaTumorData")

## first locate the list of assays
## the name 'proteins' is required
## must include two columns named 'Antibody' and 'Filename'.
proteins <- read.delim(file.path(home, "proteinAssay.tsv"), as.is=TRUE)
rownames(proteins) <- as.character(proteins$Antibody)

for (i in seq_len(nrow(proteins))) {
    temp <- RPPA(proteins$Filename[i],
                 path=home,
                 antibody=proteins$Antibody[i])
    assign(proteins$Antibody[i], temp, 1)
}
remove(i, temp)

######################################
## work out the appropriate design layout
design <- RPPADesign(ERK2,
                     grouping="blockSample",
                     center=TRUE,
                     controls=list("neg con", "pos con"))

######################################
## must define the 'model' to use
model <- "cobs"

######################################
## must define the 'measure' to use
measure <- "Mean.Net"

######################################
## must define the 'method' to use
method <- 'nlrq'
source("testRblock", echo=TRUE, max.deparse.len=1024)

method <- 'nlrob'
source("testRblock", echo=TRUE, max.deparse.len=1024)

method <- "nls"
source("testRblock", echo=TRUE, max.deparse.len=1024)

######################################
## print the concentrations from the last fit. These will
## automatically be compared with the saved output when we
## run 'R CMD check'
round(temp@concentrations, digits=4)

