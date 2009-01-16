###
### TESTLOESS.R
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

######################################
## load the tumor data
home <- system.file("rppaTripleData", package="SuperCurve")

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
rm(i, temp)

######################################
## work out the appropriate design layout
design <- RPPADesign(ACTB)

######################################
## must define the 'model' to use
model <- "loess"

######################################
## must define the 'measure' to use
measure <- "Mean.Net"

######################################
## loess is very slow; we are only going to test
## a single protein.
proteins <- proteins[nrow(proteins), ]

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

######################################
## This early design had each series repeated nine times,
## three times within a subgrid and again in three adjacent
## subgrids. Here we measure the variability of the
## replicates.
d <- ACTB@data[seq(6, nrow(ACTB@data), by=6), ]
attach(d)
foo <- paste("Series", Main.Row, Main.Col, Sub.Row, sep=".")
detach()
sum(foo == seriesNames(design))

avgs <- tapply(temp@concentrations, list(d$Sample), mean)
spread <- tapply(temp@concentrations, list(d$Sample), sd)
res <- data.frame(Mean=avgs, SD=spread, CV=spread/abs(avgs))
round(res[order(res$SD), ], digits=4)

