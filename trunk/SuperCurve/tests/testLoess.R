library(SuperCurve)

######################################
# load the tumor data
home <- system.file("rppaTripleData", package="SuperCurve")

# first locate the list of assays
# the name 'proteins' is required
# must include a column named 'Antibody'
proteins <- read.table(file.path(home, 'proteinAssay.tsv'),
                       header=TRUE, sep='\t', as.is=TRUE)
dimnames(proteins)[[1]] <- as.character(proteins$Antibody)

for (i in 1:nrow(proteins)) {
  temp <- RPPA(proteins$File[i], path=home)
  assign(proteins$Antibody[i], temp, 1)
}
rm(i, temp)

######################################
# work out the appropriate design layout
design <- RPPADesign(ACTB)

######################################
# must define the 'model' to use
model <- "loess"

######################################
# must define the 'measure' to use
measure <- "Mean.Net"

######################################
# must define the 'method' to use
method <- "nls"
source("testRblock", echo=TRUE, max.deparse.len=1024)

method <- 'nlrob'
source("testRblock", echo=TRUE, max.deparse.len=1024)

method <- 'nlrq'
source("testRblock", echo=TRUE, max.deparse.len=1024)

######################################
## print the concentrations from the last fit. These will
## automatically be compared with the saved output when we
## run 'R CMD check'
temp@concentrations

######################################
## This early design had each series repeated nine times,
## three times within a subgrid and again in three adjacent
## subgrids. Here we measure the variability of the
## replicates.
d <- ACTB@data[seq(6, nrow(ACTB@data), by=6),]
attach(d)
foo <- paste("Series", Main.Row, Main.Col, Sub.Row, sep='.')
detach()
sum(foo==seriesNames(design))

avgs <- tapply(temp@concentrations, list(d$Sample), mean)
spread <- tapply(temp@concentrations, list(d$Sample), sd)
res <- data.frame(Mean=avgs, SD=spread, CV=spread/abs(avgs))
res[order(res$SD),]
