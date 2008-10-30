###
### TESTRPPA.R
###


options(warn=1)
library(SuperCurve)

path <- system.file("rppaCellData", package="SuperCurve")

try( RPPA("cellLineInfo.tsv") )            # file does not exist
try( RPPA("cellLineInfo.tsv", path=path) ) # not MV datafile

akt <- RPPA("Akt.txt", path=path)
image(akt, colorbar=TRUE)

try( image(akt, colorbar=1) ) # the error checking is wrong here
try( image(akt, colorbar='red') ) # okay here?

try( image(akt, measure="bogus") ) # invalid measure

if (getRversion() < "2.8.0") {
    cat("Using outdated version of R...", "\n")
    pdf(width=1.2, height=6)
} else {
    dev.new(width=1.2, height=6)
}
try( image(akt) )
try( image(akt, colorbar=TRUE) )
dev.off()

