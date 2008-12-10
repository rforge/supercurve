###
### TESTRPPA.R
###


options(warn=1)
library(SuperCurve)
source("checkFuncs")

path <- system.file("rppaCellData", package="SuperCurve")

checkException(RPPA("nosuch.tsv"),
               msg="nonexistent file should fail")
checkException(RPPA("cellLineInfo.tsv", path=path),
               msg="not MicroVigene datafile should fail")

akt <- RPPA("Akt.txt", path=path)
image(akt, colorbar=TRUE)

image(akt, colorbar=1) # numeric colorbar value silently converted to logical

checkException(image(akt, colorbar="red"),
               msg="character value should fail")

checkException(image(akt, measure="bogus"),
               msg="invalid measure should fail")

if (getRversion() < "2.8.0") {
    cat("Using outdated version of R...", "\n")
    pdf(width=1.2, height=6)
} else {
    dev.new(width=1.2, height=6)
}
try( image(akt) )
try( image(akt, colorbar=TRUE) )
dev.off()

