###
### TESTRPPA.R
###


options(warn=1)
library(SuperCurve)
source("checkFuncs")

path <- system.file("rppaCellData", package="SuperCurve")

###########################
## tests of file

checkException(RPPA(pi),
               msg="invalid value should fail - must be connection")
checkException(RPPA(list.files(path)),
               msg="character vector (file) should fail")
    ## :TBD: This seems reasonable to me... (PLR)
quantfile <- list.files(path)[1]
checkException(RPPA(file.path(path, quantfile)),
               msg="absolute pathname should fail")
checkException(RPPA(""),
               msg="empty string should fail - nonexistant file")
nosuchfile <- "nosuch.tsv"
checkException(RPPA(nosuchfile),
               msg="nonexistent file should fail")


###########################
## tests of path

checkException(RPPA(quantfile,
                    path=pi),
               msg="invalid value should fail")
checkException(RPPA(quantfile,
                    path=c(path, path.expand("~"))),
               msg="character vector should fail")
checkException(RPPA(quantfile,
                    path=""),
               msg="empty string should fail")


###########################
## tests of readQuantification file

wrtconn <- textConnection(NULL, "w", local=TRUE)
checkException(RPPA(wrtconn),
               msg="write-only connection should fail")
close(wrtconn)

checkException(RPPA("cellLineInfo.tsv", path=path),
               msg="not MicroVigene datafile should fail")

###########################
## tests of readQuantification software

checkException(RPPA(quantfile,
                    path=path,
                    software=pi),
               msg="invalid value should fail")

checkException(RPPA(quantfile,
                    path=path,
                    software=c("foo", "bar")),
               msg="character vector should fail")

checkException(RPPA(quantfile,
                    path=path,
                    software=""),
               msg="empty string should fail")

checkException(RPPA(quantfile,
                    path=path,
                    software="bogus"),
               msg="data import should fail - missing method")

read.noCols <- function(file) {
    data.frame()
}

checkException(RPPA(quantfile,
                    path=path,
                    software="noCols"),
               msg="data import should fail - not enough columns")

read.missingReqdCols <- function(file) {
    data.frame(a=1, b=2, c=3, d=4, e=5, f=6)
}

checkException(RPPA(quantfile,
                    path=path,
                    software="missingReqdCols"),
               msg="data import should fail - required columns missing")

read.notEnuffRows <- function(file) {
    data.frame(Main.Row=1, Main.Col=2, Sub.Row=3, Sub.Col=4, Sample="sample",
               Other=6)
}

checkException(RPPA(quantfile,
                    path=path,
                    software="notEnuffRows"),
               msg="data import should fail - not enough rows")







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

