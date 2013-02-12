###
### TESTRPPA.R
###


options(warn=1)
library(SuperCurve)
source("checkFuncs")

extdata.dir <- system.file("extdata", package="SuperCurve")
path <- file.path(extdata.dir, "rppaCellData")

###########################
## tests of file

checkException(RPPA(pi),
               msg="invalid value should fail - must be connection")
checkException(RPPA(list.files(path)),
               msg="character vector (file) should fail")
quantfile <- list.files(path)[1]
checkTrue(is.RPPA(RPPA(file.path(path, quantfile))),
          msg="absolute pathname should succeed")
checkException(RPPA(""),
               msg="empty string should fail")
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


###########################
## tests of antibody

quantfile <- list.files(path)[1]
checkException(RPPA(file.path(path, quantfile),
                    antibody=p1),
               msg="invalid value should fail")
checkException(RPPA(file.path(path, quantfile),
                    antibody=LETTERS),
               msg="character vector should fail")
checkException(RPPA(file.path(path, quantfile),
                    antibody=""),
               msg="empty string should fail")
local({
    rppa <- RPPA(file.path(path, quantfile))
    checkTrue(identical(rppa@antibody,
                        sub(".txt$", "", quantfile)),
              msg="default value is filename w/o extension")
})

local({
    antibody <- "FOO"
    rppa <- RPPA(file.path(path, quantfile),
                 antibody=antibody)
    checkTrue(identical(rppa@antibody, antibody),
              msg="specified value is slotted")
})


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

software <- "bogus"
userMethod <- paste("read", software, sep=".")

checkTrue(!exists(userMethod, mode="function", .GlobalEnv),
          msg="method must not exist")
checkException(RPPA(quantfile,
                    path=path,
                    software=software),
               msg="data import should fail - missing method")

local({
    read.local <- function(file) {

    }

    checkException(RPPA(quantfile,
                        path=path,
                        software="local"),
                   msg="data import should fail - method not in user workspace")
})

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

