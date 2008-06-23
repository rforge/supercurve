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

# Why is there still no command that works easily across
# operating/display systems to open a new plotting window
# of fixed width?
devName <- getOption("device")
if (exists(devName, .GlobalEnv)) {
  devFun <- get(devName, .GlobalEnv)
} else if (exists(devName, asNamespace("grDevices"))) {
  devFun <- get(devName, asNamespace("grDevices"))
} else {
  devFun <- pdf
}
devFun(width=1.2, height=6)
try( image(akt) )
try( image(akt, colorbar=TRUE) )
dev.off()

