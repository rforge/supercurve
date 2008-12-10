###
### TESTRPPASET.R
###


options(warn=1)
library(SuperCurve)
source("checkFuncs")

## create project directory in per-session temporary directory
## Cannot successfully test without this...
persessionprojdir <- file.path(tempdir(), "supercurve")
if (!dir.create(persessionprojdir)) {
    stop("cannot create per-session project directory")
}


path <- system.file("rppaTumorData", package="SuperCurve")
designparams <- RPPADesignParams(grouping="blockSample",
                                 center=TRUE,
                                 controls=list("neg con", "pos con"))
fitparams <- RPPAFitParams(measure="Mean.Net",
                           model="logistic",
                           method="nlrob",
                           ignoreNegative=FALSE,
                           warnLevel=-1)

fitset <- RPPASet(path, designparams, fitparams)

###########################
## tests of path

checkException(RPPASet(path=5),
               msg="invalid value should fail")

checkException(RPPASet(path=c("results1", "results2")),
               msg="character vector should fail")

nosuchdir <- file.path(persessionprojdir, "nosuch")
checkException(RPPASet(path=nosuchdir),
               msg="nonexistent directory should fail")

filenotdir <- file.path(persessionprojdir, "somefile")
file.create(filenotdir)
checkException(RPPASet(path=filenotdir),
               msg="not a directory should fail")

emptydir <- file.path(persessionprojdir, "emptydir")
dir.create(emptydir)
checkException(RPPASet(path=emptydir,
                       designparams,
                       fitparams),
               msg="directory without quantification files should fail")

###########################
## tests of designparams and fitparams

checkException(RPPASet(path,
                       designparams=fitparams),
               msg="invalid object should fail")
checkException(RPPASet(path,
                       designparams,
                       fitparams=RPPAFitParams(measure="bogus")),
               msg="fitparams with invalid measure should fail")


###########################
## tests of summary rppaset

outdir <- file.path(persessionprojdir, "results")
dir.create(outdir)
checkException(write.summary(designparams,
                             path=outdir),
               msg="invalid object should fail")

###########################
## tests of summary path

checkException(write.summary(fitset,
                             path=nosuchdir),
               msg="nonexistent output directory should fail")

readonlydir <- file.path(persessionprojdir, "readonly")
dir.create(readonlydir, mode="0555")
checkException(write.summary(fitset,
                             path=readonlydir),
               msg="readonly output directory should fail")

###########################
## tests of summary prefix

checkException(write.summary(fitset,
                             path=outdir,
                             prefix=5),
               msg="invalid value should fail")

checkException(write.summary(fitset,
                             path=outdir,
                             prefix=c("a", "b")),
               msg="character vector should fail")

###########################
## tests of summary graphs

checkException(write.summary(fitset,
                             path=outdir,
                             graphs="yellow"),
               msg="invalid value should fail")

checkException(write.summary(fitset,
                             path=outdir,
                             graphs=c(FALSE, TRUE)),
               msg="logical vector should fail")


###########################
## tests of summary tiffdir

# :NOTE: if unspecified, "tiffdir" is assumed to be sibling of "path"
checkException(write.summary(fitset,
                             path=outdir,
                             graphs=1),
               msg="not a directory should fail")

checkException(write.summary(fitset,
                             path=outdir,
                             graphs=1,
                             tiffdir=pi),
               msg="invalid value should fail")

# :NOTE: numeric "graphs" value silently converted to logical
checkException(write.summary(fitset,
                             path=outdir,
                             graphs=1,
                             tiffdir=c("results1", "results2")),
               msg="character vector should fail")

checkException(write.summary(fitset,
                             path=outdir,
                             graphs=TRUE,
                             tiffdir=nosuchdir),
               msg="nonexistent directory should fail")

checkException(write.summary(fitset,
                             path=outdir,
                             graphs=TRUE,
                             tiffdir=filenotdir),
               msg="not a directory should fail")

## :NOTE: If "tiffdir" contains no image files, error messages from the
## ImageMagick binary appear onscreen, but they are not considered errors
## as far as the R code goes - the return code isn't currently examined
## for external failures. As many TIFF images contain "unrecognized tags",
## this is handy for letting the code continue processing. But when files
## are missing, should this be considered an error?


###########################
## tests of summary (missing ImageMagick binary)

switch(.Platform$OS.type,
       unix={
           # Willing to try test if 'convert' executable is not in /usr/bin
           binary <- system("which convert", intern=TRUE)
           if (!length(grep("/usr/bin", binary, fixed=TRUE))) {

               ##--------------------------------------------------------------
               simulateMissingConvertBinary <- function() {
                   savePATH <- Sys.getenv("PATH")
                   on.exit(Sys.setenv("PATH"=savePATH))
                   Sys.setenv("PATH"="/usr/bin")

                   tryCatch(write.summary(fitset,
                                          path=outdir,
                                          graphs=TRUE,
                                          tiffdir=emptydir),
                            warning=function(w) {
                                errmsg <- paste("(converted from warning)",
                                                w$message)
                                signalCondition(simpleError(errmsg))
                            })
               }

               checkException(simulateMissingConvertBinary(),
                              msg="missing ImageMagick binary should fail")
           } else {
               cat("skipped ImageMagick test",
                   "-",
                   "'convert' binary in /usr/bin",
                   "\n")
           }
       },
       windows={
           cat("skipped ImageMagick test (not implemented)", "\n")
       })


## test one that should work...
write.summary(fitset,
              path=outdir,
              prefix="testing",
              graphs=FALSE)



## If you need to save the results for some reason...
if (FALSE) {
    savedir <- file.path(path.expand("~"), "supercurve", "results")
    if (!file.exists(savedir)) {
        if (!dir.create(savedir, recursive=TRUE)) {
            stop(sprintf("directory %s could not be created",
                         dQuote(savedir)))
        }
    }
    file.copy(list.files(path=outdir, full.names=TRUE),
              savedir,
              overwrite=TRUE)
}

