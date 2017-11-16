###
### $Id$
###


options(warn=1)
options(show.nls.convergence=TRUE)
Sys.setlocale("LC_COLLATE", "C")   # turn off locale-specific sorting, usually

library(SuperCurve)
library(robustbase)
library(boot)
source("checkFuncs")

extdata.dir <- system.file("extdata", package="SuperCurveSampleData")

## Get a valid RPPA object to get started
path <- file.path(extdata.dir, "rppaTumorData")
jnk <- RPPA("JNK.txt", path=path)

## build the correct design
dsn <- RPPADesign(jnk,
                  grouping="blockSample",
                  center=TRUE,
                  controls=list("neg con", "pos con"))

###########################
## tests of measure

checkException(RPPAFitParams(),
               msg="missing argument")

fp <- RPPAFitParams("bogus") # cannot catch until data.frame available
checkException(RPPAFitFromParams(jnk, dsn, fp),
               msg="invalid measurement value")

fp <- RPPAFitParams(measure="Mean.Net")
summary(fp)

###########################
## tests of model and method

fp <- RPPAFitParams("Mean.Net", model="bogus") # cannot catch this yet
summary(fp)
checkException(RPPAFitFromParams(jnk, dsn, fp),  # but find bad argument here
               msg="unregistered fit class as model should fail")

checkException(RPPAFitParams("Mean.Net", method="bogus"),
               msg="invalid method should fail")

fp <- RPPAFitParams("Mean.Net", method="nlrob", model="bogus") # cannot catch this yet
summary(fp)
checkException(RPPAFitFromParams(jnk, dsn, fp),    # but find bad argument here
               msg="unregistered fit class as model should fail")

checkException(registerModel("bogus", 5),
               msg="invalid classname should fail")
checkException(registerModel("bogus", "numeric"),
               msg="invalid classname - superclass not FitClass")

## Generate matrix[models, methods] of fits
fitmodels <- SuperCurve:::getRegisteredModelKeys()
fitmethods <- eval(formals(RPPAFitParams)$method)
fits <- sapply(fitmethods,
               function(fitmethod) {
                   sapply(fitmodels,
                          function(fitmodel, fitmethod) {
                              message(sprintf("*** model: %s, method: %s",
                                              fitmodel, fitmethod))
                              fp <- RPPAFitParams("Mean.Net",
                                                  model=fitmodel,
                                                  method=fitmethod)
                              RPPAFitFromParams(jnk, dsn, fp)
                          },
                          fitmethod=fitmethod)
               })

