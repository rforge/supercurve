###
### TESTFIT.R
###


options(warn=1)
library(SuperCurve)
source("checkFuncs")

## Get a valid RPPA object to get started
path <- system.file("rppaTumorData", package="SuperCurve")
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
## :BUG: Doesn't prevent classes unrelated to FitClass
#checkException(registerModel("bogus", "numeric"),
#               msg="invalid classname - superclass not FitClass")

fp <- RPPAFitParams("Mean.Net", model="logistic", method="nlrob")
summary(fp)
fit <- RPPAFitFromParams(jnk, dsn, fp)

fp <- RPPAFitParams("Mean.Net", model="logistic", method="nls")
summary(fp)
fit <- RPPAFitFromParams(jnk, dsn, fp)

fp <- RPPAFitParams("Mean.Net", model="logistic", method="nlrq")
summary(fp)
fit <- RPPAFitFromParams(jnk, dsn, fp)

