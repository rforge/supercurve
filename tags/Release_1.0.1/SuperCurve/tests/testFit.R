library(SuperCurve)

## Get a valid RPPA object to get started
path <- system.file("rppaTumorData", package="SuperCurve")
jnk <- RPPA("JNK.txt", path=path)
## build the correct design
dsn <- RPPADesign(jnk,
                  grouping="blockSample",
                  center=TRUE,
                  controls=list("neg con", "pos con"))

try( RPPAFitParams() ) # missing required argument

fp <- RPPAFitParams("Mean.Net")
summary(fp)


fp <- RPPAFitParams("Mean.Net", model="bogus") # cannot catch this yet
summary(fp)
try(fit <- RPPAFitFromParams(jnk, dsn, fp))    # but find the bad argument here

try( RPPAFitParams("Mean.Net", method="bogus") ) # bad argument

try( fp <- RPPAFitParams("Mean.Net", method="nlrob",
                         model="bogus") ) # cannot catch this yet
summary(fp)
try(fit <- RPPAFitFromParams(jnk, dsn, fp))    # but find the bad argument here


fp <- RPPAFitParams("Mean.Net", model="logistic", method="nlrob")
summary(fp)
fit <- RPPAFitFromParams(jnk, dsn, fp)

fp <- RPPAFitParams("Mean.Net", model="logistic", method="nls")
summary(fp)
fit <- RPPAFitFromParams(jnk, dsn, fp)

fp <- RPPAFitParams("Mean.Net", model="logistic", method="nlrq")
summary(fp)
fit <- RPPAFitFromParams(jnk, dsn, fp)
