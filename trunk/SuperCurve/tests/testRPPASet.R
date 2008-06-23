###
### TESTRPPASET.R
###


options(warn=1)
library(SuperCurve)

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

write.summary(fitset,
              path="results",
              prefix="testing",
              graphs=FALSE)

