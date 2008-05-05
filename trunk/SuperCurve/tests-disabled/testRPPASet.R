library(SuperCurve)

# path <- "Y:/Private/LysateArray/SuperCurveVersions/rppaTumorData"  
setwd("../SuperCurve")
path <- "rppaTumorData"                                              
designparams <- RPPADesignParams(grouping="blockSample", center=TRUE, controls=list("neg con", "pos con"))
fitparams <- RPPAFitParams(measure = "Mean.Net", ignoreNegative=FALSE, method='nls', warnLevel=-1)                       
fitset <- RPPAFitDir(path, designparams, fitparams)
write.summary(fitset, file = 'supercurve', path = path, normalize = 'median', graphs=FALSE)