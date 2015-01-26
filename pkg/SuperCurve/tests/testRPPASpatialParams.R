###
### $Id$
###


options(warn=1)
library(SuperCurve)
source("checkFuncs")

###########################
## tests of cutoff

checkException(RPPASpatialParams(cutoff="bogus"),
               msg="invalid character value should fail")
checkException(RPPASpatialParams(cutoff=-1),
               msg="invalid value (too small) should fail")
checkException(RPPASpatialParams(cutoff=2),
               msg="invalid value (too large) should fail")
checkException(RPPASpatialParams(cutoff=1:10),
               msg="numeric vector should fail")

###########################
## tests of k

checkException(RPPASpatialParams(k="bogus"),
               msg="invalid character value should fail")
checkException(RPPASpatialParams(k=Inf),
               msg="invalid value (infinite) should fail")
checkException(RPPASpatialParams(k=1),
               msg="invalid value (too small) should fail")
checkException(RPPASpatialParams(k=1:10),
               msg="numeric vector should fail")

###########################
## tests of gamma

checkException(RPPASpatialParams(gamma="bogus"),
               msg="invalid character value should fail")
checkException(RPPASpatialParams(gamma=Inf),
               msg="invalid value (infinite) should fail")
checkException(RPPASpatialParams(gamma=-1),
               msg="invalid value (too small) should fail")
checkException(RPPASpatialParams(gamma=3),
               msg="invalid value (too large) should fail")
checkException(RPPASpatialParams(gamma=1:10),
               msg="numeric vector should fail")

###########################
## tests of plotSurface

checkException(RPPASpatialParams(plotSurface="bogus"),
               msg="invalid character value should fail")
checkException(RPPASpatialParams(plotSurface=1),
               msg="invalid logical value should fail")
checkException(RPPASpatialParams(plotSurface=c(TRUE, FALSE)),
               msg="logical vector should fail")


