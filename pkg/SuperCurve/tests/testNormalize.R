###
### $Id$
###


options(warn=1)
library(SuperCurve)
source("checkFuncs")

###########################
## tests of method

checkException(RPPANormalizationParams(method="bogus"),
               msg="invalid character value should fail")
checkException(RPPANormalizationParams(method=""),
               msg="invalid value (empty string) should fail")
checkException(RPPANormalizationParams(method=c("foo", "bar")),
               msg="character vector should fail")

###########################
## tests of arglist

checkException(RPPANormalizationParams(method="vs",
                                       arglist="bogus"),
               msg="invalid character value should fail")
checkException(RPPANormalizationParams(method="vs",
                                       arglist=list(TRUE, FALSE)),
               msg="arglist with unnamed components should fail")

##
##
checkTrue(is.RPPANormalizationParams(
               RPPANormalizationParams(method="vs",
                                       arglist=NULL)),
          msg="NULL arglist should succeed")
checkTrue(is.RPPANormalizationParams(
               RPPANormalizationParams(method="house",
                                       arglist=list(antibodies=c("FOO",
                                                                 "PLUGH")))),
          msg="arglist with named components should succeed")

