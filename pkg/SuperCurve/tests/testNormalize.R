###
### $Id$
###


options(warn=1)
set.seed(65789)

library(SuperCurve)
source("checkFuncs")


############################
## tests of normalize()
antibodies <- c("WIBBLE",
                "WOBBLE",
                "WUBBLE",
                "PIPPO", 
                "TOTO",
                "TITI",
                "TATA",
                "NORF")
concs <- matrix(rnorm(1024),
                ncol=length(antibodies),
                dimnames=list(samples=NULL, antibodies=antibodies))

## Verify preconditions
checkException(normalize(concs, calc.medians=FALSE, sweep.cols=TRUE),
               msg="sweep without calculating medians should fail")

checkException(normalize(concs, method="vs", sweep.cols=FALSE),
               msg="'vs' method expecting swept columns should fail")

## Normalize using sample median
normconcs.median <- normalize(concs, method="median")
str(normconcs.median)
normconcs.median[1:5, ]

## Normalize using housekeeping antibodies
housekeeping <- c("PIPPO",
                  "NORF")
normconcs.house <- normalize(concs, method="house", antibodies=housekeeping)
str(normconcs.house)
normconcs.house[1:5, ]

## Normalize using variable slope
normconcs.vs <- normalize(concs, method="vs")
str(normconcs.vs)
normconcs.vs[1:5, ]

## Normalize using Tukey's median polish
normconcs.medpolish <- normalize(concs, method="medpolish", calc.medians=FALSE)
str(normconcs.medpolish)
normconcs.medpolish[1:5, ]

