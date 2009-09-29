###
### TESTDESIGN.R
###


options(warn=1)
library(SuperCurve)
source("checkFuncs")

## Get a valid RPPA object to get started
path <- system.file("rppaTumorData", package="SuperCurve")
jnk <- RPPA("JNK.txt", path=path)

###########################
## tests of grouping

checkException(RPPADesignParams(grouping="bogus"),
               msg="invalid character value should fail")
checkException(RPPADesignParams(grouping=1),
               msg="invalid value should fail")

###########################
## tests of ordering

checkException(RPPADesignParams(ordering="bogus"),
               msg="invalid character value should fail")
checkException(RPPADesignParams(ordering=1),
               msg="invalid value should fail")

###########################
## tests of controls

## Specifying controls as character vector is valid (coerced internally)
dp <- RPPADesignParams(controls=c("neg con", "pos con"))
checkIdentical(dp@controls, list("neg con", "pos con"))

## Specifying controls indirectly via slide design file
slidedesignfile="slidedesign.tsv"
dp <- RPPADesignParams(designfile=slidedesignfile, path=path)
checkIdentical(dp@designfile, file.path(path, slidedesignfile))
dsn <- RPPADesignFromParams(jnk, dp)
checkIdentical(dsn@controls, list("neg con", "pos con"))

## This is for showing off the plot, where the controls dominate
dp <- RPPADesignParams(grouping="bySample")
dsn <- RPPADesignFromParams(jnk, dp)
plot(jnk, dsn)

## Putting non-character elements into list should fail
checkException(RPPADesignParams(controls=list(TRUE)),
               msg="logical scalar list component should fail")
checkException(RPPADesignParams(controls=list(pi)),
               msg="numeric scalar list component should fail")
checkException(RPPADesignParams(controls=list(1:3)),
               msg="integer vector list component should fail")
checkException(RPPADesignParams(controls=list(list("foo", "bar"))),
               msg="list as list component should fail")

## Using a vector as list component (instead of making a list) now illegal
checkException(RPPADesignParams(controls=list(c("neg con", "pos con", "blanks"))),
               msg="character vector list component should fail")

dp <- RPPADesignParams(grouping="bySample",
                       controls=list("neg con", "pos con", "blanks"))
dsn <- RPPADesignFromParams(jnk, dp)
plot(jnk, dsn)

###########################
## tests of steps and series

## :KRC: why does this work? which value does it think is being set?
## :PLR: As its length is 1, the actual value passed is irrelevant; does the
## same thing as if the default had been passed. Dumb but harmless.
dp <- RPPADesignParams(steps=13) # should probably give an error, but does not
dsn <- RPPADesignFromParams(jnk, dp)

## Specifying series without steps should fail (length > 1)
checkException(RPPADesignParams(steps=1:3),
               msg="both steps and series must be specified together")

# This lets us pass in things of the wrong length, and we only
# discover it later.
dp <- RPPADesignParams(steps=1:3, series=factor(1:3))
checkException(RPPADesignFromParams(jnk, dp),
               msg="lengths of steps/series must equal prod(dim(RPPA))")

# Why doesn't this crash?
dp <- RPPADesignParams(steps=1:768, series=factor(1:768))
dsn <- RPPADesignFromParams(jnk, dp)
image(dsn)
plot(jnk, dsn)

# Here we have 768 series each of step size 1. I bet this will break
# the fits later on...
dp <- RPPADesignParams(steps=rep(1, 768), series=factor(1:768))
dsn <- RPPADesignFromParams(jnk, dp)
image(dsn)
plot(jnk, dsn)

###########################
## tests of center

RPPADesignParams(center=1) # numeric center value silently converted to logical
checkException(RPPADesignParams(center="x"),
               msg="character value should fail")
checkException(RPPADesignParams(center=c(TRUE, FALSE)),
               msg="logical vector value should fail")

###########################
## tests of alias

dp <- RPPADesignParams(alias=list(foo=1))
checkException(RPPADesignFromParams(jnk, dp),
               msg="too few list components should fail")

dp <- RPPADesignParams(alias=list(foo=1, bar=2))
checkException(RPPADesignFromParams(jnk, dp),
               msg="missing reqd list components should fail")

## :TODO: Lack an example of this in actual usage...
## This passes existing checks here but would probably cause problems later...
dp <- RPPADesignParams(alias=list(Alias=NA, Sample=NA))
RPPADesignFromParams(jnk, dp)



###########################
## test of plot. main extra argument is "measure"

checkException(plot(jnk, dsn, measure="bogus"),
               msg="invalid value - nonexistent colname")

