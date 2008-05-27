library(SuperCurve)

## Get a valid RPPA object to get started
path <- system.file("rppaTumorData", package="SuperCurve")
jnk <- RPPA("JNK.txt", path=path)

## check the 'grouping' variable
## both of these fail
try( dp <- RPPADesignParams(grouping='bogus') )
try( dp <- RPPADesignParams(grouping=1) )

## check the 'ordering variable'
## both of these fail
try( dp <- RPPADesignParams(ordering='bogus') )
try( dp <- RPPADesignParams(ordering=1) )

###########################
## tests of 'controls'

## obvious error in specifying controls. Should we allow
## character vectors as well as lists?
try( dp <- RPPADesignParams(controls=c('neg con', 'pos con')) )

## This is for showing off the plot, where the controls dominate
dp <- RPPADesignParams(grouping='bySample') 
dsn <- RPPADesignFromParams(jnk, dp)
plot(jnk, dsn)

## Putting a vector into a list (instead of making a list) seems to work
## just fine. I really don't see why6 it should....
## Apparently, we were clever enough to include an 'unlist' in the
## .controlVector function. 
dp <- RPPADesignParams(grouping='bySample',
                       controls=list(c('neg con', 'pos con', 'blanks')))
dsn <- RPPADesignFromParams(jnk, dp)
plot(jnk, dsn)

###########################
## tests of 'steps' and 'series'

# why does this work? which value does it think is being set?
dp <- RPPADesignParams(13) # this should probably give an error, but does not
dsn <- RPPADesignFromParams(jnk, dp)

# why does this fail when the previous one works?
try( dp <- RPPADesignParams(1:3) )

# This lets us pass in things of the wrong length, and we only
# discover it later.
dp <- RPPADesignParams(1:3, factor(1:3))
try( dsn <- RPPADesignFromParams(jnk, dp) )

# Why doesn't this crash? 
dp <- RPPADesignParams(1:768, factor(1:768))
dsn <- RPPADesignFromParams(jnk, dp)
image(dsn)
plot(jnk, dsn)

# Here we have 768 series each of step size 1. I bet this will break
# the fits later on...
dp <- RPPADesignParams(rep(1, 768), factor(1:768))
dsn <- RPPADesignFromParams(jnk, dp)
image(dsn)
plot(jnk, dsn)

###########################
## tests of center

# there is no reason to throw these errors
try( dp <- RPPADesignParams(center='x') )
try( dp <- RPPADesignParams(center=1) )
try( dp <- RPPADesignParams(center=c(TRUE, FALSE)) )

###########################
## tests of alias
