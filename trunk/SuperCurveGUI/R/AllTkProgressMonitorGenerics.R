###
### ALLTKPROGRESSMONITORGENERICS.R
###


##
## Accessors
##
if (!isGeneric("progressContinue")) {
    setGeneric("progressContinue",
               function(object, ...) standardGeneric("progressContinue"))
}

##
## Mutators
##
if (!isGeneric("progressAbort<-")) {
    setGeneric("progressAbort<-",
               function(object, ..., value) standardGeneric("progressAbort<-"))
}

