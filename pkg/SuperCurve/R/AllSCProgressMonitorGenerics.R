###
### $Id$
###


##
## Accessors
##
if (!isGeneric("progressStage")) {
    setGeneric("progressStage",
               function(object, ...) standardGeneric("progressStage"))
}

if (!isGeneric("progressMarquee")) {
    setGeneric("progressMarquee",
               function(object, ...) standardGeneric("progressMarquee"))
}

##
## Mutators
##
if (!isGeneric("progressMarquee<-")) {
    setGeneric("progressMarquee<-",
             function(object, ..., value) standardGeneric("progressMarquee<-"))
}

if (!isGeneric("progressStage<-")) {
    setGeneric("progressStage<-",
               function(object, ..., value) standardGeneric("progressStage<-"))
}

