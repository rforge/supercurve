###
### $Id$
###


##
## Accessors
##
if (!isGeneric("elapsed")) {
    setGeneric("elapsed",
               function(object, ...) standardGeneric("elapsed"))
}

if (!isGeneric("progressLabel")) {
    setGeneric("progressLabel",
               function(object, ...) standardGeneric("progressLabel"))
}

if (!isGeneric("progressValue")) {
    setGeneric("progressValue",
               function(object, ...) standardGeneric("progressValue"))
}

if (!isGeneric("progressMaximum")) {
    setGeneric("progressMaximum",
               function(object, ...) standardGeneric("progressMaximum"))
}

if (!isGeneric("progressMinimum")) {
    setGeneric("progressMinimum",
               function(object, ...) standardGeneric("progressMinimum"))
}

if (!isGeneric("progressError")) {
    setGeneric("progressError",
               function(object, ...) standardGeneric("progressError"))
}

if (!isGeneric("progressDone")) {
    setGeneric("progressDone",
               function(object, ...) standardGeneric("progressDone"))
}

##
## Mutators
##
if (!isGeneric("progressLabel<-")) {
    setGeneric("progressLabel<-",
               function(object, ..., value) standardGeneric("progressLabel<-"))
}

if (!isGeneric("progressValue<-")) {
    setGeneric("progressValue<-",
               function(object, ..., value) standardGeneric("progressValue<-"))
}

if (!isGeneric("progressMaximum<-")) {
    setGeneric("progressMaximum<-",
             function(object, ..., value) standardGeneric("progressMaximum<-"))
}

if (!isGeneric("progressMinimum<-")) {
    setGeneric("progressMinimum<-",
             function(object, ..., value) standardGeneric("progressMinimum<-"))
}

if (!isGeneric("progressError<-")) {
    setGeneric("progressError<-",
               function(object, ..., value) standardGeneric("progressError<-"))
}

if (!isGeneric("progressDone<-")) {
    setGeneric("progressDone<-",
               function(object, ..., value) standardGeneric("progressDone<-"))
}

