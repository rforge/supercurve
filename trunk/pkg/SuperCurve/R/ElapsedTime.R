###
### ELAPSEDTIME.R
###

require(methods)
options(warn=1)


##
## Class
##

##=============================================================================
setClass("ElapsedTime",
         representation(start="numeric"),           ## start time
         prototype(start=0))


##-----------------------------------------------------------------------------
is.ElapsedTime <- function(x) {
    extends(class(x), "ElapsedTime")
}


##-----------------------------------------------------------------------------
## Generates an ElapsedTime object.
ElapsedTime <- function() {
    new("ElapsedTime")
}


##
## Methods
##


##-----------------------------------------------------------------------------
setMethod("initialize",
    signature(.Object="ElapsedTime"),
    function(.Object, ...) {
        .Object@start <- as.numeric(proc.time()["elapsed"])
        callNextMethod(.Object, ...)
    })


##-----------------------------------------------------------------------------
setMethod("elapsed",
    signature(object="ElapsedTime"),
    function(object,
             units=c("auto",
                     "secs",
                     "mins",
                     "hours",
                     "days")) {
        secs <- as.numeric(proc.time()["elapsed"]) - object@start

        units <- match.arg(units)
        if (units == "auto") {
                 if (secs < 60)    units <- "secs"
            else if (secs < 3600)  units <- "mins"
            else if (secs < 86400) units <- "hours"
            else                   units <- "days"
        }
        switch(units,
               "secs"  = structure(secs,       units=units, class="difftime"),
               "mins"  = structure(secs/60,    units=units, class="difftime"),
               "hours" = structure(secs/3600,  units=units, class="difftime"),
               "days"  = structure(secs/86400, units=units, class="difftime"))
    })

