###
### ALLGENERICS.R
###

if (!isGeneric("coef")) {
    setGeneric("coef",
               function(object, ...) standardGeneric("coef"))
}

if (!isGeneric("coefficients")) {
    setGeneric("coefficients",
               function(object, ...) standardGeneric("coefficients"))
}

if (!isGeneric("fitSeries")) {
    setGeneric("fitSeries",
               function(object, ...) standardGeneric("fitSeries"))
}

if (!isGeneric("fitSlide")) {
    setGeneric("fitSlide",
               function(object, ...) standardGeneric("fitSlide"))
}

if (!isGeneric("fitslot")) {
    setGeneric("fitslot",
               function(object, ...) standardGeneric("fitslot"))
}

if (!isGeneric("fitted")) {
    setGeneric("fitted",
               function(object, ...) standardGeneric("fitted"))
}
  
if (!isGeneric("hist")) {
    setGeneric("hist",
               function(x, ...) standardGeneric("hist"))
}

if (!isGeneric("image")) {
    setGeneric("image",
               function(x, ...) standardGeneric("image"))
}

if (!isGeneric("plot")) {
    setGeneric("plot",
               function(x, y, ...) standardGeneric("plot"))
}

if (!isGeneric("resid")) {
    setGeneric("resid",
               function(object, ...) standardGeneric("resid"))
}

if (!isGeneric("residuals")) {
    setGeneric("residuals",
               function(object, ...) standardGeneric("residuals"))
}

if (!isGeneric("summary")) {
    setGeneric("summary",
               function(object, ...) standardGeneric("summary"))
}

if (!isGeneric("trimConc")) {
    setGeneric("trimConc",
               function(object, ...) standardGeneric("trimConc"))
}

