###
### ALLGENERICS.R
###

###############################
## S3 or non-generics converted to S4

# defined for: RPPA, RPPADesign, RPPAFit
if (!isGeneric("summary")) {
    setGeneric("summary",
               function(object, ...) standardGeneric("summary"))
}

# defined for: RPPA, RPPAFit.
## Why not RPPADesign?
if (!isGeneric("image")) {
    setGeneric("image",
               function(x, ...) standardGeneric("image"))
}

# defined for: RPPAFit
# Why not RPPADEsign, since there is a [expletive] plotDesign function?
if (!isGeneric("plot")) {
    setGeneric("plot",
               function(x, y, ...) standardGeneric("plot"))
}

# defined for: RPPAFit
if (!isGeneric("hist")) {
    setGeneric("hist",
               function(x, ...) standardGeneric("hist"))
}

# defined for: nothing. so why is it here?
if (!isGeneric("coef")) {
    setGeneric("coef",
               function(object, ...) standardGeneric("coef"))
}

# defined for: nothing. so why is it here?
if (!isGeneric("coefficients")) {
    setGeneric("coefficients",
               function(object, ...) standardGeneric("coefficients"))
}

# defined for: LoessFitClass, CobsFitClass, LogisticFitClass, RPPAFit
if (!isGeneric("fitted")) {
    setGeneric("fitted",
               function(object, ...) standardGeneric("fitted"))
}
  
# defined for: RPPAFit
if (!isGeneric("resid")) {
    setGeneric("resid",
               function(object, ...) standardGeneric("resid"))
}

# defined for: RPPAFit
if (!isGeneric("residuals")) {
    setGeneric("residuals",
               function(object, ...) standardGeneric("residuals"))
}

###############################
## Brand New Generic Functions

# defined for: FitClass, LoessFitClass, CobsFitClass, LogisticFitClass
if (!isGeneric("trimConc")) {
    setGeneric("trimConc",
               function(object, ...) standardGeneric("trimConc"))
}

# defined for: FitClass, LoessFitClass, CobsFitClass, LogisticFitClass
if (!isGeneric("fitSeries")) {
    setGeneric("fitSeries",
               function(object, ...) standardGeneric("fitSeries"))
}

# defined for: FitClass, LoessFitClass, CobsFitClass, LogisticFitClass
if (!isGeneric("fitSlide")) {
    setGeneric("fitSlide",
               function(object, ...) standardGeneric("fitSlide"))
}

# defined for: RPPASet
## if there is only one class that buses it, why is it generic?
if (!isGeneric("fitslot")) {
    setGeneric("fitslot",
               function(object, ...) standardGeneric("fitslot"))
}

