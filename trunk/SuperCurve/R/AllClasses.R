###
### ALLCLASSES.R
###

##
## Private Classes
##
setClass("FitClass",
         representation("VIRTUAL"))

setOldClass("cobs")
setOldClass("loess")

setClass("cobsFitClass",
         representation("FitClass",
                        model = "cobs", # actually this is of class cobs, but I could not get that to work
                        lambda = "numeric"),
         prototype=prototype(lambda=0))

setClass("loessFitClass",
         representation("FitClass",
                        model="loess"))

setClass("logisticFitClass",
         representation("FitClass",
                        coefficients="numeric"), # alpha, beta, gamma
         prototype=prototype(coefficients=c(alpha=0, beta=0, gamma=0)))

##
## Public Classes
##
setClass("RPPA",
         representation=list(data="data.frame",
                             file="character"))

setClass("RPPADesign",
         representation=list(layout="data.frame",
                             alias="list",
                             sampleMap="character",
                             controls="list"))

setClass("RPPADesignParams",
         representation=list(steps="numeric",
                             series="factor",
                             grouping="character",
                             ordering="character",
                             alias="list",
                             center="logical",
                             controls="list"))

setClass("RPPAFit",
         representation=list(call="call",              # function call used to create the model
                             rppa="RPPA",              # required parameter
                             design="RPPADesign",      # required parameter
                             measure="character",      # required parameter
                             method="character",       # optional parameter
                             trimset="numeric",        # list(lo.intensity, hi.intensity, lo.conc, hi.conc)
                             model="FitClass",         # curve model
                             concentrations="numeric", # main output
                             lower="numeric",          # confidence interval
                             upper="numeric",          # confidence interval
                             conf.width="numeric",     # width of confidence interval
                             intensities="numeric",    # intensities related to series concentrations
                             p.values="numeric",       # measure of goodness of fit per dilution series
                             ss.ratio="numeric",
                             warn="character",
                             version="character"))

setClass("RPPAFitParams",
         representation=list(measure="character",
                             xform="function",
                             method="character",
                             ci="logical",
                             ignoreNegative="logical",
                             trace="logical",
                             verbose="logical",
                             veryVerbose="logical",
                             warnLevel="numeric",
                             trim="logical",
                             model="character"))

setClass("RPPASet",
         representation=list(call="call",               # function call used to create the model
                             version="character",       # package version
                             design="RPPADesign",       # common design for all slides
                             rppas="array",             # vector of RPPAs
                             fitparams="RPPAFitParams", # parameters used for fitting
                             fits="array"))             # set of fits

