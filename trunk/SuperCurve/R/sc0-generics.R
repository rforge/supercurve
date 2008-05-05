.onLoad <- function(...) require(methods)

if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

if (!isGeneric("hist"))
  setGeneric("hist", function(x, ...) standardGeneric("hist"))

if (!isGeneric("image"))
  setGeneric("image", function(x, ...) standardGeneric("image"))

if (!isGeneric("coef"))
  setGeneric("coef", function(object, ...) standardGeneric("coef"))
if (!isGeneric("coefficients"))
  setGeneric("coefficients", function(object, ...) standardGeneric("coefficients"))

if (!isGeneric("residuals"))
  setGeneric("residuals", function(object, ...) standardGeneric("residuals"))
if (!isGeneric("resid"))
  setGeneric("resid", function(object, ...) standardGeneric("resid"))

if (!isGeneric("fitted"))
  setGeneric("fitted", function(object, ...) standardGeneric("fitted"))
  
# REQUIRED for R 2.4 or earlier
# PROHIBITED for R 2.5. (Sigh)
#if (!isGeneric("names"))
#  setGeneric("names", function(x) standardGeneric("names"))

if (!isGeneric("summary"))
  setGeneric("summary", function(object, ...) standardGeneric("summary"))

