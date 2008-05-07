###
### SUMMARY-METHODS.R
###


##-----------------------------------------------------------------------------
setMethod("summary", "RPPA",
          function(object,
                   ...) {
    cat(paste("An RPPA object loaded from", object@file),
        "\n\n")
    summary(object@data)
})


##-----------------------------------------------------------------------------
setMethod("summary", "RPPADesign",
          function(object,
                   ...) {
    cat("An RPPA design object with controls:", "\n")
    print(unlist(object@controls))
    cat("\n")
    summary(object@layout)
})


##-----------------------------------------------------------------------------
setMethod("summary", "RPPAFit",
          function(object,
                   ...) {
    cat(paste("An RPPAFit object constructed via the function call:",
              "\n",
              as.character(list(object@call))),
        "\n")
})

