##
### TCLINFO.R
###

library(tclish)


##-----------------------------------------------------------------------------
test.tclinfo <- function(input, expected) {
    output <- do.call(getFromNamespace("tclinfo", "tclish"), input)
    identical(tclvalue(output), expected)
}


## Run tests
tclinfo.hostname.expected <- tolower(Sys.info()[["nodename"]])
test.tclinfo(list("hostname"),
             tclinfo.hostname.expected)

tclinfo.patchlevel <- tclvalue(tclinfo("patchlevel"))
tclinfo.version.expected <- sub("\\.[[:digit:]]*$", "", tclinfo.patchlevel)
test.tclinfo(list("tclversion"),
             tclinfo.version.expected)

