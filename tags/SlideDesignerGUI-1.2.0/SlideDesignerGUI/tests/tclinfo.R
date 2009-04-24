##
### TCLINFO.R
###

require(tcltk)

## Pretend the following are "exported"
tclinfo <- SlideDesignerGUI:::tclinfo


##-----------------------------------------------------------------------------
test.tclinfo <- function(input, expected) {
    output <- do.call(getFromNamespace("tclinfo", "SlideDesignerGUI"), input)
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

