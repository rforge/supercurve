###
### MKRPPATUMORDATASET.R
###

local({
    ##
    ## Tumor data with 3 antibodies
    ##

    tumordata.dir <- system.file("rppaTumorData", package="SuperCurve")
    erk2 <- RPPA("ERK2.txt", path=tumordata.dir)
    gsk3 <- RPPA("GSK3.txt", path=tumordata.dir)
    jnk  <- RPPA("JNK.txt", path=tumordata.dir)

    tDesign <- RPPADesign(erk2,
                          grouping="blockSample",
                          center=TRUE,
                          controls=list("neg con",
                                        "pos con"))

    dataset <- file.path(system.file("data", package="SuperCurve"),
                         "rppaTumor.rda")
    save(erk2, gsk3, jnk, tDesign, file=dataset)
})

