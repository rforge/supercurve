###
### MKRPPATUMORDATASET.R
###


local({
    ##-------------------------------------------------------------------------
    makeRPPAs <- function(antibody,
                          filename,
                          datadir,
                          xform=function(x) tolower(x)) {
        ## Check argumments
        stopifnot(is.character(antibody) && length(antibody) == 1)
        stopifnot(is.character(filename) && length(filename) == 1)
        stopifnot(is.character(datadir) && length(datadir) == 1)
        stopifnot(is.function(xform))

        ## Begin processing
        assign(varname <- make.names(xform(antibody)),
               RPPA(filename, path=datadir),
               envir=environment(makeRPPAs))

        return(varname)
    }


    ##
    ## Tumor data with 3 antibodies
    ##

    instdata.dir <- system.file("rppaTumorData", package="SuperCurve")
    proteinassayfile <- file.path(instdata.dir, "proteinAssay.tsv")
    proteinassay.df <- read.delim(proteinassayfile)

    rppas <- apply(proteinassay.df,
                   1,
                   function(proteinassay, datadir) {
                       makeRPPAs(proteinassay["Antibody"],
                                 proteinassay["Filename"],
                                 datadir)
                   },
                   instdata.dir)

    layoutinfofile <- file.path(instdata.dir, "layoutInfo.tsv")
    layoutinfo.df <- read.delim(layoutinfofile)
    assign(design <- "tDesign",
           RPPADesign(rppa <- get(rppas[1]),
                      grouping="blockSample",
                      alias=layoutinfo.df,
                      center=TRUE,
                      controls=list("neg con",
                                    "pos con")))

    ## Update package data directory
    filename <- paste(sub("Data$", "", basename(instdata.dir)), "rda", sep=".")
    dataset <- file.path(system.file("data", package="SuperCurve"), filename)
    save(list=c(rppas, design), file=dataset)
})

