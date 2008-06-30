###
### MKRPPATRIPLEDATASET.R
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
    ## Triple data with 5 antibodies
    ##

    instdata.dir <- system.file("rppaTripleData", package="SuperCurve")
    ## :BUG: Assay file does not reference 'cas3_3600.txt' so only 4 objects...
    proteinassayfile <- file.path(instdata.dir, "proteinAssay.tsv")
    proteinassay.df <- read.delim(proteinassayfile)

    rppas <- apply(proteinassay.df,
                   1,
                   function(proteinassay, datadir) {
                       makeRPPAs(proteinassay[1],
                                 proteinassay[2],
                                 datadir)
                   },
                   instdata.dir)

    assign(design <- "tripledesign",
           RPPADesign(rppa <- get(rppas[1]),
                      grouping="byRow",
                      controls=list("Buffer",
                                    "Blank")))

    ## Update package data directory
    filename <- paste(sub("Data$", "", basename(instdata.dir)), "rda", sep=".")
    dataset <- file.path(system.file("data", package="SuperCurve"), filename)
    save(list=c(rppas, design), file=dataset)
})

