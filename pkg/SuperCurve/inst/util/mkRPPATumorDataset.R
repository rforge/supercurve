###
### $Id$
### (Re)creates 'rppaTumor' dataset object found in 'data' directory.
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
               RPPA(filename,
                    path=datadir,
                    antibody=antibody),
               envir=environment(makeRPPAs))

        return(varname)
    }


    ##
    ## Tumor data with 3 antibodies
    ##

    extdata.dir <- system.file("extdata", package="SuperCurve")
    rawdata.dir <- file.path(extdata.dir, "rppaTumorData")
    proteinassayfile <- file.path(rawdata.dir, "proteinAssay.tsv")
    proteinassay.df <- read.delim(proteinassayfile)

    rppas <- apply(proteinassay.df,
                   1,
                   function(proteinassay, datadir) {
                       makeRPPAs(proteinassay["Antibody"],
                                 proteinassay["Filename"],
                                 datadir)
                   },
                   rawdata.dir)

    ## :BUG: last two lines of layout info file look hinky.
    layoutinfofile <- "layoutInfo.tsv"
    slidedesignfile <- "slidedesign.tsv"

    assign(design <- "tDesign",
           RPPADesign(rppa <- get(rppas[1]),
                      grouping="blockSample",
                      center=TRUE,
                      aliasfile=layoutinfofile,
                      designfile=slidedesignfile,
                      path=rawdata.dir))

    ## Update package data directory
    filename <- sprintf("%s.RData", sub("Data$", "", basename(rawdata.dir)))
    dataset <- file.path(system.file("data", package="SuperCurve"), filename)
    save(list=c(rppas, design),
         file=dataset, 
         compress="xz", 
         compression_level=9)
})

