###
### MKRPPASINGLESUBGRIDDATASET.R
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
                    antibody=antibody,
                    software="singlesubgrid"),
               envir=environment(makeRPPAs))

        return(varname)
    }


    ##
    ## Superslide data (5 dilution series) with 4 antibodies
    ##

    extdata.dir <- system.file("extdata", package="SuperCurve")
    rawdata.dir <- file.path(extdata.dir, "rppaSingleSubgridData")
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

    slidedesignfile <- "slidedesign.tsv"

    assign(design <- "sdesign",
           RPPADesign(rppa <- get(rppas[1]),
                      designfile=slidedesignfile,
                      path=rawdata.dir))

    ## Update package data directory
    filename <- sprintf("%s.RData", sub("Data$", "", basename(rawdata.dir)))
    dataset <- file.path(system.file("data", package="SuperCurve"), filename)
    save(list=c(rppas, design), file=dataset)
})

