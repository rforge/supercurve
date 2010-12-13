###
### MKRPPACELLDATASET.R
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
    ## 40 cell lines with 3 antibodies
    ##

    extdata.dir <- system.file("extdata", package="SuperCurve")
    rawdata.dir <- file.path(extdata.dir, "rppaCellData")
    proteinassayfile <- file.path(rawdata.dir, "proteinAssay.tsv")
    proteinassay.df <- read.delim(proteinassayfile)

    rppas <- apply(proteinassay.df,
                   1,
                   function(proteinassay, datadir) {
                       makeRPPAs(proteinassay["Antibody"],
                                 proteinassay["Filename"],
                                 datadir,
                                 xform=function(varname) {
                                     ## Distinguish from tumor data variable
                                     if (varname == "ERK2") {
                                         varname <- "c.erk2"
                                     }
                                     tolower(varname)
                                 })
                   },
                   rawdata.dir)

    ## The design here does not follow any of our standard shorthands,
    ## since it has interleaved 8-step dilution replicates contained
    ## in a single 4x4 subgrid
    rppa <- get(rppas[1])
    steps <- rep(c(rep(8:5, 2), rep(4:1, 2)), 40) - 4.5
    rep.temp <- factor(paste("Rep", rep(rep(1:2, each=4), 80), sep=""))
    series <- factor(paste(as.character(rppa@data$Sample),
                           as.character(rep.temp),
                           sep="."))

    ## :TODO: Missing 'slidedesign.tsv' and 'layoutInfo.tsv' files
    assign(design <- "design40",
           RPPADesign(rppa,
                      steps=steps,
                      series=series))

    ## Update package data directory
    filename <- sprintf("%s.RData", sub("Data$", "", basename(rawdata.dir)))
    dataset <- file.path(system.file("data", package="SuperCurve"), filename)
    save(list=c(rppas, design), file=dataset)
})

