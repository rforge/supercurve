###
### MKRPPACELLDATASET.R
###

local({
    ##
    ## 40 cell lines with 3 antibodies
    ##

    cell40data.dir <- system.file("rppaCellData", package="SuperCurve")
    akt    <- RPPA("AKT.txt", path=cell40data.dir)
    c.erk2 <- RPPA("ERK2no2.txt", path=cell40data.dir)
    ctnnb1 <- RPPA("Bcatenin40breastcelllines.txt", path=cell40data.dir)

    ## The design here does not follow any of our standard shorthands,
    ## since it has interleaved 8-step dilution replicates contained
    ## in a single 4x4 subgrid
    steps <- rep(c(rep(8:5, 2), rep(4:1, 2)), 40) - 4.5
    rep.temp <- factor(paste('Rep', rep(rep(1:2, each=4), 80), sep=''))
    series <- factor(paste(as.character(akt@data$Sample),
                           as.character(rep.temp),
                           sep='.'))
    design40 <- RPPADesign(akt,
                           steps=steps,
                           series=series)
    rm(steps, rep.temp, series)

    dataset <- file.path(system.file("data", package="SuperCurve"),
                         "rppaCell.rda")
    save(akt, c.erk2, ctnnb1, design40, file=dataset)
})

