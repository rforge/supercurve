###
### RPPA.R
###


##=============================================================================
setClass("RPPA",
         representation=list(data="data.frame",
                             file="character"))


## :TODO: Change API to accept file object, replacing filename/path args
## Logically, it really should be nothing more than a couple lines, with
## a file object as the generator's sole argument.

## :TODO: The blanks argument belongeth not here.

##-----------------------------------------------------------------------------
## Generates an RPPA object from a Microvigene .txt file.
RPPA <- function(filename, path='.', blanks=NULL) {
    ## Check arguments
    if (!is.character(filename)) {
        stop(sprintf("argument %s must be character",
                     sQuote("filename")))
    } else if (!(length(filename) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("filename")))
    }

    if (!is.character(path)) {
        stop(sprintf("argument %s must be character",
                     sQuote("path")))
    } else if (!(length(path) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("path")))
    } else if (!file.exists(path)) {
        ## :TODO: Add code to verify directory exists
        stop(sprintf("directory %s does not exist",
                     dQuote(path)))
    }
    ## :TODO: Add checks for 'blanks' argument

    ## :TODO: Move code to read quantification file to external method
    ## from redesign
    pathname <- file.path(path, filename)
    if (!file.exists(pathname)) {
        stop(sprintf("file %s does not exist",
                     dQuote(pathname)))
    }

    ## MicroVigene introducing an extra header line in later versions of file
    ## :TODO: Something more sensible than hardcoding the two choices of the
    ## number of header lines....
    ## By the way, what happens if someone uses something other than
    ## MicroVigene (like ArrayVision or SPOT), when this whole thing breaks.
    get.num.header.lines <- function(filename) {
        line <- readLines(filename, n=1)
        mv.version <- as.numeric(strsplit(line, "[:blank:]")[[1]][3])
        num.header.lines <- if (mv.version < 2900) 4 else 5
        return(num.header.lines)
    }

    skip.lines <- get.num.header.lines(pathname)
    quant.df <- read.delim(pathname,
                           quote='',
                           row.names=NULL,
                           skip=skip.lines)

    ## :TODO: Replace hardcoded substitutions below with algorithmic equivalent
    ## from redesign
    quant.df <- quant.df[, 1:(ncol(quant.df)-1)]
    newNames <- dimnames(quant.df)[[2]]
    newNames <- sub("GeneID",  "Sample",  newNames)
    newNames <- sub("mean_",   "Mean.",   newNames)
    newNames <- sub("vol_",    "Vol.",    newNames)
    newNames <- sub("median_", "Median.", newNames)
    newNames <- sub("net",     "Net",     newNames)
    newNames <- sub("total",   "Total",   newNames)
    newNames <- sub("bkg",     "Bkg",     newNames)
    newNames <- sub("dust",    "Dust",    newNames)
    dimnames(quant.df)[[2]] <- newNames

    ## :TBD: Couldn't this be externalized? Perhaps add method to do exactly
    ## same processing, removing need for extra argument...

    #########
    # Several sets of slides have large numbers of blanks which we want to
    # exclude from the model fitting. The following procedure treats the
    # blanks as controls, which realizes this purpose. Certainly, the sample
    # name called 'control' must appear in the argument "control" in
    # function "RPPADesignParams".
    ######
    if (!is.null(blanks)) {
        quant.df$Sample <- as.character(quant.df$Sample)
        quant.df$Sample[blanks] <- 'control'
        quant.df$Sample <- as.factor(quant.df$Sample)
    }

    ## :TBD: Add path to object? Convert to canonical format prior to doing so?
    ## :KRC: No, I just want the filename, not the full path. That is why the
    ## arguments were separated to start with.
    ## :PLR: Question was whether an additional slot would contain directory
    ## info, not a merge with existing field.

    ## Create new class
    new("RPPA",
        data=quant.df,
        file=filename)
}


##-----------------------------------------------------------------------------
setMethod("summary", "RPPA",
          function(object,
                   ...) {
    cat(sprintf("An RPPA object loaded from %s", dQuote(object@file)),
        "\n\n")
    summary(object@data)
})


##-----------------------------------------------------------------------------
setMethod("image", "RPPA",
          function(x,
                   measure="Mean.Net",
                   main=measure,
                   colorbar=FALSE,
                   col=terrain.colors(256),
                   ...) {
    ## Check arguments
    if (!is.character(measure)) {
        stop(sprintf("argument %s must be character",
                     sQuote("measure")))
    } else if (!(length(measure) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("measure")))
    }

    if (!is.character(main)) {
        stop(sprintf("argument %s must be character",
                     sQuote("main")))
    } else if (!(length(main) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("main")))
    }

    if (!is.logical(colorbar)) {
        stop(sprintf("argument %s must be logical",
                     sQuote("colorbar")))
    } else if (!(length(colorbar) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("colorbar")))
    }

    ## Begin processing
    data.df <- x@data
    my <- max(data.df$Main.Row) * max(data.df$Sub.Row)
    mx <- max(data.df$Main.Col) * max(data.df$Sub.Col)
    yspot <- 1+my-(max(data.df$Sub.Row)*(data.df$Main.Row-1) + data.df$Sub.Row)
    xspot <- max(data.df$Sub.Col)*(data.df$Main.Col-1) + data.df$Sub.Col
    geo <- tapply(data.df[, measure],
                  list(xspot, yspot),
                  mean)
    if (colorbar) {
        ## Get the size of the plotting region in relative units
        startPlt <- par()$plt

        ## We're only going to partition things on the x-axis, so only
        ## the first 2 coordinates are of interest. Define the boundaries
        ## for the two panels so that a 10/1 width ratio is attained.
        imagePlt    <- startPlt
        colorbarPlt <- startPlt
        startWidth  <- startPlt[2] - startPlt[1]

        imagePlt[2]    <- startPlt[1] + (10/12)*startWidth
        colorbarPlt[1] <- startPlt[2] - ( 1/12)*startWidth

        ## Draw the colorbar
        ## :TODO: Figure out how to set margins so it works in small windows...
        par(plt=colorbarPlt)
        image(1,
              seq(min(geo, na.rm=TRUE),
                  max(geo, na.rm=TRUE),
                  length=256),
              matrix(1:256, nrow=1),
              col=col,
              xaxt="n",
              xlab="",
              yaxt="n",
              ylab="")
        axis(4) # Put labeling at right
        box()

        ## Set things up to draw main image and revert back for next figure
        par(plt=imagePlt, new=TRUE)
        on.exit(par(plt=startPlt))
    }
    image(1:mx,
          1:my,
          geo,
          col=col,
          main=main,
          ...)
    abline(h=(0.5 + seq(0, my, length=1+max(data.df$Main.Row))))
    abline(v=(0.5 + seq(0, mx, length=1+max(data.df$Main.Col))))
    invisible(x)
})


##
##
if (FALSE) {
  source("AllGenerics.R")
  source("sc1-rppa.R")
  path <- "../inst/rppaTumorData"
  erk2 <- RPPA("ERK2.txt", path=path)
  summary(erk2)
  image(erk2)
  image(erk2, colorbar=TRUE)
  image(erk2, "Vol.Bkg", colorbar=TRUE)
  rm(path, erk2)
}

