###
### RPPA.R
###


## :TODO: Change API to accept file object, replacing filename/path args
## Logically, it really should be nothing more than a couple lines, with
## a file object as the generator's sole argument.

##-----------------------------------------------------------------------------
## A function to read in the Microvigen .txt files to generate an RPPA object.
RPPA <- function(filename, path='.', blanks=NULL) {

    ## :TODO: Move code to read quantification file to external method
    ## from redesign
    pathname <- file.path(path, filename)

    ## MicroVigene introducing an extra header line in later versions of file
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

    new("RPPA",
        data=quant.df,
        file=filename)
}



if (FALSE) {
  path <- "../inst/rppaTumorData"
  erk2 <- RPPA("ERK2.txt", path=path)
  summary(erk2)
  image(erk2)
  image(erk2, colorbar=TRUE)
  image(erk2, "Vol.Bkg", colorbar=TRUE)
  rm(path, erk2)
}

