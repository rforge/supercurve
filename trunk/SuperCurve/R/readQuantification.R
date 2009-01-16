###
### READQUANTIFICATION.R
###


##-----------------------------------------------------------------------------
## Reads quantification datafiles and returns data frame containing
## the desired information
readQuantification <- function(file, software) {
    ## Check arguments
    if (!inherits(file, "connection")) {
        stop(sprintf("argument %s must be connection",
                     sQuote("file")))
    } else if (!isOpen(file, "r")) {
        stop(sprintf("connection %s not open for read",
                     dQuote(summary(file)$description)))
    }

    if (!is.character(software)) {
        stop(sprintf("argument %s must be character",
                     sQuote("software")))
    } else if (!(length(software) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("software")))
    } else if (!nzchar(software)) {
        stop(sprintf("argument %s must not be empty string",
                     sQuote("software")))
    }

    ## Begin processing
    methodName <- paste("read", software, sep=".")
    readMethod <- if (exists(methodName,
                             mode="function",
                             globalenv())) {
                      get(methodName,
                          mode="function",
                          globalenv())
                  } else if (exists(methodName,
                                    mode="function",
                                    asNamespace("SuperCurve"))) {
                      get(methodName,
                          mode="function",
                          asNamespace("SuperCurve"))
                  } else {
                      warning(sprintf("no user-provided method named %s found",
                                      sQuote(methodName)))
                      NULL
                  }

    ## The datafile format for each row must contain the four values
    ## needed to specify the logical location of a spot on an array,
    ## the unique identifier of the sample at that location, and an
    ## unspecified numeric measurement. Additional columns may be present.

    quant.df <- if (is.function(readMethod)) {
                    readMethod(file)
                }

    if (is.null(quant.df)) {
        pathname <- summary(file)$description
        stop(sprintf("cannot import data from file %s",
                     dQuote(pathname)))
    }

    reqdColnames <- c("Main.Row",
                      "Main.Col",
                      "Sub.Row",
                      "Sub.Col",
                      "Sample")

    ## Ensure minimum number of columns
    nreqdColumns <- length(reqdColnames) + 1
    if (!(ncol(quant.df) >= nreqdColumns)) {
        stop("not enough columns in quantification datafile")
    }

    ## Ensure required columns exist
    found <- reqdColnames %in% colnames(quant.df)
    if (!all(found)) {
        missingColumns <- reqdColnames[!found]
        stop(sprintf(ngettext(length(missingColumns),
                              "missing required column %s",
                              "missing required columns %s"),
                     paste(dQuote(missingColumns), collapse = ", ")))
    }

    ## Ensure number of rows matches slide dimensions
    nexpectedRows <- prod(.dimOfLayout(quant.df))
    if (nrow(quant.df) != nexpectedRows) {
        stop(sprintf("number of rows (%d) differs from expected (%d)",
                     nrow(quant.df),
                     nexpectedRows))
    }

    ## Specify datatype of location columns
    storage.mode(quant.df$Main.Row) <- "integer"
    storage.mode(quant.df$Main.Col) <- "integer"
    storage.mode(quant.df$Sub.Row) <- "integer"
    storage.mode(quant.df$Sub.Col) <- "integer"

    return(quant.df)
}


##-----------------------------------------------------------------------------
## Reads MicroVigene text datafile
read.microvigene <- function(file) {
    ## Check arguments
    stopifnot(inherits(file, "connection"))

    isMicroVigene <- function(pathname) {
        line <- readLines(pathname, n=1)
        isTRUE(grep("MicroVigene", line, fixed=TRUE) == 1)
    }

    getMicroVigeneVersion <- function(pathname) {
        line <- readLines(pathname, n=1, ok=FALSE)
        mv.version <- as.numeric(strsplit(line, "[:blank:]")[[1]][3])
    }

    getTimestamp <- function(pathname) {
        line <- readLines(pathname, n=3, ok=FALSE)[3]
        timestamp <- as.POSIXct(line, format="%m/%d/%Y %I:%M:%S %p")
    }

    getNumHeaderLines <- function(mv.version) {
        ## Vendor introduced extra header line in later versions of file format
        numHeaderLines <- if (mv.version < 2900) 4 else 5
    }

    ## Begin processing
    pathname <- summary(file)$description

    ## Check if this is really a MicroVigene datafile
    if (!isMicroVigene(pathname)) {
        stop(sprintf("file %s is not a MicroVigene datafile",
                     dQuote(pathname)))
    }

    ## Read data from file
    mvvers <- getMicroVigeneVersion(pathname)
    mvdata.df <- read.delim(file,
                            quote="",
                            row.names=NULL,
                            skip=getNumHeaderLines(mvvers))

    ## Eliminate spurious column caused by too many tab characters
    if ("X" %in% colnames(mvdata.df)) {
        mvdata.df[["X"]] <- NULL
    }

    ## Standardize column names
    colnames(mvdata.df)[colnames(mvdata.df) %in% "GeneID"] <- "Sample"
    colnames(mvdata.df) <- .capwords(make.names(colnames(mvdata.df),
                                                allow_=FALSE))

    ## Annotate data frame with metadata
    attr(mvdata.df, "software")  <- "microvigene"
    attr(mvdata.df, "version")   <- mvvers
    attr(mvdata.df, "timestamp") <- getTimestamp(pathname)

    return(mvdata.df)
}

