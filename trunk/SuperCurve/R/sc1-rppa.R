setClass("RPPA",
         representation = list(
           data="data.frame",
           file="character"
           ))

RPPA.orig <- function(filename, path='.') {
  v <- readLines(file.path(path, filename), n=1)
  Mversion <- as.numeric(strsplit(v,"[:blank:]")[[1]][3])
  if (Mversion < 2900) { # For Microvigene version 2.0 and higher, skip first 5 lines
	  skip.lines <- 4
  } else {
	  skip.lines <- 5
  }
  temp <- read.table(file.path(path, filename), fill=TRUE,
                     header=TRUE, row.names=NULL, sep='\t',
                     quote='', comment.char='', skip=skip.lines)
  temp <- temp[,1:(ncol(temp)-1)]
  newNames <- dimnames(temp)[[2]]
  newNames <- sub("GeneID",   "Sample",  newNames)
  newNames <- sub("mean\_",   "Mean.",   newNames)
  newNames <- sub("vol\_",    "Vol.",    newNames)
  newNames <- sub("median\_", "Median.", newNames)
  newNames <- sub("net",      "Net",     newNames)
  newNames <- sub("total",    "Total",   newNames)
  newNames <- sub("bkg",      "Bkg",     newNames)
  newNames <- sub("dust",     "Dust",    newNames)
  dimnames(temp)[[2]] <- newNames
  new("RPPA", data=temp, file = filename)
}


RPPA <-
function(filename, path='.', blanks=0) {
  ## A function to read in the Microvigen .txt files to generate an RPPA object.	
  ## Author: Kevin Coombes
  ## Modified and commented by Wenbin Liu	(12/16/2007)

  for(k in 4:20) {
       temp <- try(read.table(file.path(path, filename), fill=TRUE,
                     header=TRUE, row.names=NULL, sep='\t',
                     quote='', comment.char='', skip=k), silent=TRUE)
       if(is(temp, 'try-error')) {
         #print('Please ignore this error message.')
         next
       }
       if (colnames(temp)[1]=='Main.Row')
          break
  }
  # The program tries an increasing number of rows to skip until
  # it reaches a header line that starts with 'Main.Row'. Generally,
  # that number falls between 4 and 13. This problem occurs when the Microvigen
  # .txt files were read in Linux/Unix versions of R, and also for changes between
  # Microvigen versions.  


  ## Also note that the backslashes("\") before underscore("_") are not accepted
  ## by R-2.6.0 and newer, so they are removed in the code below. 
  temp <- temp[,1:10]
  newNames <- dimnames(temp)[[2]]
  newNames <- sub("GeneID",   "Sample",  newNames)
  newNames <- sub("mean_",   "Mean.",   newNames)
  newNames <- sub("vol_",    "Vol.",    newNames)
  newNames <- sub("median_", "Median.", newNames)
  newNames <- sub("net",      "Net",     newNames)
  newNames <- sub("total",    "Total",   newNames)
  newNames <- sub("bkg",      "Bkg",     newNames)
  newNames <- sub("dust",     "Dust",    newNames)
  dimnames(temp)[[2]] <- newNames


  ######### 
  # Newest modification of RPPA:
  # Several sets of slides have large numbers of blanks which we want to exclude from the model fitting.
  # The following procedure treats the blanks as controls, which realizes this purpose.
  # Certainly, the sample name called 'control' must appear in the argument "control" in function "RPPADesignParams".
  ######
  temp$Sample <- as.character(temp$Sample)
  temp$Sample[blanks] <- 'control'
  temp$Sample <-as.factor(temp$Sample)
  
  ##########
  # Read sample info in order to integrate a column called 
  # 'SampleID' to the data slot of the RPPA object:
  # The file has to be 
  ##########
  
  sam.info <- read.table
  
  
  
  new("RPPA", data=temp, file = filename)

}









setMethod("summary", "RPPA", function(object, ...) {
  cat(paste("An RPPA object loaded from", object@file, "\n\n"))
  summary(object@data)
})

image.RPPA <- function(x, measure="Mean.Net", main=measure,
                                    colorbar=FALSE,
                                    col=terrain.colors(256),
                                    ...) {
  temp <- x@data
  my <- max(temp$Main.Row) * max(temp$Sub.Row)
  mx <- max(temp$Main.Col) * max(temp$Sub.Col)
  yspot <- 1+my-(max(temp$Sub.Row)*(temp$Main.Row-1) + temp$Sub.Row)
  xspot <- max(temp$Sub.Col)*(temp$Main.Col-1) + temp$Sub.Col
  geo <- tapply(temp[,measure], list(xspot, yspot), mean)
  if (colorbar) {
    layout(matrix(c(2, 1), nrow=1), widths=c(10,1))
    opar <- par(mai=c(1, 0.5, 1, 0.1))
    on.exit(par(opar))
    on.exit(layout(1))
    image(1, seq(min(geo, na.rm=TRUE), max(geo, na.rm=TRUE), length=256),
      matrix(1:256, nrow=1), col=col,
      xaxt='n', xlab='', ylab='')

  }
  image(1:mx, 1:my, geo, main=main, col=col, ...)
  abline(h = 0.5 + seq(0, my, length=1+max(temp$Main.Row)))
  abline(v = 0.5 + seq(0, mx, length=1+max(temp$Main.Col)))
  invisible(x)
}

setMethod("image", "RPPA", image.RPPA)


if (FALSE) {
  path <- "../inst/rppaTumorData"
  erk2 <- RPPA("ERK2.txt", path=path)
  summary(erk2)
  image(erk2)
  image(erk2, colorbar=TRUE)
  image(erk2, "Vol.Bkg", colorbar=TRUE)
  rm(path, erk2)
}

