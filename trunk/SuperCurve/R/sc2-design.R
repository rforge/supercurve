setClass("RPPADesign",
         representation=list(
           layout="data.frame",
           alias="list",
           sampleMap="character",
           controls="list"
           ))

setClass("RPPADesignParams", representation=list(
					   steps="numeric",
                       series="factor",
                       grouping="character",
                       ordering="character",
                       alias="list", 
                       center="logical", 
                       controls="list"
                       ))

                       
RPPADesignParams <- function(
                       steps=rep(0,1),
                       series=factor(rep(0,1)),
                       grouping=c("byRow", "byCol", "bySample", "blockSample"),
                       ordering=c("decreasing", "increasing"),
                       alias=list(), center=FALSE, controls=list()) {
  new("RPPADesignParams",
        steps = steps, series = series, grouping = grouping, ordering = ordering, alias = alias, center=center, controls = controls)                 
}

if (FALSE) {
.attachslot <- function(x) {
    xname <- substitute(x)
    sl <- names(getSlots(class(x)))
    slotnames <- paste(sl, " <<- ", xname, "@", sl, sep = "")
    for (i in slotnames) {
        eval(parse(text = substitute(slotnames, list(slotnames = slotnames))))     
    }
} 
}
     
                
RPPADesignFromParams <- function(raw, designparams) {
  RPPADesign(raw, designparams@steps, designparams@series, 
    designparams@grouping, designparams@ordering, 
    designparams@alias, designparams@center, designparams@controls)
}

# This is bad to have two constructors here since now this has to be kept in sync with PRRADesignParams
# Only keep for backwards compatibility atm
RPPADesign <- function(raw, steps=rep(0,1),
                       series=factor(rep(0,1)),
                       grouping=c("byRow", "byCol", "bySample", "blockSample"),
                       ordering=c("decreasing", "increasing"),
                       alias=list(), center=FALSE, controls=list()) {
  if (length(alias) < 1) {
	
    alias <- list(Alias=levels(raw@data$Sample), 
      Sample=levels(factor(tolower(as.character(raw@data$Sample)))))
  }
  
  if (inherits(raw, "RPPA")) raw <- raw@data
  temp <- data.frame(raw[, c("Main.Row", "Main.Col", "Sub.Row", "Sub.Col", "Sample")])
  if (length(steps) < 2 & length(series) < 2) {
    grouping <- match.arg(grouping)
    ordering <- match.arg(ordering)
    steps <- rep(NA, nrow(raw))
    series <- rep(NA, nrow(raw))
    if (grouping == "byRow") {
      series <- factor(paste("Series", temp$Main.Row, temp$Main.Col,
                             temp$Sub.Row, sep='.'))
      if (ordering == "increasing") {
        steps <- temp$Sub.Col - 1
      } else { # ordering == "decreasing"
        steps <- max(temp$Sub.Col) - temp$Sub.Col
      }
    } else if (grouping == "byCol") {
      series <- factor(paste("Series", temp$Main.Row, temp$Main.Col,
                             temp$Sub.Col, sep='.'))
      if (ordering == "increasing") {
        steps <- temp$Sub.Row - 1
      } else { # ordering == "decreasing"
        steps <- max(temp$Sub.Row) - temp$Sub.Row
      }
    } else if (grouping == "bySample") {
      series <- temp$Sample
      for (sam in levels(temp$Sample)) {
        where <- temp$Sample == sam
        n <- sum(where)
        if (ordering == "increasing") {
          steps[where] <- -1 + (1:n)
        } else { # ordering == "decreasing"
          steps[where] <- n - (1:n)
        }
      } # end for(sam)
    } else if (grouping == "blockSample") {
      attach(temp)
      series <- factor(paste(as.character(Sample), Main.Row, Main.Col, sep='.'))
      detach()
      for (sam in levels(series)) {
        where <- series == sam
        n <- sum(where)
        if (ordering == "increasing") {
          steps[where] <- -1 + (1:n)
        } else { # ordering == "decreasing"
          steps[where] <- n - (1:n)
        }
      } # end for(sam)      
    } # end if grouping
    
    if (center) {
	    for (ser in levels(series)) {
	      where <- series == ser
	      steps[where] <- steps[where] - median(steps[where])
	    }
	} else {
	  # set top intensity (undiluted) spot to zero
	    for (ser in levels(series)) {
	      where <- series == ser
	      steps[where] <- steps[where] - max(steps[where])
	    }
	}
  
  } else if (length(steps) < 2 | length(series) < 2) {
    stop("You must supply both 'steps' and 'series' if you supply either one")
  } else { # both series and steps supplied
    if(length(steps) != nrow(temp) | length(series) != nrow(temp)) {
      stop("lengths do not match")
    }
    temp$Sample <- series # override sample names from file, with user supplied series names
    # it is important to overrid so that users can specify controls with reference to their series names
    
  }# end if(is.null(steps))
  
  
  if(any(is.na(steps))) warning("Some dilution steps have not been specified")
  temp$Steps <- steps
  temp$Series <- series
  sampleMap <- as.vector(tapply(as.character(temp$Sample), list(series), function(x) x[[1]]))
  sampleMap <- tolower(sampleMap)
  names(sampleMap) <- levels(series)
  
  new("RPPADesign",
      layout=temp,
      alias=alias,
      sampleMap=sampleMap,
      controls=controls)
}


setMethod("image", "RPPADesign", function(x, ...) {
  # figure out how to make "geographic" pictures
  temp <- x@layout
  my <- max(temp$Main.Row) * max(temp$Sub.Row)
  mx <- max(temp$Main.Col) * max(temp$Sub.Col)
  yspot <- 1+my-(max(temp$Sub.Row)*(temp$Main.Row-1) + temp$Sub.Row)
  xspot <- max(temp$Sub.Col)*(temp$Main.Col-1) + temp$Sub.Col
  geo.steps <- tapply(temp$Steps, list(xspot, yspot), mean)
  image(1:mx, 1:my, geo.steps, ...)
  abline(h = 0.5 + seq(0, my, length=1+max(temp$Main.Row)))
  abline(v = 0.5 + seq(0, mx, length=1+max(temp$Main.Col)))
  invisible(geo.steps)
})



# plot the series in an RPPA under a given design layout
# see if the series make sense under this layout
plotDesign.org <- function(rppa, design, measure='Mean.Total', main='') {
	y <- rppa@data[, measure]
	x <- design@layout$Steps
	plot(c(min(x),max(x)), c(min(y), max(y)), type='n', main=paste(measure, "Intensity v.s. Dilution Step", main), 
	  xlab = 'Dilution Step', ylab = 'Intensity')
	series <- design@layout$Series
	s <- seriesNames(design) # strip out control spots
	rows <- length(s)
	bow <- rainbow(rows)
	for (i in 1:rows) {
		lines(x = x[series == s[i]], y = y[series == s[i]], col=bow[i], type='b')
	}
}

#########
plotDesign<-
function(rppa, design, measure='Mean.Total', main='') {
        y <- rppa@data[, measure]
        x <- design@layout$Steps

      ########
      ##The following were modified by Wenbin Liu:
      # Sometimes there are many (e.g., hundreds of) appearances of 'control' in the
      # Sample column and the original max(x) will mess up the plot.
      #######
               
        is.ctrl<- .controlVector(design)  #get the indexes of the control spots.
        par(mfrow=c(1,1)) #added by Wenbin Liu to avoid existing partitions of graphic device.
        plot(c(min(x[!is.ctrl]),max(x[!is.ctrl])), c(min(y), max(y)), type='n', main=paste(measure, "Intensity v.s. Dilution Step", main), 
          xlab = 'Dilution Step', ylab = 'Intensity')
        series <- design@layout$Series
        s <- seriesNames(design) # strip out control spots
        rows <- length(s)
        bow <- rainbow(rows)
        for (i in 1:rows) {
            lines(x = x[series == s[i]], y = y[series == s[i]], col=bow[i], type='b')
        }
}




setMethod("summary", "RPPADesign", function(object, ...) {
  cat("An RPPA design object with controls:\n")
  print(unlist(object@controls))
  cat("\n")
  summary(object@layout)
})

.controlVector <- function(design) {
  sample <- as.character(design@layout$Sample)
  temp <- rep(FALSE, length(unique(sample)))
  names(temp) <- unique(sample)
  temp[unlist(design@controls)] <- TRUE
  temp[sample]
}
  
seriesNames <- function(design) {
  if (!inherits(design, "RPPADesign"))
    stop("Invalid design class")
  isControl <- .controlVector(design)
  series <- as.character(design@layout$Series[!isControl])
  unique(series)
}

getSteps <- function(design) {
  isControl <- .controlVector(design)
  design@layout$Steps[!isControl]
}

setMethod("names", "RPPADesign", function(x) {
  isControl <- .controlVector(x)
  as.character(x@layout$Series[!isControl])
})

if(FALSE) {
  path <- "../inst/rppaTumorData"
  erk2 <- RPPA("ERK2.txt", path=path)
  design <- RPPADesign(erk2, grouping="blockSample", center=TRUE)
  image(design)
  summary(design)
  design <- RPPADesign(erk2, grouping="blockSample",
                       controls=list("neg con", "pos con"))
  image(design)
  summary(design)
  rm(path, erk2, design)
}

