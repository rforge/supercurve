###
### RPPASET.R
###

##############################################################
# Fit a set of slides with a common layout
# Initial version: Corwin Joy


# provide a generic convenience function to view a slot in the array of fits as a simple matrix view
# e.g. fitslot(fitset, 'concentrations')
#
setMethod("fitslot", "RPPASet",
          function(object, sl, ...) {
  expr <- paste("object@fits[[1]]@", sl, sep='')
  mat <- matrix(NA, nrow = length(eval(parse(text=expr))), ncol = length(rownames(object@fits)))
  rownames(mat) <- colnames(t(eval(parse(text=expr))))
  
  for(j in 1:ncol(mat)) {
	  expr <- paste("object@fits[[j]]@", sl, sep='')
	  mat[,j] <- eval(parse(text=expr))
  }
  colnames(mat) <- rownames(object@fits)
  mat
})


# provide a convenience function to save fit results to file
# file = base filename
# path = path
# normalize = "median" for median normalization of the concentrations. 
#     "median" = for each sample the median intensity over all plates is subtracted to account for varying sample concetratiosn
#
setMethod("write.summary", "RPPASet",
          function(object, file, path, graphs = TRUE, tiffdir = NULL, ...) {
	conc <- fitslot(object, 'concentrations')
	conc.ss <- fitslot(object, 'ss.ratio')
	if(sum(as.character(object@design@alias$Alias) == as.character(object@design@alias$Sample)) < nrow(conc)) {
		# We have non-trivial alias names.
		# Use sample aliases to write out data
		rno <- rownames(conc)
		sn <- object@design@sampleMap[rno]
		lookup.sn <- match(sn, object@design@alias$Sample)
        alias.name <- as.character(object@design@alias$Alias)[lookup.sn]
        rownames(conc) <- alias.name
        rownames(conc.ss) <- alias.name
	}
	write.csv(conc , file=file.path(path, paste(file, '_conc_raw.csv', sep='')))
	
	# median polish to normalize sample, slide effects
	pol <- medpolish(conc,  trace.iter = F)
	conc <- pol$residuals
	sample.correction <- pol$row
	conc <- cbind(sample.correction, conc)
	write.csv(conc , file=file.path(path, paste(file, '_conc_med_polish.csv', sep='')))
	
	write.csv(conc.ss , file=file.path(path, paste(file, '_ss.csv', sep='')))
	
	# Use red/yellow/green palette for residual plots. From RColorBrewer palette RdYlGn
	RYG <- c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837")

	if(graphs) { # save fit graphs
		op <- par()
	    par(mfrow=c(2,1))
	    proteins <- rownames(object@fits)
	    for(i in 1:length(proteins)) {
		    # first pair of plots
		    ptitle <- paste(object@fits[[i]]@measure, ":  ", proteins[i], sep ="")
			try(plot(object@fits[[i]], xlim=c(-15,15), main=ptitle, xform = object@fitparams@xform))
			# Mark R^2 = 0.4 and below as red.
			try(image.RPPAFit(object@fits[[i]], measure = "ResidualsR2", xlab="Residuals R^2", main="", col = RYG, zlim = c(0.4, 1)))
			dev.copy(png, file.path(path, paste(file, proteins[i], 'png', sep='.')),
			         width=640, height=640)
			dev.off()
			
			try(plot(object@fits[[i]], xlim=c(-15,15), main=ptitle, xform = object@fitparams@xform, type = "resid"))
			try(plot(object@fits[[i]], xlim=c(-15,15), main=ptitle, xform = object@fitparams@xform, type = "steps"))
			dev.copy(png, file.path(path, paste(file, proteins[i], '2', 'png', sep='.')),
			         width=640, height=640)
			dev.off()
		}
		par(op)
		
		if (TRUE) {
			# Use ImageMagick to merge output graphs with source tiff files
			if(is.null(tiffdir)) {
				savedir <- getwd()
				setwd(path)
				setwd("..")
				setwd("tif") # assume that the tif images are in a sibling directory named "tif"
				tiffdir <- getwd()
				setwd(savedir)
			}
			for(i in 1:length(proteins)) {
				tiff <- sub(".txt", ".tif", proteins[i], fixed = T)
				tiff <- file.path(tiffdir, tiff)
				base <- sub(".txt", "", proteins[i], fixed = T)
				
				# convert *4EBP1.tif "Y:\Private\LysateArray\DorisSiwak\Feiller\Feiler results\*4EBP1.txt.png" -append -quality 100 4EBP1.jpg
				
				pg1 <- file.path(path, paste(file, proteins[i], 'png', sep='.'))
				pg2 <- file.path(path, paste(file, proteins[i], '2', 'png', sep='.'))
				output <- file.path(path, paste(base, "jpg", sep='.'))
				# convert  $pg1 $pg2 +append $tiff -append -quality 100 $output
				print(paste('merging tiff for', proteins[i]))
				flush.console()
				shell(paste('convert "', pg1, '" "', pg2, '" +append ', '"', tiff, '" -append -quality 100 "', output, '"', sep = ""), invisible = TRUE)
			}
	    }
	}
})
           
# Create an RPPA set from a directory of slides.
# path = directory to analyze
# designparams =  A common slide design specification of class RPPADesignParams
# fitparams =  A common fit specification of class RPPAFitParams
#
# example usage:
# see tests/testRPPASet.R


####### RPPAFitDir() 
RPPAFitDir <- function(path, designparams, fitparams, blanks=blanks) {
        if(!inherits(designparams, "RPPADesignParams"))
        stop("'design' must be a valid RPPADesignParams object")
  ## Modified by Wenbin Liu.
  ## Main change: added argument blanks.
  ## Purpose: to exclude the blanks when fitting supercurve
    if(!inherits(fitparams, "RPPAFitParams"))
        stop("'fitparams' must be a valid RPPAFitParams object")
    
        call <- match.call()
         
        slidefiles <- list.files(path = path, pattern = ".*[tT][xX][tT]$")  # assume all .txt files in the directory are slides
        
        # Load alias information in directory
        if (length(designparams@alias) < 1) {
                 if (file.exists(file.path(path, 'layoutInfo.tsv'))[1]) {
                         sampleLayout <- try(read.table(file.path(path, 'layoutInfo.tsv'),
                                     header=TRUE, row.names=NULL, sep='\t',
                                     quote='', comment.char=''))
                                     
                     al <- list(Alias = sampleLayout$Alias, Sample = sampleLayout$Sample)
                     designparams@alias <- al
             }
        }
        
        #warning(paste('reading ', slidefiles[1]), immediate. = TRUE)
        print(paste('reading ', slidefiles[1]))
        firstslide <- RPPA(slidefiles[1], path=path, blanks)
        design <- RPPADesignFromParams(firstslide, designparams)
        
        plotDesign(firstslide, design, 'Mean.Total', main = slidefiles[1]) # plot the first slide as a quick design check
        # Modified by wenbin liu

        rppas <- array(list(), c(length(slidefiles)), slidefiles)
        rppas[[1]] <- firstslide
        if(length(slidefiles) > 1) {
                for (i in 2:length(slidefiles)) {
                   print(paste('reading ', slidefiles[i])) #print replaced warning -- Wenbin
                   rppas[[i]] <- RPPA(slidefiles[i], path=path, blanks)  ##### Wenbin
                }
        }
        flush.console()
        
        fits <- array(list(), c(length(slidefiles)), slidefiles)
        for (i in 1:length(slidefiles)) {
           #warning(paste('fitting ', slidefiles[i]), immediate. = TRUE)
           print(paste("fitting", slidefiles[i], ".", "Please wait."))
           flush.console()
           fits[[i]] <- RPPAFitFromParams(rppas[[i]], design = design, fitparams = fitparams)
        }

        rownames(fits) <- slidefiles
        rownames(rppas) <- slidefiles
        
        new("RPPASet",
            call=call,
            design=design,
            rppas=rppas,
            fitparams=fitparams,
            fits=fits)
          #removed actual argument version=packageDescription("SuperCurve", fields="Version") by Wenbin Liu
}

