###
### TOPONORM.R
###


##=============================================================================

spatialNorm <- function(rppa,
                        measure,
                        nDilut=1,
                        plot.surface=F,
                        k=100,
                        gamma=0.1,
                        negcon=NULL,
                        poscon=list(),
                        cutoff=0.8)   

{
	                   
	## Check arguments
    if (!inherits(rppa, "RPPA")) {
        stop(sprintf("argument %s must be object of class %s",
                     sQuote("rppa"), "RPPA"))
    }

    if (!is.numeric(k)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("k")))
    } else if (!(length(k) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("k")))
    }

    if (!is.numeric(gamma)) {
        stop(sprintf("argument %s must be numeric",
                     sQuote("gamma")))
    } else if (!(length(gamma) == 1)) {
        stop(sprintf("argument %s must be of length 1",
                     sQuote("gamma")))
    }

    ## :TBD: Add required 'mgcv' package to Suggests or Depends?
    ## :TBD: What exactly do we use from this package?
    if (!require(mgcv)) {
        stop(sprintf("%s package required for fitting the GAM in the %s method",
                     sQuote("mgcv"), sQuote("spatialNorm")))
    }

 
   # Set up the row and column variables
    mydata <- rppa@data
	mydata$Row <- (mydata$Main.Row-1)*max(mydata$Sub.Row) + mydata$Sub.Row
	mydata$Col <- (mydata$Main.Col-1)*max(mydata$Sub.Col) + mydata$Sub.Col
	# pd is a dataset with the row and column index used for predicting the surface
	pd <- data.frame(Row=mydata$Row,Col=mydata$Col)
	
	#find the positive controls
	positives <- mydata
	items <- mydata$Sample %in% unlist(poscon)
	positives <- positives[items,]

	# FIND A WAY TO IDENTIFY NOISE REGION
	# The default is to assume there are no negative controls in
	#   which case I use the background to compute the noise region.
	#   If there are positive controls I am assuming that they all have
	#   the same name and a different name from the other controls.  The name
	#   of the negative control spots is entered as "negcon" when the function is called.
	
	if (is.null(negcon)) bg <- mydata$Mean.Total-mydata$Mean.Net
	else {items <- mydata$Sample==negcon
	      bg <- mydata$Mean.Net[items]}
	# Compute background cutoff using the quantile of background specificied when the function
	# is called.  If this value gets too low, I automatically use a larger quantile.
	bgCut <- quantile(bg,cutoff)
	if(bgCut<= 100) {bgCut <- quantile(buffer,.99)
	              if(bgCut<=100) {bgCut <- max(buffer[-which.max(buffer)])}}
 	
	# Now remove positive controls less than this value            
	is.na(mydata[items,'Mean.Net']) <- mydata[items,'Mean.Net'] < bgCut

	#TBD:  need to change how we identify the different levels of positive control
	# (we probably should use Sub.Row to identify these spots)
	for (i1 in 1:nDilut) {
		temp <- positives
		temp <- temp[temp$Sub.Row==(i1) |temp$Sub.Row==(i1+6),]
		b1 <- gam(Mean.Net~s(Row,Col,bs="ts",k=k),data=temp,gamma=gamma)
		assign(paste('surface',i1,sep=""),predict.gam(b1,newdata=pd))
	}
	



	# make a plot of the different surfaces
	# TBD: (esn) I don't know the best way to organize the various levels of surface plots.  
	#  Here I plot them one at a time
	if(plot.surface==T) {
		temprppa <- rppa
		par(ask=T)
		for (i2 in 1:nDilut) {
			x <- paste("surface",i2,sep="")
			temprppa@data[,x] <- eval(as.name(x))
			image(temprppa,x,colorbar=T)
		}
		par(ask=F)
	}
	
		

	# Constrain the surface so they do not cross
	if(nDilut > 1) {
		for (i3 in 2:nDilut) {
			s2 <- eval(as.name(paste("surface",i3,sep="")))
			s1 <- eval(as.name(paste("surface",(i3-1),sep="")))
			s2[s1<s2] <- s1[s1<s2]-runif(1)
			assign(paste('surface',i3,sep=""),s2)
		}
	}
	

	################################################ 
	# FUNCTIONS									   #
	# These are functions used to do the nested    #
	# surface normalization						   #
	################################################

	# The "which.bin" function takes as input a vector
	# whose first value is the meaure to be corrected
	# (either Mean.Net or Mean.Total) and the others
	# values at each level of predicted surface.
	# The value that is returned is the surface to which
	# each spot is closest in intensity

    which.bin <- function(value) {
        stopifnot(is.numeric(value) && length(value) > 1)

        x <- value[1]
        vec <- value[-1]
        n <- length(vec)
        place <- NA
        for (i in seq(1, n-1)) {
            if (vec[i] > x & x > vec[i+1]) {
                place <- i
                break
            }
            if (is.na(place)) {
                ## :TBD: Can both of these conditions be true? Shannon:  No because
                # we constrain the surface so they don't "cross" above
                if (x >= vec[1]) {
                    place <- 0
                }
                if (x <= vec[n]) {
                    place <- n
                }
            }
        }
        place
    }

	# The "getp" function takes as input a vector
	# whose first value is the output from the "which.bin" function,
	# the second is the meaure to be corrected
	# (either Mean.Net or Mean.Total) and the others
	# values at each level of predicted surface.
	# If the spot to be corrected falls between two surface, the value
	# that is returned is the fraction it falls between the two surfaces.
	# Otherwise a 0 is returned.
		
	getp <- function(value) {
        stopifnot(is.numeric(value) && length(value) >= 3)

        item <- value[1]
        mn <- value[2]
        #vec <- value[3:length(value)]
        vec <- value[-(1:2)]
        n <- length(vec)

        p <- if (item != 0 && item != n) {
                (vec[item] - mn) / (vec[item] - vec[item+1])
             } else {
                 0
             }
    }

	
	# The "getadj" function takes as input a vector
	# whose first value is the output from the "which.bin" function,
	# the second is output from the "getp" function and the third
	# is the meaure to be corrected
	# (either Mean.Net or Mean.Total).  The other values are the corrected
	# measure values.   If we want to correct the Mean.Net values of each
	# spot, then the fourth column of the input matrix is the Mean.Net value
	# scaled to the first positive control surface, the fifth is the
	# Mean.Net values scaled to the half strength positive controls and so on.
	# The value that is return is the overall adjustment.
	
	getadj <- function(value) {
        stopifnot(is.numeric(value) && length(value) >= 3)

        item <- value[1]
        p <- value[2]
        #vec <- value[3:length(value)]
        vec <- value[-(1:2)]
        n <- length(vec)

        adj <- if (item == 0) {
                   vec[1]
               } else if (item == n) {
                   vec[n]
               } else {
                   vec[item]*(1-p) + vec[item+1]*p
               }
    }
	
	###########################################################
	# END OF FUNCTIONS                                        #
	###########################################################
	
	
	# Organize the matrix for input into the "which.bin" function
	mn <- rppa@data[,measure]
	surf <- mn
	for (i4 in 1:nDilut) {
		x <- eval(as.name(paste('surface',i4,sep="")))
		surf <- cbind(surf,x)
	}

	
	# Now apply the "which.bin" and "getp" function to each row in order 
	# to find the closest positive control surface and the fraction between
	# two surfaces
	place <- apply(surf,1,which.bin)
	myvalue <- cbind(place,surf)
	p <- apply(myvalue,1,getp)
	
	
	# Perform scaling to each of the positive control surfaces
	
	adj <- matrix(NA,nrow=nrow(mydata),ncol=nDilut)
	for (i5 in 1:nDilut) {
		x <- mydata[,measure]
		s1 <- eval(as.name(paste("surface",i5,sep="")))
		adj[,i5] <- (x/s1)*median(s1)
	}

	# Now retrive the appropriate adjustment based on the closest positive
	# control surface and the fraction between two surfaces as computed
	# by the "getadj" function.
		
	myadjust <- cbind(place,p,adj)
	temp <- apply(myadjust,1,getadj)
	
	rppa@data$Spatial.Norm <- temp
	#rppa@data$Spatial.Norm[temp<0] <- rppa@data$Mean.Net[temp<0]
	
	
	
	return(rppa)
}














