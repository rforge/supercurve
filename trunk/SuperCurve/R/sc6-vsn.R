


normalize <- function(data,method="median",house=NULL) {
	
	# The normalize function performs normalization for sample
	# loading after quantification.  It has two required
	# input values: 1) the data matrix with samples in the rows 
	# and proteins in the columns. 2) the method of sample loading
	# normalization:
	#	median - the sample median (row median) is subtracted from
	# 			 each sample
	#   vs	   - variable slope normalization.  Here the sample median
	#			 is used along with a multiplicate gamma 
	#   house  - housekeeping normalization.  The median of a housekeeping
	#		     protein or set of housekeeping proteins are used.  If housekeeping
	# 			 normalization is selected, then the name of the protein(s) to
	#            be used must also be supplied.
	
	
	
	
	# This function estimates the multiplicative gamma terms
	# from variable slope normalization.  It takes as input
	# the data matrix (with samples in the rows and proteins in the
	# columns).  It is assumed that this matrix has already had
	# the column median swept out from its columns.  It outputs
	# the estimates of the gammas (the multiplicative protein effects).
	 
	estimateGamma <- function(Xhat) {

		nCol <- ncol(Xhat)
		gamma <- matrix(0,nrow=nCol,ncol=nCol)
		means <- apply(Xhat,2,mean)
		
		for (i in 1:(nCol-1)) {
			for (j in (i+1):nCol) {
				r <- cor(Xhat[,i],Xhat[,j],use="complete.obs")
				a <- Xhat[,i]
				n <- length(a)
				tt <- r*sqrt((n-2)/(1-r^2))
				chk <- pt(tt,n-2,lower.tail=F)
				if (chk < .05) {
					eig <- eigen(var(cbind(Xhat[,i],Xhat[,j]),na.rm=T))
					tmp <- (-1)*eig$vectors[1,2]/eig$vectors[2,2]
					gamma[i,j] <- tmp
				}
			}
		}
		gamma[gamma<=0] <- 1
		upper <- upper.tri(gamma)
		ind <- which(upper,arr.ind=T)
	
		design <- matrix(0,ncol=nCol,nrow=nrow(ind))
		for(i in 1:nrow(ind)) {
			design[i,ind[i,1]] <- -1
			design[i,ind[i,2]] <- 1
		}
	
		loggamma <- log(gamma[upper])
	
		newrow <- rep((1/nCol),nCol)
		nonsingular <- rbind(newrow,design)
		lestimateMean <- qr.solve(nonsingular,c(0,loggamma))
	
		estimate1 <- exp(lestimateMean)
		val  <- estimate1
		
		val
	}

		
	#############################
	
	rowMedian <- apply(data,1,median,na.rm=TRUE)
	colMedian <- apply(data,2,median,na.rm=TRUE)
	
	data <- sweep(data,2,colMedian)
	gamma <- NULL
	houseMedian <- NULL
	
	if (method=="median") {
		# median normalization
		dataNorm <- sweep(data,1,rowMedian)
	}
	else if (method=="house") {
		#housekeeping normalization
		houseMedian <- apply(data[,house],1,median,na.rm=TRUE)
		dataNorm <- sweep(data,1,houseMedian,"-")
	}
	else if (method=="vs") {
		# variable slope normalization
		gamma <- estimateGamma(data)
		temp <- sweep(data,2,gamma,"/")
		dataNorm <- sweep(temp,1,rowMedian,"-")
	}
	
	  list(dataNorm=dataNorm,rowMedian=rowMedian,colMedian=colMedian,
	     gamma=gamma,houseMedian=houseMedian)
	
}


	

	
	
	
	
	



















