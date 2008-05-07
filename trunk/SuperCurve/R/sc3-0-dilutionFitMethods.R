###
### DILUTIONFITMETHODS.R
###

##############################################################
# Classes for fit functions to drive dilutionFit

# Required methods 

# fitSlide
# Use the conc and intensity series for an entire slide to
# fit a curve for the the slide of intensity = f(conc)
#
# Inputs:
# conc and intensity values for a slide
# Outputs:
# (none) Use this data to create and store parameters for
# the slide fit of intensity = f(conc) in the object
setMethod("fitSlide", "FitClass",
          function(object, conc, intensity, ...) {
	stop("fitSlide must be implemented for each kind of FitClass")
})

# fitSeries
# find the concentration for an individual dilution series
# given the curve fit for the slide
#
# Inputs
# dilutions and intensities for a single dilution series
# est.conc  = starting estimated concentration for dilution = 0
#
# Outputs
# est.conc = estimated concentration for dilution = 0
#
setMethod("fitSeries", "FitClass",
          function(object, diln, intensity, est.conc, method="nls", silent=TRUE, trace=FALSE, ...) {
	stop("fitSeries must be implemented for each kind of FitClass")
})

########################################################################
# General utility functions for curve response fits

# trim
# return concentration and intesity cutoffs for the model

setMethod("trimConc", "FitClass",
          function(object, conc, intensity, design,...) {
	stop("trim must be implemented for each kind of FitClass")
    list(lo.intensity = 0, hi.intensity = 0, lo.conc = 0, hi.conc = 0)
})

#
.slide.model <- function(conc) {
	    obj <- get(".RPPA.fit.model", env = .GlobalEnv)
		fitted(obj, conc)
}

# Fit the dilution series to the model for the slide
.series.fit <- function(object, diln, intensity, est.conc, method = "nls", silent = T, trace = F, ...) {
	dum <- list(Y=intensity, Steps=diln)
			
	# define regression method
	if (method == "nls") {
	  nlsmeth <- nls
	} else if (method == "nlrob"){
	  nlsmeth <- nlrob
	  if (!require('MASS'))
       stop("This routine requires the MASS library for the 'rlm' routine.")
	} else if (method == "nlrq"){
	  nlsmeth <- function(...) {nlrq(..., control=nlrq.control(maxiter=10,eps=1e-02))}
	  if (!require('quantreg'))
       stop("This routine requires the quantreg library for the 'nlrq' routine.")
	}else {
	  stop('unrecognized regression method')
	}
	
	# function .slide.model references object back here for curve model
	assign(".RPPA.fit.model", object, env = .GlobalEnv)
	
	# tmp <- nlsmeth(Y ~ .slide.model(Steps+X), data = data.frame(Y = intensity, Steps = diln), start=list(X=est.conc),trace=trace)	                 
	
	tmp <- try(nlsmeth(Y ~ .slide.model(Steps+X), data = data.frame(Y = intensity, Steps = diln), start=list(X=est.conc),trace=trace), silent)	                 
	          
	if (is(tmp, 'try-error')) {
	  if(!silent) {warning('unavoidable nls/rlm error')}
	  warn <- 'unavoidable nls-error'
	  resids <- 0
	} else { # the model fitting succeded, so we can continue
		warn <- ''
	    est.conc <- coef(tmp)
		resids <- residuals(tmp)
	}
	
	list(warn=warn, est.conc = est.conc, resids = resids)
	
}




# This bisection search was shamelessly copied from the "fields" package
.bisection.search <- function (x1, x2, f, tol = 1e-07, niter = 25, f.extra = NA, upcross.level = 0) 
{
    f1 <- f(x1, f.extra) - upcross.level
    f2 <- f(x2, f.extra) - upcross.level
    if (f1 > f2) 
        stop(" f1 must be < f2 ")
    iter <- niter
    for (k in 1:niter) {
        xm <- (x1 + x2)/2
        fm <- f(xm, f.extra) - upcross.level
        if (fm < 0) {
            x1 <- xm
            f1 <- fm
        }
        else {
            x2 <- xm
            f2 <- fm
        }
        if (abs(fm) < tol) {
            iter <- k
            break
        }
    }
    xm <- (x1 + x2)/2
    fm <- f(xm, f.extra) - upcross.level
    list(x = xm, fm = fm, iter = iter)
}

.generic.trim <- function(object, conc, intensity, design,...) {
	  # Trim the concentration estimates to bound lower and upper concentration
      # estimates at the limits of what can be detected given our background noise.
      #
	  max.step <- max(getSteps(design))
	  min.step <- min(getSteps(design))
	  r <- fitted(object, conc)-intensity  #residuals
	  s <- mad(r, na.rm = T)
	  lBot <- quantile(intensity, probs=c(.01), na.rm=TRUE)
	  lTop <- quantile(intensity, probs=c(.99), na.rm=TRUE)
	  
	  # Use trim  * (median absolute deviation of residuals) as estimate for background noise
	  # By default, trim is set to 2.
	  # Trim is arbitrary, and was based on trying multiple cuttoff levels across multiple slides.
	  trim <-  2 * s 
	  lo.intensity <- lBot +  trim
	  # hi.intensity <- lTop - trim
	  hi.intensity <- max(intensity) # In practice we rarely see the response "top out". Do not trim at the top end.
	  
	  # search fitted model to find conc corresponding to lo.intensity
	  lo.conc <- .bisection.search(min(conc, na.rm = T), max(conc, na.rm = T), 
	    function(x, object) {fitted(object, x)-lo.intensity}, f.extra = object, tol = 0.1)$x 
	  lo.conc <- lo.conc - max.step # adjust min allowable conc to point at undiluted spot
	  
	  hi.conc <- .bisection.search(min(conc, na.rm = T), max(conc, na.rm = T), 
	    function(x, object) {fitted(object, x)-hi.intensity}, f.extra = object, tol = 0.1)$x
	  hi.conc <- hi.conc - min.step # adjust min allowable conc to point at most dilute spot	 
	    
	  list(lo.intensity = lo.intensity, hi.intensity = hi.intensity, lo.conc = lo.conc, hi.conc = hi.conc)
	  
}

##############################################################
# Loess model class

setMethod("fitSlide", "loessFitClass",
          function(object, conc, intensity, ...) {	
	fit.lo <- loess(intensity ~ conc)
	new("loessFitClass", model = fit.lo)
})

setMethod("fitted", "loessFitClass",
          function(object, conc, ...) {
	fit.lo <- object@model
	
	# loess will not interpolate beyond the initial fitted conc. range
	lo <- min(fit.lo$x)
	conc <- pmax(min(fit.lo$x), conc)
	conc <- pmin(max(fit.lo$x), conc)
	conc.pred <- conc
	conc.pred[is.na(conc)] <- lo
    
	intensity <- predict(fit.lo, data.frame(conc = conc.pred))
	intensity[is.na(conc)] <- NA
	
	intensity
})

setMethod("fitSeries", "loessFitClass",
          function(object, diln, intensity, est.conc, method = "nls", silent = T, trace = F, ...) {
	.series.fit(object, diln, intensity, est.conc, method , silent, trace)		
})

setMethod("trimConc", "loessFitClass",
          function(object, conc, intensity, design, ...) {
	.generic.trim(object, conc, intensity, design, ...)	 
})
	

##############################################################
# cobs model class

setMethod("fitSlide", "cobsFitClass",
          function(object, conc, intensity, ...) {	
	library("cobs")
	fit.lo <- cobs(conc, intensity, constraint="increase", nknots=20, lambda = object@lambda, degree = 2, tau = 0.5,
	  print.warn = FALSE, print.mesg = FALSE)
	# plot(fit.lo)
	
	new("cobsFitClass", model = fit.lo, lambda = fit.lo$lambda)
})


.predict.spline=function(xvec, aknot, acoef){
	library("splines")
	nknot=length(aknot)
	aknotnew=c(aknot[1], aknot[1],aknot, aknot[nknot], aknot[nknot])
	ncoef=length(acoef)
	
	xvec[xvec<(aknot[1]+(1e-8))]=aknot[1]+(1e-8)
	xvec[xvec > (aknot[nknot]-(1e-8))]=aknot[nknot]-(1e-8) 
	
	a=spline.des(aknotnew, xvec,ord=3)
	fvalvec= (a$design)%*%acoef
	
	return(as.vector(fvalvec))
}

setMethod("fitted", "cobsFitClass",
          function(object, conc,...) {
	fit <- object@model
	
	# Predict missing values at min intensity
	lo <- min(fit$x)
	conc.pred <- conc
	conc.pred[is.na(conc)] <- lo
    
	if (TRUE){
		# predict.cobs is irritating
		# It returns predicted values after sorting on the input vector.
		# So we get intensity ~ sort(fit)
		# We need to undo this sort to find predictions using the original 
		# concentration ordering
		
		n <- length(conc.pred)
		if (n > 1) {
			#undo sort on fit
			o <- sort.list(conc.pred, method = "quick", na.last = NA)
			u <- rep(NA, n)
			u[o] <- 1:n
			cobs.intensity <- predict(fit, conc.pred[o])[,"fit"]
			intensity <- cobs.intensity[u]
		} else {
			# only one data point
			intensity <- predict(fit, conc.pred)[,"fit"]
		}
		
	} else {
		# The above sort and unsort process is yucky and a bit slow.  
		# Jianhua did not use the cobs predict method and instead evaluates
		# the spline directly.  Unfortunately there seems to be a bug in
		# .predict.spline where it does not have the correct number of coefficients sometimes
	    aknot=fit$knots
	    acoef=fit$coef
	    intensity <- .predict.spline(conc.pred,aknot,acoef) 
    }
    
    # plot(cobs.intensity, intensity)

	intensity[is.na(conc)] <- NA
	
	intensity
})

setMethod("fitSeries", "cobsFitClass",
          function(object, diln, intensity, est.conc, method = "nls", silent = T, trace = F, ...) {
	.series.fit(object, diln, intensity, est.conc, method , silent, trace)		
})

setMethod("trimConc", "cobsFitClass",
          function(object, conc, intensity, design, ...) {
	.generic.trim(object, conc, intensity, design, ...)	 
})
	


##############################################################
# Logistic model class

.ea <- function(x) { exp(x)/(1+exp(x)) }  # note: rnls does not work with local functions
.lp <- function(p) log(p/(1-p))


.coef.quantile.est <- function(intensity) {
	lBot <- quantile(intensity, probs=c(.05), na.rm=TRUE)
	lTop <- quantile(intensity, probs=c(.95), na.rm=TRUE)
	p.alpha <- lBot
    p.beta  <- lTop-lBot
	p.gamma <- log(2)  # assume linear response on log2 scale as first guess
	list(alpha=p.alpha, beta=p.beta, gamma=p.gamma)
}

setMethod("fitSlide", "logisticFitClass",
          function(object, conc, intensity, ...) {
	 cf <- as.list(object@coefficients)	 
	 
	 if (cf$gamma == 0) {
		 cf <- .coef.quantile.est(intensity) #initialize coefficients
	 }
	 dum <- data.frame(xval=conc, yval=log(intensity + 5000))
	 nls.model <- try(nls(yval ~ log(alpha + beta*.ea(gamma*xval)+ 5000), data=dum,
	                     start=list(alpha=cf$alpha, beta=cf$beta, gamma=cf$gamma),
	                     control=nls.control(maxiter=100), na.action='na.omit'))
	 if(is(nls.model, 'try-error')) {
  		warning('unable to perform 1st pass overall slide fit. trying quantiles.') 
  		# This is a crude way to get alpha and beta but it seems to be robust.
  		cf <- .coef.quantile.est(intensity)		
	} else {  
		cf <- coef(nls.model)
		p.alpha <- cf['alpha']
		p.beta  <- cf['beta']
		p.gamma <- cf['gamma']
		cf <- list(alpha=p.alpha, beta=p.beta, gamma=p.gamma)
	}
	
	new("logisticFitClass", coefficients = unlist(cf))
})

setMethod("fitted", "logisticFitClass",
          function(object, conc, ...) {
	cf <- as.list(object@coefficients)
	cf$alpha + cf$beta*.ea(cf$gamma*conc)	
})



setMethod("fitSeries", "logisticFitClass",
          function(object, diln, intensity, est.conc, method = "nls", silent = T, trace = F, ...) {
	.series.fit(object, diln, intensity, est.conc, method , silent, trace)		
})


setMethod("trimConc", "logisticFitClass",
          function(object, conc, intensity, design, ...) {
	  # Trim the concentration estimates to bound lower and upper concentration
      # estimates at the limits of what can be detected given our background noise.
      #
	  cf <- as.list(object@coefficients)
	  max.step <- max(getSteps(design))
	  min.step <- min(getSteps(design))
	  r <- fitted(object, conc)-intensity  #residuals
	  s <- mad(r, na.rm = T)
	  # Use trim  * (median absolute deviation of residuals) as estimate for background noise
	  # By default, trim is set to 2.
	  # Trim is arbitrary, and was based on trying multiple cuttoff levels across multiple slides.
	  trim <-  2 * s / cf$beta
	  lo.conc <- .lp(trim)/cf$gamma - max.step
	  hi.conc <- .lp(1-trim)/cf$gamma - min.step
	  
	  lo.intensity <- cf$alpha +  cf$beta * trim
	  hi.intensity <- cf$alpha + cf$beta - cf$beta * trim
	    
	  list(lo.intensity = lo.intensity, hi.intensity = hi.intensity, lo.conc = lo.conc, hi.conc = hi.conc)
})

