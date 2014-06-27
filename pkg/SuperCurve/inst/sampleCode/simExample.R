doSims <- FALSE

if (doSims) {


dilutionTest <- function(nsamp, ndilut, machine.noise, trueA, trueB, trueBeta) {
	##############################
	ea <- function(x) { exp(x)/(1+exp(x)) }
	
	foo <- function(x, A, B, alpha=0, beta=10/(B-A)) {
	  A + (B-A)*ea(alpha+beta*x)
	}
	
	
	# Y ~ p.alpha + p.beta*ea(p.gamma*(B+X))
	
	balance <- (1:ndilut)-(ndilut+1)/2 # center the series on 0
	# balance <- (-(ndilut-1)):0
	
	data <- matrix(0, nrow=nsamp, ncol=ndilut)
	trueX <- rep(-4, nsamp)
	for (i in 1:nsamp) {
	  data[i,] <- foo(balance+trueX[i], trueA, trueB, beta=trueBeta) +
	    rnorm(ndilut, 0, machine.noise) 
	}
	# see that it tracks the curve pretty well
	xx <- seq(-12, 12, by=0.5)
	yspread <- 0.05 * (trueB-trueA)
	plot(xx, foo(xx, trueA, trueB, beta=trueBeta), type='l',
	     ylim=c(trueA - yspread, trueB+yspread), 
	     xlab='log quantity', ylab='intensity', main='Truth')
	for(i in 1:nrow(data)) {
	  points(balance+trueX[i], data[i,], col=i, pch=i)
	}
	
	readline("Press Enter to Continue") # pause
	
	fit <- dilutionFit(data, verbose=T)
	yy <- foo(xx, A=fit$p.alpha,
	          B=fit$p.beta + fit$p.alpha,
	          beta=fit$p.gamma)
	
	plot(xx, foo(xx, trueA, trueB, beta=trueBeta), type='l', lwd=2,
	     ylim=c(trueA - yspread, trueB+yspread), 
	     xlab='log quantity', ylab='intensity')
	lines(xx, yy, col='red', lwd=2)
	for(i in 1:nrow(data)) {
	  points(fit$balance[i,]+fit$pass2[i], data[i,], col='red', pch=i)
	  points(balance+trueX[i], data[i,], col='black', pch=i)
	}
	legend(-10, 5000, c('Truth', 'Estimated'), col=c('black', 'red'), lwd=2)
	title('Simulation One')
	readline("Press Enter to Continue") # pause
	
	fit

}

# Dilution fit RegresssionTest
dilutionFitRegessionTest <- function() {
	
	######################
	######################################################
	# see what kinds of data we can simulate
	#
	# parameters:
	nsamp <- 50          # number of diluted samples
	ndilut <- 6          # number of steps in the dilution series
	machine.noise <- 150 # spread on the log intensity values
	trueA <- 1000        # baseline
	trueB <- 5000        # max detection
	trueGamma <- 0.8      # logistic slope
	
	
	stuff <- dilutionTest(nsamp, ndilut, machine.noise, trueA, trueB, trueGamma)
	
	
	# Why are the fitted coefficients generally negative?
	hist(stuff$pass2)
	
	# We expect 
	#
	# $p.alpha
	# 919.6492 
	
	# $p.beta
	# 983.5837 
	
	# $p.gamma
	# 1.057468 
	
	print(paste("true alpha", trueA, ": fitted alpha", stuff$p.alpha))
	print(paste("true beta", trueB-trueA, ": fitted beta (we expect fitted to be low)", stuff$p.beta))
	print(paste("true gamma", trueGamma, ": fitted gamma", stuff$p.gamma))
	
	# Since all samples are from same distribution, average offset from main supercurve should be near 0
	print(paste("mean pass2", mean(stuff$pass2)))
	print(paste("sd pass2", sd(stuff$pass2)))
	
	if(abs(stuff$p.alpha-trueA) > 90) {
		warning('dilutionFit() did not fit alpha as expected')
	}
	
	if(abs(stuff$p.beta-900) > 30) {
		warning('dilutionFit() did not fit beta as expected')
	}
	
	if(abs(stuff$p.gamma-trueGamma) > 0.7) {
		warning('dilutionFit() did not fit gamma as expected')
	}
	
	
	
}



#### simulation plan
#
# Keep fixed values for trueA, trueB, trueBeta as above
# [1]repeat for 10 different values of machine.noise
#   [2]repeat 3 times (full range; bottom end; top end)
#     select 40 locations for EC50 values (note: sometimes span the
#          linear range, sometimes only half to two-thirds of the range)
#     [3]repeat 100(?) times
#       generate duplicate dilution series (length 8) for each EC50 value
#       [4]repeat 100(?) times
#         estimate individual logistic values with est-EC50
#         estimate common logistic curve with est-EC50
# what to save from each repetition?

# simulation count parameters
nrep <- 100 # number of innermost loop[4] replicates

# global parameters:
trueA <- 1000        # baseline
trueB <- 5000        # max detection
trueBeta <- 0.8      # logistic slope
ndilut <- 8          # number of steps in the dilution series
balance <- (1:ndilut)-(ndilut+1)/2 # center the series on 0

ea <- function(x) { exp(x)/(1+exp(x)) }

foo <- function(x, A, B, alpha=0, beta=10/(B-A)) {
	  A + (B-A)*ea(alpha+beta*x)
}

# first loop: varying the error in measured intensity
machine.noise <- 150 # spread on the log intensity values

# second loop:
nsamp <- 40          # number of diluted samples
mu <- 0              # main location of concentrations (0 = EC50)
sigma <- 4.5         # biological spread in concentrations

# third loop:
# set aside space for the data
data <- matrix(0, nrow=2*nsamp, ncol=ndilut)
# generate the true Ec50's
trueX <- rep(rnorm(nsamp, mu, sigma), each=2)

# fourth loop:
indParam <- array(NA, dim=c(nrep, nsamp, 3))
dimnames(indParam)[[3]] <- c('alpha', 'beta', 'gamma')
indOffset <- array(NA, dim=c(nrep, 2*nsamp))

jointParam <- array(NA, dim=c(nrep, 3))
dimnames(jointParam)[[2]] <- c('alpha', 'beta', 'gamma')
jointOffset <- array(NA, dim=c(nrep, 2*nsamp))

for (rep in 1:nrep) {
  print(rep)
  # now generate the data matrix
  for (i in 1:(2*nsamp)) {
    data[i,] <- foo(balance+trueX[i], trueA, trueB, beta=trueBeta) +
      rnorm(ndilut, 0, machine.noise) 
  }
  for (i in 1:nsamp) {
    print(i)
    sampselect <- 2*i + (-1:0)
    temp <- try(dilutionFit(data[sampselect,]))
    if (!is(temp, 'try-error')) {
      indParam[rep, i,] <- c(temp$p.alpha, temp$p.beta, temp$p.gamma)
      indOffset[rep, sampselect] <- temp$pass2
    }
  }
  temp <- dilutionFit(data)
  jointParam[rep,] <- c(temp$p.alpha, temp$p.beta, temp$p.gamma)
  jointOffset[rep,] <- temp$pass2
}

io <- apply(indOffset, 2, mean, na.rm=TRUE)
jo <- apply(jointOffset, 2, mean, na.rm=TRUE)
isd <- apply(indOffset, 2, sd, na.rm=TRUE)
jsd <- apply(jointOffset, 2, sd, na.rm=TRUE)
pairs(data.frame(io, jo, trueX))

plot(io, jo, type='n', ylim=c(-11, 11), xlim=c(-5,5),
     xlab='individual estimates', ylab='joint estimates')
for (rep in 1:nrep) {
  points(indOffset[rep,], jointOffset[rep,])
}
abline(0,1)
abline(0,3, col='red')

badslope <- coef(lm(as.vector(indOffset) ~ rep(trueX, each=nrep) - 1))
badslope <- 0.33
opar <- par(mfrow=c(2,1), mai=c(0.9,0.9, 0.1, 0.1))
plot(rep(trueX, each=nrep), as.vector(jointOffset),
     xlim=c(-11,11), ylim=c(-11,11), xlab='true EC50', ylab='joint est EC50')
abline(0,1)
plot(rep(trueX, each=nrep), as.vector(indOffset),
     xlim=c(-11,11), ylim=c(-11,11), xlab='true EC50', ylab='indiv est EC50')
abline(0, badslope)
par(opar)

###################
# LEFT END

mu <- -4              # main location of concentrations (0 = EC50)
sigma <- 2.5            # biological spread in concentrations
# set aside space for the data
le.data <- matrix(0, nrow=2*nsamp, ncol=ndilut)
# generate the true Ec50's
le.trueX <- rep(rnorm(nsamp, mu, sigma), each=2)

# fourth loop:
le.indParam <- array(NA, dim=c(nrep, nsamp, 3))
dimnames(le.indParam)[[3]] <- c('alpha', 'beta', 'gamma')
le.indOffset <- array(NA, dim=c(nrep, 2*nsamp))

le.jointParam <- array(NA, dim=c(nrep, 3))
dimnames(le.jointParam)[[2]] <- c('alpha', 'beta', 'gamma')
le.jointOffset <- array(NA, dim=c(nrep, 2*nsamp))

for (rep in 1:nrep) {
  print(rep)
  # now generate the data matrix
  for (i in 1:(2*nsamp)) {
    le.data[i,] <- foo(balance+le.trueX[i], trueA, trueB, beta=trueBeta) +
      rnorm(ndilut, 0, machine.noise) 
  }
  for (i in 1:nsamp) {
    print(i)
    sampselect <- 2*i + (-1:0)
    temp <- try(dilutionFit(le.data[sampselect,]))
    if (!is(temp, 'try-error')) {
      le.indParam[rep, i,] <- c(temp$p.alpha, temp$p.beta, temp$p.gamma)
      le.indOffset[rep, sampselect] <- temp$pass2
    }
  }
  temp <- dilutionFit(le.data)
  le.jointParam[rep,] <- c(temp$p.alpha, temp$p.beta, temp$p.gamma)
  le.jointOffset[rep,] <- temp$pass2
}

le.io <- apply(le.indOffset, 2, mean, na.rm=TRUE)
le.jo <- apply(le.jointOffset, 2, mean, na.rm=TRUE)
le.isd <- apply(le.indOffset, 2, sd, na.rm=TRUE)
le.jsd <- apply(le.jointOffset, 2, sd, na.rm=TRUE)

pairs(data.frame(le.io, le.jo, le.trueX))

plot(le.io, le.jo, type='n', ylim=c(-11, 11), xlim=c(-5,5),
     xlab='individual estimates', ylab='joint estimates')
for (rep in 1:nrep) {
  points(le.indOffset[rep,], le.jointOffset[rep,])
}
abline(0,1)
abline(0,3, col='red')

badslope <- coef(lm(as.vector(le.indOffset) ~ rep(le.trueX, each=nrep) - 1))
badslope <- 0.33
opar <- par(mfrow=c(2,1), mai=c(0.9,0.9, 0.1, 0.1))
plot(rep(le.trueX, each=nrep), as.vector(le.jointOffset),
     xlim=c(-15,8), ylim=c(-15,8), xlab='true EC50', ylab='joint est EC50')
abline(0,1)
plot(rep(le.trueX, each=nrep), as.vector(le.indOffset),
     xlim=c(-15,8), ylim=c(-15,8), xlab='true EC50', ylab='indiv est EC50')
abline(0, badslope)
par(opar)

} # end if(doSims)

