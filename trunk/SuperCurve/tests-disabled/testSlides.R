# version 2.0 requires rlm from the MASS library
require(MASS)
library(SuperCurve)

# setwd("Y:/Private/LysateArray/SuperCurveVersions")
setwd("../SuperCurve")

############################################################################
# start by loading the data, which consists of three files each from
# three different data sets


######################################
# load the 40-cell-line data

# first locate the list of assays
cell.home <- "rppaCellData"
cell.proteins <- read.table(file.path(cell.home, 'proteinAssay.tsv'),
                       header=TRUE, sep='\t', as.is=TRUE)
dimnames(cell.proteins)[[1]] <- as.character(cell.proteins$Antibody)

for (i in 1:nrow(cell.proteins)) {
  temp <- read.table(file.path(cell.home, cell.proteins$File[i]),
                     header=TRUE, row.names=NULL, sep='\t',
                     quote='', comment.char='', skip=4)
  temp <- temp[,1:10]
  dimnames(temp)[[2]][5:10] <- c('Sample', 'Mean.Net', 'Mean.Total',
                                 'Median.Net', 'Vol.Bkg', 'Vol.Dust')
  assign(paste('cell', cell.proteins$Antibody[i], sep='.'), temp, 1)
}
rm(i, temp)

print("loaded cell data")

######################################
# load the tumor data

# first locate the list of assays
tumor.home <- "rppaTumorData"
tumor.proteins <- read.table(file.path(tumor.home, 'proteinAssay.tsv'),
                       header=TRUE, sep='\t', as.is=TRUE)
dimnames(tumor.proteins)[[1]] <- as.character(tumor.proteins$Antibody)

for (i in 1:nrow(tumor.proteins)) {
  temp <- read.table(file.path(tumor.home, tumor.proteins$File[i]),
                     header=TRUE, row.names=NULL, sep='\t',
                     quote='', comment.char='', skip=4)
  temp <- temp[,1:11]
  dimnames(temp)[[2]][5:11] <- c('Sample', 'Mean.Net', 'Mean.Total',
                                 'Median.Net', 'Vol.Net', 'Vol.Bkg', 'Vol.Dust')
  assign(paste('tumor', tumor.proteins$Antibody[i], sep='.'), temp, 1)
}
rm(i, temp)

print("loaded tumor data")

######################################
# load the triple-triple data

# first locate the list of assays
triple.home <- "rppaTripleData"
triple.proteins <- read.table(file.path(triple.home, 'proteinAssay.tsv'),
                       header=TRUE, sep='\t', as.is=TRUE)
dimnames(triple.proteins)[[1]] <- as.character(triple.proteins$Antibody)

for (i in 1:nrow(triple.proteins)) {
  temp <- read.table(file.path(triple.home, triple.proteins$File[i]),
                     header=TRUE, row.names=NULL, sep='\t',
                     quote='', comment.char='', skip=4)
  dimnames(temp)[[2]][c(5:7,11,14,16:17)] <- c('Sample', 'Mean.Net', 'Mean.Total',
                                      'Median.Net', 'Vol.Net', 'Vol.Bkg', 'Vol.Dust')

  assign(paste('triple', triple.proteins$Antibody[i], sep='.'), temp, 1)
}
rm(i, temp)

print("loaded triple data")

############################################################################
# work out the appropriate design layouts

temp <- eval(as.name(paste('cell', cell.proteins$Antibody[1], sep='.')))
cell.design <- temp[,1:5]
cell.design$Steps <- rep(c(rep(8:5, 2), rep(4:1, 2)), 40)
temp <- paste('Rep', rep(rep(1:2, each=4), 80), sep='')
cell.design$Series <- factor(paste(as.character(cell.design$Sample), temp, sep='.'))
rm(temp)

temp <- eval(as.name(paste('tumor', tumor.proteins$Antibody[1], sep='.')))
tumor.design <- temp[,1:5]
tumor.design$Steps <- rep(c(7:1, NA), 96)
tumor.design$Series <- tumor.design$Sample
rm(temp)

temp <- eval(as.name(paste('triple', triple.proteins$Antibody[1], sep='.')))
triple.design <- temp[,1:5]
triple.design$Steps  <- rep(c(6:1), 288)
triple.design$Series <- rep(paste('Ser', 1:288, sep=''), each=6)
rm(temp)

print("loaded designs")

############################################################################
# bundle up the list of files to be preocessed so we can automate it later
temp <- rep(c('cell', 'tumor', 'triple'),
            times=c(nrow(cell.proteins),
              nrow(tumor.proteins),
              nrow(triple.proteins)))
temp2 <- c(as.character(cell.proteins$Antibody),
           as.character(tumor.proteins$Antibody),
           as.character(triple.proteins$Antibody))
items <- data.frame(Set=temp, Ab=temp2)
rm(temp, temp2)

############################################################################
plotCloud <- function(set, ab, quant, version) {
  data <- eval(as.name(paste(set, ab, sep='.')))
  fit <-  eval(as.name(paste(set, ab, 'fit', quant, version, sep='.')))
  xx <- seq(-15, 15, length=300)
  yy <- fit$p.alpha + fit$p.beta*ea(fit$p.gamma*xx)
  plot(xx, yy, type='l', xlab='Log Concentration', ylab='Intensity',
       main=paste(set, ab, quant, version),
       ylim=c(min(data[,quant], na.rm=TRUE), max(data[,quant], na.rm=TRUE)))
  points(fit$fitX, data[,quant])
  invisible(fit)
}


############################################################################
quants   <- c("Mean.Total", "Mean.Net")

# note: version 0.80 includes a hidden global variable called
# .centerDilutionbalance to control whether or not the dilution
# steps are centered at 0. We have checked that setting this value
# to TRUE gives the same numeric results as version 0.75

# versions 0.12, 0.20 require us to define these functions...
lp <- function(p) log(p/(1-p))
ea <- function(x) exp(x)/(1+exp(x))



for (i in 1:nrow(cell.proteins)) {
  temp <- RPPA(cell.proteins$File[i], path=cell.home)
  assign(paste('rppa.cell', cell.proteins$Antibody[i], sep='.'), temp, 1)
}
steps <- rep(c(rep(8:5, 2), rep(4:1, 2)), 40) - 4.5
rep.temp <- factor(paste('Rep', rep(rep(1:2, each=4), 80), sep=''))
series <- factor(paste(as.character(rppa.cell.AKT@data$Sample),
                             as.character(rep.temp),
                             sep='.'))
rppa.cell.design <- RPPADesign(rppa.cell.AKT, steps=steps, series=series)
rm(steps, rep.temp,series)

for (i in 1:nrow(tumor.proteins)) {
  temp <- RPPA(tumor.proteins$File[i], path=tumor.home)
  assign(paste('rppa.tumor', tumor.proteins$Antibody[i], sep='.'), temp, 1)
}
rppa.tumor.design <- RPPADesign(rppa.tumor.ERK2, grouping="blockSample",
                                center=TRUE, controls=list("neg con", "pos con"))

for (i in 1:nrow(triple.proteins)) {
  temp <- RPPA(triple.proteins$File[i], path=triple.home)
  assign(paste('rppa.triple', triple.proteins$Antibody[i], sep='.'), temp, 1)
}
rppa.triple.design <- RPPADesign(rppa.triple.ACTB)
rm(i,temp)

ver <- packageDescription('SuperCurve', fields='Version')
#ver <- '0.93n'
print("slide, mean squared error, trimmed (10%) mse")
for (quant in quants) {
  print(paste('quant', quant))
  for (i in 1:nrow(items)) {
	itemName <- paste('rppa', items$Set[i], items$Ab[i], sep='.')
    item <- eval(as.name(itemName))
    design <- eval(as.name(paste('rppa', items$Set[i], 'design', sep='.')))
    temp <- try(RPPAFit(item, design, quant, ignoreNegative=FALSE,
                    method='nls', warnLevel=-1), TRUE)
    if(is(temp, 'try-error')) {
	    print(paste(items$Set[i], items$Ab[i], quant, ver, 'FAILED'))
	    next
    }
    assign(paste(items$Set[i], items$Ab[i], 'fit', quant, ver, sep='.'), temp, 1)
    meansq <- mean(resid(temp)^2, na.rm=TRUE)
    tmeansq <- mean(resid(temp)^2, na.rm=TRUE, trim = 0.1)
    par(bg='white')
    plot(temp, xlim=c(-15,15), main=paste(items$Set[i], items$Ab[i], quant, ver),
         sub=paste('Mean square residuals =', round(meansq)))
    if (FALSE) {
	    dev.copy(png, file.path(paste(items$Set[i], items$Ab[i], quant, ver, 'png', sep='.')),
	             width=640, height=640)
	    dev.off()
    }
         
    print(paste(items$Set[i], items$Ab[i], quant, round(meansq), round(tmeansq)))
  }
}
rm(i, quant, itemName, item, design, meansq, tmeansq)

############################################################################
versions <- ver
rm(ver)


# cell line replicates

cell.mediff <- array(NA, dim = c(length(versions),
                           length(quants),
                           nrow(cell.proteins)),  
                   dimnames = list(versions,
                       quants,
                       as.character(cell.proteins$Antibody)))
for (ab in cell.proteins$Antibody) {
  for (quant in quants) {
    for (ver in versions) {
      temp <- try(eval(as.name(paste('cell', ab, 'fit', quant, ver, sep='.'))))
      if(!is(temp, 'try-error')) {
	    x <- temp@concentrations
        rep1 <- which(regexpr("Rep1", names(x)) > 0)
        rep2 <- which(regexpr("Rep2", names(x)) > 0)
        cell.mediff[ver, quant, ab] <-
          median(abs(x[rep1] - x[rep2]))
      }
    }
  }
}
rm(ab, quant, ver, temp, rep1, rep2, x)
print("Difference between replicates")
print(cell.mediff)


