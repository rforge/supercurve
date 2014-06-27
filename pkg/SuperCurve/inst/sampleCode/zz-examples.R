###
### $Id$
###

if(!exists("NET")) {
  NET <- TRUE
}

if (NET) {
  measure <- "Mean.Net"
  type <- "net"
} else {
  measure <- "Mean.Total"
  type <- "total"
}

extdata.dir <- system.file("extdata", package="SuperCurve")

######################################################################
# tumor data with 3 antibodies

tABG <- matrix(NA, 3, 3)
dimnames(tABG) <- list(c('ERK2', 'GSK3', 'JNK'), c('alpha', 'beta', 'gamma'))
path <- file.path(extdata.dir, "rppaTumorData")
erk2 <- RPPA("ERK2.txt", path=path)
tDesign <- RPPADesign(erk2, grouping="blockSample",
                      center=TRUE, controls=list("neg con", "pos con"))
sn <- seriesNames(tDesign)
tValues <- matrix(NA, length(sn), 3)
dimnames(tValues) <- list(substring(sn, 1, regexpr('\\.', sn)-1),
                          c('ERK2', 'GSK3', 'JNK'))
rm(sn)

fit <- RPPAFit(erk2, tDesign, measure=measure, method='nls', verbose=TRUE)
tABG['ERK2',] <- coef(fit)
tValues[, 'ERK2'] <- fit@concentrations

gsk3 <- RPPA("GSK3.txt", path=path)
fit <- RPPAFit(gsk3, tDesign, measure=measure, method='nls', verbose=TRUE)
tABG['GSK3',] <- coef(fit)
tValues[, 'GSK3'] <- fit@concentrations

jnk <- RPPA("JNK.txt", path=path)
fit <- RPPAFit(jnk, tDesign, measure=measure, method='nls', verbose=TRUE)
tABG['JNK',] <- coef(fit)
tValues[, 'JNK'] <- fit@concentrations

write.table(tABG, paste("modelParameters-tumor", type, "krc.tsv", sep="-"),
            row.names=TRUE, col.names=NA, sep="\t", quote=FALSE)
write.table(tValues, paste("fittedValues-tumor", type, "krc.tsv", sep="-"),
            row.names=TRUE, col.names=NA, sep="\t", quote=FALSE)


##OPTIONAL: save(erk2, jnk, gsk3, tDesign, file="rppaTumor.RData")

######################################################################
# 40 cell lines with 3 antibodies

ABG <- matrix(NA, 3, 3)
dimnames(ABG) <- list(c('AKT', 'ERK2', 'CTNNB1'), c('alpha', 'beta', 'gamma'))
path40 <- file.path(extdata.dir, "rppaCellData")
akt <- RPPA("AKT.txt", path=path40)
# The design here does not follow any of our standard shorthands, since it has
# interleaved 8-step dilution replicates contained in a single 4x4 subgrid
steps <- rep(c(rep(8:5, 2), rep(4:1, 2)), 40) - 4.5
rep.temp <- factor(paste('Rep', rep(rep(1:2, each=4), 80), sep=''))
series <- factor(paste(as.character(akt@data$Sample),
                             as.character(rep.temp),
                             sep='.'))
design40 <- RPPADesign(akt, steps=steps, series=series)
rm(steps, rep.temp,series)
sn <- seriesNames(design40)
cvalues <- matrix(NA, length(sn), 3)
dimnames(cvalues) <- list(  substring(sn, 1, regexpr('\\.', sn)-1),
                          c('AKT', 'ERK2', 'CTNNB1'))
rm(sn)

fit <- RPPAFit(akt, design40, measure=measure, method='nls', verbose=TRUE)
ABG['AKT',] <- coef(fit)
cvalues[, 'AKT'] <- fit@concentrations

c.erk2 <- RPPA("ERK2no2.txt", path=path40)
fit <- RPPAFit(c.erk2, design40, measure=measure, method='nlrob', verbose=TRUE)
ABG['ERK2',] <- coef(fit)
cvalues[, 'ERK2'] <- fit@concentrations

ctnnb1 <- RPPA("Bcatenin40breastcelllines.txt", path=path40)
fit <- RPPAFit(ctnnb1, design40, measure=measure, method='nlrob', verbose=TRUE)
ABG['CTNNB1',] <- coef(fit)
cvalues[, 'CTNNB1'] <- fit@concentrations

write.table(ABG, paste("modelParameters-40", type, "krc.tsv", sep="-"),
            row.names=TRUE, col.names=NA, sep="\t", quote=FALSE)
write.table(cvalues, paste("fittedValues-40", type, "krc.tsv", sep="-"),
            row.names=TRUE, col.names=NA, sep="\t", quote=FALSE)


##################

folded.values <- (cvalues[2*(1:40),] + cvalues[2*(1:40)-1,])/2
divergence <- (cvalues[2*(1:40),] - cvalues[2*(1:40)-1,])
summary(divergence)
topper <- scale(cvalues[2*(1:40),])
bottom <- scale(cvalues[2*(1:40)-1,])
core <- diag(t(topper) %*% bottom / 39) # correlations between replicates are > 0.98
rm(bottom, topper)

rep40 <- rbind(summary(divergence), paste("Correl.:", format(core, digits=4)))
write.table(rep40, paste("replicates-40", type, "krc.tsv", sep="-"),
            row.names=TRUE, col.names=NA, sep="\t", quote=FALSE)

save(c.erk2, akt, ctnnb1, design40, file="rppaCell.RData")

##################
if(FALSE){
akt.fit <- RPPAFit(akt, design40, measure=measure, method='q', verbose=TRUE)

temp.df <- data.frame(fitX=fitted(akt.fit, 'x'),
                      fitY=fitted(akt.fit), # y is default
                      res=residuals(akt.fit),
                      ares=abs(residuals(akt.fit)),
                      linres=residuals(akt.fit, type='l'),
                      raw=akt.fit@rppa@data[, measure])

lo <- loess(ares~fitX, data=temp.df)
ox <- order(lo$x)
plot(lo)
lines(lo$x[ox], lo$fitted[ox], lwd=2)

M <- sqrt(pi/2)
attach(temp.df)
plot(fitX, raw)
lines(fitX[ox], fitY[ox], lwd=2)
lines(fitX[ox], (fitY+M*lo$fitted)[ox], col='red')
lines(fitX[ox], (fitY-M*lo$fitted)[ox], col='red')
lines(fitX[ox], (fitY+2*M*lo$fitted)[ox], col='purple')
lines(fitX[ox], (fitY-2*M*lo$fitted)[ox], col='purple')
detach()

attach(temp.df)
plot(fitX, raw)
lines(sort(fitX), sort(fitY), lwd=2)
plot(fitX, res)
plot(fitX, linres)
plot(fitY, res)
plot(fitY, linres)
plot(raw, linres)
plot(raw, res)
plot(raw, abs(res))
qqnorm(res)
qqline(res)
qqnorm(linres)
qqline(linres)
detach()






r2 <- tapply(resid(akt.fit), list(names(akt.fit@design)), function(x){sum(x^2)})
r2 <- r2[names(akt.fit@ss.ratio)]
v2 <- tapply(fitted(akt.fit), list(names(akt.fit@design)), var)
v2 <- v2[names(akt.fit@ss.ratio)]


plot(akt.fit@concentrations, r2/v2)
plot(akt.fit@concentrations, akt.fit@ss.ratio)

plot(akt.fit@concentrations, r2/v2, ylim=c(0, 10))

plot(akt.fit@ss.ratio, r2)

plot(akt.fit@ss.ratio, r2/v2, ylim=c(0, 4))

data.frame(con=akt.fit@concentrations, ssr=akt.fit@ss.ratio, r2, v2, r2/v2)[r2/v2 >10,]
tdf <- data.frame(con=akt.fit@concentrations, ssr=akt.fit@ss.ratio, r2, v2, r2/v2)
tdf[order(akt.fit@ss.ratio),]

log.akt <- akt
log.akt@data$LogNet <- log(akt@data$Mean.Net)
log.fit <- RPPAFit(log.akt, design40, measure='LogNet', method='q', verbose=TRUE)


cifit <- getConfidenceInterval(akt.fit)

oc <- order(cifit@concentrations)
x <- cifit@concentrations[oc]
y <- cifit@intensities[oc]
l <- cifit@lower[oc]
u <- cifit@upper[oc]

plot(cifit)
#plot(x, ylim=c(min(l), max(u)))
for (i in 1:length(x)) {
  points(x[i], y[i], col='red', pch=16)
  lines(c(l[i], u[i]), rep(y[i], 2), col='red')
}
}#fi

