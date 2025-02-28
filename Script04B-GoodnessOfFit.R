## -----------------------------------------------------------------------------
## Script04B-GoodnessOfFit
## This script is designed to evaluate the GOF of the fitted model. 

## -----------------------------------------------------------------------------
library(mgcv)
library(mgcViz)
library(RColorBrewer)
library(gridExtra)
library(sde)
library(Matrix)
library(expm)
library(gratia)
library(e1071)
library(sde)

## -----------------------------------------------------------------------------
pal.blue <- brewer.pal(9, "Blues")
pal.yellow <- brewer.pal(9, "YlOrBr")
pal.greys <- brewer.pal(9, "Greys")

## -----------------------------------------------------------------------------
load(file=paste0("output/output02-ModelFormulation", 8, ".RData"))
# load("source/BB10.RData")
source("Script04A-GOF-Functions.R")

## -----------------------------------------------------------------------------
## Colonial Ties
coefficients(gam.fit)[1]
GOF_col <- GOF_univariate(gam.fit, 1)
GOF_col[[1]]

## -----------------------------------------------------------------------------
## Climatic Dissimilarity
coefficients(gam.fit)[2]
GOF_temp <- GOF_univariate(gam.fit, 2)
GOF_temp[[1]]

## -----------------------------------------------------------------------------
## Log-Distance
coefficients(gam.fit)[3:12]
GOF_dist <- GOF_multivariate(gam.fit, index = 3:12, BB.stat = BB10[[2]])
GOF_dist[[1]]

## -----------------------------------------------------------------------------
seq.s.complete <- seq(0, 1, length.out=nrow(dat.gam))
n.e <- nrow(dat.gam)

pdf("gof-dist.pdf", width=11, height=8)
plot(
  gam.fit$model$stp,
  apply(GOF_dist[[2]], 1, function(x) crossprod(x,x)),
  type="l", 
  lwd=2,
  ylim= c(0,10), 
  col=pal.blue[8], 
  xlab="Time (Original Scale)", 
  ylab="Squared Norm of the Martingale-Residual Process", 
  main="Goodness of Fit of Distance"
)
for (iter in 1:100){
  lines(
    gam.fit$model$stp,
    BB10[[1]][,iter], 
    col=pal.blue[4], lwd=0.3, 
    lty=3)
}
lines(
  gam.fit$model$stp,
  apply(GOF_dist[[2]], 1, function(x) crossprod(x,x)),
  type="l", 
  lwd=2, col=pal.blue[8])
abline(v=1961, col=pal.yellow[8], lty=2, lwd=1.5)
abline(v=1983, col=pal.yellow[8], lty=2, lwd=1.5)
abline(v=1993, col=pal.yellow[8], lty=2, lwd=1.5)
abline(v=2004, col=pal.yellow[8], lty=2, lwd=1.5)

dev.off()

## -----------------------------------------------------------------------------
## Log-Trade
coefficients(gam.fit)[13:22]
GOF_trade <- GOF_multivariate(gam.fit, index = 13:22, BB.stat = BB10[[2]])
GOF_trade[[1]]

## -----------------------------------------------------------------------------
BBglobal_cov <- BB.simulator(list(1,2,3:12,13:22))
GOF_pvalue_global(gam.fit, list(1,2,3:12,13:22), BBglobal_cov)

save.image("output/output04-GoodnessOfFit.RData")
