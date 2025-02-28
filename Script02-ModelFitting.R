## -----------------------------------------------------------------------------
## Script02-ModelFitting
## This script is designed to fit the relational event model including: 
## - A set of endogenous covariates selected from possible model formulations; 
## - Monadic random effects for insect and plant invasiveness; 
## - Monadic random effect for country invasibility;
## - Dyadic random effect for species co-invasion;

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
load("output/output01-Environment-Model-Fitting.RData")
load("source/modtv.RData")

## -----------------------------------------------------------------------------
library(mgcv)

################################################################################
## -------------------------------------------------------------------------- ##
################################################################################

## -----------------------------------------------------------------------------
re="-1+s(ins,by=Lins,bs='re')+s(plt,by=Lplt,bs='re')+
    s(cc,by=Lc,bs='re')+s(ls,by=Lls,bs='re')"
colnames(dat.gam)[1] <- "event"

## -----------------------------------------------------------------------------
gam.fit <- bam(as.formula(paste0(as.character(modtv[8]),re)),
           family="binomial"(link = 'logit'),
           method="fREML", data=dat.gam)

## -----------------------------------------------------------------------------
s.gam.fit <- summary(gam.fit)

## -----------------------------------------------------------------------------
save(dat.gam, s.gam.fit, gam.fit,
     file=paste0("output/output02-ModelFormulation", 8, ".RData"))
