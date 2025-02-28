## -----------------------------------------------------------------------------
## Script01-Envinronment-Model-Fitting
## This script is designed to create an environment 
## containing the necessary objects for model fitting 
## with a particular focus on matrix random-effect matrices
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
library(mgcv)
load("source/output00-LoadingSourceData.RData")
load("source/output01-BimodalCaseControlDataset.RData")
load("source/modtv.RData")

################################################################################
## -------------------------------------------------------------------------- ##
################################################################################

## -----------------------------------------------------------------------------
lifeform <- c("Insects", "Vascular plants")
lf <- subset[subset$LifeForm %in% lifeform,c(1,9,10,12)]
lf <- lf[order(lf$FirstRecord),]
nrow(lf)
lf_ins <- lf[lf$LifeForm=="Insects",]
nrow(lf_ins)
lf_plt <- lf[lf$LifeForm=="Vascular plants",]
nrow(lf_plt)

t <- which(FR$FirstRecord < 1880 & FR$LifeForm %in% lifeform)
FRbefore <- FR[t,c("FirstRecord",
                   "Taxon","Region",
                   "LifeForm")]

## POSSIBLE SPECIES ####
spec <- unique(lf$Taxon)
s <- length(spec)
native$sp.num <- match(native$Species, spec)
lf$sp.num <- match(lf$Taxon, spec)
spec_ins <- unique(lf$Taxon[lf$LifeForm=="Insects"])
(s_ins <- length(spec_ins))
s_ins.num <- match(spec_ins, spec)
spec_plt <- unique(lf$Taxon[lf$LifeForm=="Vascular plants"])
(s_plt <- length(spec_plt))
s_plt.num <- match(spec_plt, spec)

## POSSIBLE REGIONS ####
# All the regions codified in the matrix distances
# are considered as possible
reg <- colnames(distances)
reg.lf <- unique(lf$Region)
reg.lf.num <- match(reg.lf,reg)
(r <- length(reg.lf))
native$c.num <- match(native$Region, reg)
lf$c.num <- match(lf$Region, reg)

# SELECT ONLY CODIFIED (S-R) FROM NATIVE
nat <- native[,c("Species", "Region","sp.num","c.num")]
nat <- na.omit(nat)
head(nat)
nrow(nat[nat$sp.num %in% s_ins.num,])
nrow(nat[nat$sp.num %in% s_plt.num,])

# ORDER THE DATA W.R.T. TIME
lf <- lf[order(lf$FirstRecord),]

## -----------------------------------------------------------------------------
insects <- dat.gam$plant1==99
  
## INSECTS RANDOM FACTOR ####
ins1 <- dat.gam$insect1
ins2 <- dat.gam$insect2
ins <- factor(c(ins1,ins2))
dim(ins) <- c(length(ins1),2)
# Related by matrix
Lins <- matrix(1,length(ins1),2); Lins[,2] <- -1
  
## PLANT RANDOM FACTOR ####
plt1 <- dat.gam$plant1
plt2 <- dat.gam$plant2
plt <- factor(c(plt1,plt2))
dim(plt) <- c(length(plt1),2)
# Related by matrix
Lplt <- matrix(1,length(plt1),2); Lplt[,2] <- -1
  
## COUNTRY RANDOM FACTOR ####
c1 <- dat.gam$c1
c2 <- dat.gam$c2
cc <- factor(c(c1,c2))
dim(cc) <- c(length(c1),2)
# Related by matrix
Lc <- matrix(1,length(c1),2); Lc[,2] <- -1
  
## SENDER CO-INVASION RANDOM MATRIX ####
ls0 <- matrix(NA, nrow=nrow(dat.gam), ncol=6)
colnames(ls0) <- c("sp1", "ls1", "sp2", "ls2","mixed1", "mixed2")
ls0[insects,] <- cbind(dat.gam$insect1[insects],dat.gam$ls1[insects],
                       dat.gam$insect2[insects],dat.gam$ls2[insects],
                       dat.gam$ls1[insects]%in%spec_plt, 
                       dat.gam$ls2[insects]%in%spec_plt)
ls0[(!insects),] <- cbind(dat.gam$plant1[(!insects)],dat.gam$ls1[(!insects)],
                          dat.gam$plant2[(!insects)],dat.gam$ls2[(!insects)],
                          dat.gam$ls1[(!insects)]%in%spec_ins, 
                          dat.gam$ls2[(!insects)]%in%spec_ins)

ls1 <- as.character(interaction(ls0[,1], ls0[,2], drop = TRUE))
ls1[ls0[,2]=="Novelty"] <- "Novelty"
ls2 <- as.character(interaction(ls0[,3], ls0[,4], drop = TRUE))
ls2[ls0[,4]=="Novelty"] <- "Novelty"
ls <- c(ls1,ls2)
ls[ls %in% names(table(ls))[table(ls)==1]] <- "Rare interaction"
ls <- factor(ls)
dim(ls) <- c(length(ls1),2)
Lls <- matrix(1,length(ls1),2); Lls[,2] <- -1

## -----------------------------------------------------------------------------
# CHECKING
# sum(ins[ins[,1]!=99,1]==lf_ins$Taxon)
# sum(plt[plt[,1]!=99,1]==lf_plt$Taxon)
# sum(cc[ins[,1]!=99,1]==lf_ins$Region)
# sum(cc[plt[,1]!=99,1]==lf_plt$Region)
# sum(cc[,1]==lf$Region)

## -----------------------------------------------------------------------------
save.image("output/output01-Environment-Model-Fitting.RData")
