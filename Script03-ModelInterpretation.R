## -----------------------------------------------------------------------------
## Script03-ModelInterpretation
## This script is designed to interpret the results concerning:
## 1) Parametric Estimation
## 2) Non-Parametric Estimation

## -----------------------------------------------------------------------------
library(mgcv)
library(mgcViz)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(igraph)
library(ggsci)
library(RColorBrewer)
library(png)

## -----------------------------------------------------------------------------
pal.blue <- brewer.pal(9, "Blues")
pal.yellow <- brewer.pal(9, "YlOrBr")

## -----------------------------------------------------------------------------
load(file=paste0("output/output02-ModelFormulation", 8, ".RData"))
load("output/output01-Environment-Model-Fitting.RData")

## -----------------------------------------------------------------------------
s.gam.fit

################################################################################
## -------------------------------------------------------------------------- ##
################################################################################

## 1) Parametric Estimation ####

## -----------------------------------------------------------------------------
### Colonial Ties ####
gam.fit$coefficients[1]

### Climatic Dissimilarity ####
gam.fit$coefficients[2]

## -----------------------------------------------------------------------------
### Log-Distance ####
b <- getViz(gam.fit)
od <- plot(sm(b, 1))
# range(o$data$fit$y)
# 1-exp(range(o$data$fit$y)[1])
# 1-exp(range(o$data$fit$y)[2])

pdf("log_dist.pdf", width = 11, height = 8)
od + l_fitLine(colour = pal.blue[8], size=1) + 
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  ylim(-0.5, 0) +
  l_ciLine(mul = 5, colour = 1, linetype = 2, size=1) + 
  labs(title = "Time-Varying Effect of Log-Distance",
       subtitle = "Insects and Vascular Plants",
       x = "Time (year)",
       y = "Time-varying coefficient") +
  theme(axis.text.x = element_text(color = "grey20", 
                                   size = 18, angle = 90, 
                                   hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", 
                                   size = 18, angle = 0, 
                                   hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", 
                                    size = 20, angle = 0, 
                                    hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", 
                                    size = 20, angle = 90, 
                                    hjust = .5, vjust = .5, face = "plain"),
        title = element_text(color = "black", 
                             size = 25, angle = 0, 
                             hjust = .5, vjust = .5, 
                             family="serif", face = "bold"))
dev.off()


### Log-Trade ####
ot <- plot(sm(b, 2))
# range(o$data$fit$y)
# exp(range(o$data$fit$y)[2])
# exp(range(o$data$fit$y)[1])

pdf("log_trade.pdf", width = 11, height = 8)
ot + l_fitLine(colour = pal.yellow[4], size=1) + 
  l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  ylim(-0.1, 0.6) +
  l_ciLine(mul = 5, colour = 1, linetype = 2, size=1) + 
  labs(title = "Time-Varying Effect of Log-Trade",
       subtitle = "Insects and Vascular Plants",
       x = "Time (year)",
       y = "Time-varying coefficient") +
  theme(axis.text.x = element_text(color = "grey20", 
                                   size = 14, angle = 90, 
                                   hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", 
                                   size = 14, angle = 0, 
                                   hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "grey20", 
                                    size = 20, angle = 0, 
                                    hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", 
                                    size = 20, angle = 90, 
                                    hjust = .5, vjust = .5, face = "plain"),
        title = element_text(color = "black", 
                             size = 25, angle = 0, hjust = .5, vjust = .5, 
                             family="serif", face = "bold"))
dev.off()

## -----------------------------------------------------------------------------
### Random Effects ####
re.gam <- coef(gam.fit)[23:length(coef(gam.fit))]

## -----------------------------------------------------------------------------
#### INSECT INVASIVENESS ####
as.numeric(gam.vcomp(gam.fit)[3,1])
re.ins <- re.gam[1:length(levels(ins))]
names(re.ins) <- levels(ins)
sort(re.ins, decreasing = TRUE)[1:5]

## -----------------------------------------------------------------------------
#### PLANT INVASIVENESS ####
as.numeric(gam.vcomp(gam.fit)[4,1])
re.plt <- re.gam[(length(levels(ins))+1):
                   (length(levels(ins))+length(levels(plt)))]
names(re.plt) <- levels(plt)
sort(re.plt, decreasing = TRUE)[1:5]

# species.re <- data.frame(Insect=names(sort(re.ins, decreasing = TRUE)[1:5]), 
#            Plants=names(sort(re.plt, decreasing = TRUE)[1:5]))
# colnames(species.re) <- c("Insects","Vascular plants")
# xtable(species.re)

## -----------------------------------------------------------------------------
#### COUNTRY POPULARITY ####
re.countries <- re.gam[(length(levels(ins))+length(levels(plt))+1):
                         (length(levels(ins))+length(levels(plt))+length(levels(cc)))]
names(re.countries) <- levels(cc)

names(sort(re.countries, decreasing=TRUE)[1:2]) %in% 
  names(which(col[,"United Kingdom"]==1))

re.countries["United States"]
re.countries["New Zealand"]
re.countries["South Africa"]
sort(re.countries, decreasing = TRUE)[1:40]

pdf("insects-map.pdf", width = 12)
ddf <- data.frame(country=names(re.countries), 
                  Popularity=re.countries)
rownames(ddf) <- NULL
world <- map_data("world")
ddf[ddf$country=="United States","country"]<-"USA"
names(re.countries)[which(!names(re.countries) %in% unique(world$region))]

world %>%
  merge(ddf, by.x = "region", by.y = "country", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Popularity)) + geom_polygon() + 
  labs(title = "Regions' Popularity for Insects and Vascular Plants",
       x = "Longitude",
       y = "Latitude") +
  theme(axis.title.x = element_text(color = "grey20", 
                                    size = 20, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "grey20", 
                                    size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        title = element_text(color = "black", 
                             size = 25, angle = 0, hjust = .5, vjust = .5, 
                             family="serif", face = "bold"),
        legend.title = element_text(color = "grey20", 
                                    size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"))
dev.off()

## -----------------------------------------------------------------------------
#### SPECIES CO-INVASION ####
re.last.species <- re.gam[(length(levels(ins))+length(levels(plt))+length(levels(cc))+1):
                            length(re.gam)]
names(re.last.species) <- levels(ls)

ls.lev <- setdiff(levels(ls), c("Rare interaction", "Novelty"))
comb <- matrix(NA, nrow=length(ls.lev), ncol=4)
for (k in 1:length(ls.lev)){
  comb[k,1] = unlist(strsplit(ls.lev[k], "[.]"))[1]
  comb[k,2] = unlist(strsplit(ls.lev[k], "[.]"))[2]
  comb[k,3] = ls.lev[k]
  comb[k,4] = re.last.species[ls.lev[k]]
}
comb <- as.data.frame(comb)
comb$t1 <- ifelse(comb$V1 %in% spec_ins,1,0)
comb$t2 <- ifelse(comb$V2 %in% spec_ins,1,0)
comb$mixed <- comb$t1 != comb$t2
comb <- comb[order(abs(as.numeric(comb$V4)),decreasing=TRUE),]
comb <- comb[abs(as.numeric(comb$V4))>=log(1.5),]
sum(comb$mixed==1)/nrow(comb)
comb[which(comb$mixed==1,),]

pdf("species-coinvasion.pdf", width=12, height = 12)

img1 <- readPNG("source_data/Frankliniella_occidentalis.png")
img2 <- readPNG("source_data/Anoplolepis_gracilipes.png")
img3 <- readPNG("source_data/Syzygium_cumini.png")
img4 <- readPNG("source_data/Chromolaena_odorata.png")
img5 <- readPNG("source_data/Sesbania_sericea.png")

vertices <- data.frame(vertices=unique(c(comb$V1, comb$V2)),
                       taxa=ifelse(unique(c(comb$V1, comb$V2)) %in% spec_ins,1,0))
vertices <- vertices[order(vertices$taxa, decreasing = TRUE),]
g <- graph_from_data_frame(comb[,1:2], vertices=vertices$vertices,
                           directed=TRUE)
V(g)$type <- vertices$taxa
V(g)$color <- ifelse(vertices$taxa,pal.yellow[4],pal.blue[8])
lay <- layout_in_circle(graph = g, order=1:nrow(vertices))

# V(g)$raster = replicate(vcount(g), img1, simplify=FALSE)

E(g)$weight <- abs(as.numeric(comb[,4]))
ecol=rep("gray",ecount(g))
ecol[comb[,4]>0]="black"

plot(g, edge.color=ecol,edge.width=E(g)$weight*1.5, vertex.color=V(g)$color,
     vertex.size=8, layout=lay, vertex.label=NA, edge.curved=0.2,
     edge.arrow.size = 0.25, xlim=c(-2.3,2.3))
#vertex.shape="raster")

x = lay[,1]*1.5
y = lay[,2]*1.5

#create vector of angles for text based on number of nodes 
# (flipping the orientation of the words half way around so none appear 
# upside down)
angle = ifelse(atan(-(lay[,1]/lay[,2]))*(180/pi) < 0,  
               90 + atan(- (lay[,1]/lay[,2]))*(180/pi), 270 + atan(-lay[,1]/lay[,2])*(180/pi))

#Apply the text labels with a loop with angle as srt
for (i in 1:length(x)) {
  if((V(g)$name[i]%in% names(sort(re.ins,decreasing = TRUE)[1:5]))){
    text(x=x[i], y=y[i], labels=V(g)$name[i], adj=NULL, 
         pos=NULL, cex=.8, col=pal.yellow[4], srt=angle[i], xpd=T)
  } else {
    if ((V(g)$name[i]%in% names(sort(re.plt,decreasing = TRUE)[1:5]))){
      text(x=x[i], y=y[i], labels=V(g)$name[i], adj=NULL, 
           pos=NULL, cex=.8, col=pal.blue[8], srt=angle[i], xpd=T)
    } else {
      text(x=x[i], y=y[i], labels=V(g)$name[i], adj=NULL, 
           pos=NULL, cex=.6, col="black", srt=angle[i], xpd=T)
    }
  }
}
legend("topright", y=lay[1,2]*1.3,legend = c("Positive Edge", "Negative Edge",
                                             "Insect Node", "Plant Node"),
       lwd=c(3,3,NA,NA),pch=c(NA,NA,19,19), col=c("black","gray", 
                                                  pal.yellow[4], pal.blue[8]),
       cex=1.2)


rasterImage(img1, xleft = x[1]+0.4, xright = x[1]+0.7, ytop = y[1]-0.05, 
            ybottom = y[1]+0.25)
rasterImage(img2, xleft = x[14]-0.15, xright = x[14]+0.15, ytop = y[14]+0.65, 
            ybottom = y[14]+0.35)
rasterImage(img3, xleft = x[39]-0.35, xright = x[39]-0.1, ytop = y[39]-0.3, 
            ybottom = y[39]-0.6)
rasterImage(img4, xleft = x[42]-0.15, xright = x[42]+0.15, ytop = y[42]-0.35, 
            ybottom = y[42]-0.65)
rasterImage(img5, xleft = x[44]-0.15, xright = x[44]+0.15, ytop = y[44]-0.3, 
            ybottom = y[44]-0.6)
dev.off

################################################################################
## -------------------------------------------------------------------------- ##
################################################################################

## 2) Non-Parametric Estimation ####

at.risk.ins <- at.risk.plt <- at.risk <- NULL
alien.occ <- matrix(0, nrow = s, ncol = r)
rownames(alien.occ) <- spec
colnames(alien.occ) <- reg.lf
for (n.sp in 1:s){
  nat.id <- unique(nat$c.num[nat$sp.num == n.sp])
  possible.to <- setdiff(reg.lf.num,nat.id)
  
  if (n.sp %in% s_ins.num){
    # value 1 for possible events involving insects
    alien.occ[n.sp,reg[possible.to]] <- 1}
  else {
    # value 2 for possible events involving plants
    alien.occ[n.sp,reg[possible.to]] <- 2}
}
sum(alien.occ==1)
sum(alien.occ==2)

for (i in 1:nrow(lf)){
  sub_stp <- lf[lf$FirstRecord==lf[i,"FirstRecord"],c("sp.num", "Region")]
  ni <- nrow(sub_stp)
  for (j in 1:ni){alien.occ[sub_stp[j,1],sub_stp[j,2]] <- 0}
  if (lf[i,1] %in% spec_ins){
    at.risk.ins <- c(at.risk.ins, sum(alien.occ==1))
    at.risk <- c(at.risk, sum(alien.occ==1))
  } else {
    at.risk.plt <- c(at.risk.plt, sum(alien.occ==2))
    at.risk <- c(at.risk, sum(alien.occ==2))
  }
}
insects <- dat.gam$plant1==99

n.e <- nrow(dat.gam)
Lins1 <- matrix(1,n.e,1); Lins2 <- matrix(1,n.e,1);
Lplt1 <- matrix(1,n.e,1); Lplt2 <- matrix(1,n.e,1);
Lc1 <- matrix(1,n.e,1); Lc2 <- matrix(1,n.e,1);
Lls1 <- matrix(1,n.e,1); Lls2 <- matrix(1,n.e,1);

new.data.ev <- data.frame(temp_dist = dat.gam$temp_dist1,
                          log_dist = dat.gam$log_dist1,
                          log_trade = dat.gam$log_trade1,
                          col = dat.gam$col1,
                          stp = dat.gam$stp,
                          ins = ins[,1],
                          Lins = Lins1,
                          plt = plt[,1],
                          Lplt = Lplt1,
                          cc = cc[,1],
                          Lc = Lc1, 
                          ls = ls[,1],
                          Lls = Lls1)
dat.gam$lp1 <- exp(rowSums(predict.gam(gam.fit, type="terms",
                                       newdata = new.data.ev)))
Z1p <- predict.gam(gam.fit, type="lpmatrix",
                   newdata = new.data.ev)

new.data.nv <- data.frame(temp_dist = dat.gam$temp_dist2,
                          log_dist = dat.gam$log_dist2,
                          log_trade = dat.gam$log_trade2,
                          col = dat.gam$col2,
                          stp = dat.gam$stp,
                          ins = ins[,2],
                          Lins = Lins2,
                          plt = plt[,2],
                          Lplt = Lplt2,
                          cc = cc[,2],
                          Lc = Lc2, 
                          ls = ls[,2],
                          Lls = Lls2)
dat.gam$lp2 <- exp(rowSums(predict.gam(gam.fit, type="terms",
                                       newdata = new.data.nv)))
Z2p <- predict.gam(gam.fit, type="lpmatrix",
                   newdata = new.data.nv)

dat.gam$S0 <- dat.gam$lp1 + dat.gam$lp2

baseline.evalutation <- function(dat.new, at.risk){
  den <- dat.new$S0*at.risk/2
  l <- NULL
  for (i in 1:(length(den))){
    l <- c(l, 1/den[i])
  }
  L <- cumsum(l)
  return(data.frame(time=dat.new$stp, hazard=L))
}

pdf("cum-baseline.pdf", width = 11, height=8)
plot(baseline.evalutation(dat.gam, at.risk),
     type="l", 
     xlab="Time (year)",
     ylab="Cumulative Hazard",
     main="Matched and Pooled Estimation of the Cumulative Baseline Hazard", 
     col=1,
     lty=3,
     lwd=2)
lines(baseline.evalutation(dat.gam[insects,],at.risk.ins),
      col=pal.blue[8], 
      lwd=2)
lines(baseline.evalutation(dat.gam[!insects,],at.risk.plt),
      col=pal.yellow[4], 
      lwd=2)
legend("topleft",legend=c("Pooled: Insect + Plants", "Matched: Insects", 
                          "Matched: Vascular plants"),
       lty =c(3,1,1), lwd=c(2,2,2), col=c(1,pal.blue[8],pal.yellow[4]))
dev.off()