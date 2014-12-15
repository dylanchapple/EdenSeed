setwd("~/Desktop/GIS Project")

library(vegan)

species <- read.csv("oacspecies.csv", header=T)

species[is.na(species)] <- 0  ##turns NA into 0 for analysis

colnames(species)
species1 <- species[, 2:14]  
#removing the plot names from the data
head(species1)
plotnames <- as.character(unique(species$Plot))
speciesnames <- colnames(species1)
speciesnames
head(speciesnames)
plotnames
x <- array(species1, dimnames=list(plotnames, speciesnames))

head(species1)

species.dis <- vegdist(x) 
species.dis2 <- vegdist(x, method="bray") 
##creates dissimilarity matrix
species.dis1 <- vegdist(x, binary=TRUE)

head(species.dis)

dis <- read.csv("oacdistance.csv", header=T) ##basic matrix describing a grid of the sample collection points
dis1 <- (dis[,2:3])
coor <- colnames(dis1)

distlabel <- array(dis1, dimnames=list(plotnames, coor))

eucdis <- dist(distlabel)
dist(x, upper = TRUE)
m <- as.matrix(dist(x))
as.dist(m)
m
eucdist <- dist(distlabel)
eucdis <- dist(distlabel)
plot(species.dis~eucdis)
m2 <- lm(species.dis~eucdis)
abline(m2)
summary(m2)

m2 <- lm(species.dis2~eucdis)
plot(species.dis2~eucdis)
abline(m2)
summary(m2)

pdf("oacdistdecayfresh.pdf")
plot(species.dis~eucdis, main="Distance Decay Across 1 Grid", ylab="Bray-Curtis Dissimilarity", xlab="Euclidean Distance, Meters")
abline(m2)
dev.off()

plot(species.dis1~eucdis)
n <- lm(species.dis1~eucdis)
abline(n)
summary(n) 

species.dis.log <- log(species.dis)
species.dis1.log <- log(species.dis1)


plot(species.dis1.log~eucdis)
n.log <- lm(species.dis1.log~eucdis)
abline(n.log)
summary(n.log)


Spatial Distances:

dis <- read.csv("oacdistance.csv", header=T) ##basic matrix describing a grid of the sample collection points
dis1 <- (dis[,2:3])
coor <- colnames(dis1)
coor <- as.character(unique(dis$Row.Labels))
distlabel <- array(dis1, dimnames=list(plotnames, coor))

eucdis <- dist(distlabel)
dist(distance, diag = TRUE)
dist(x, upper = TRUE)
m <- as.matrix(dist(x))
as.dist(m)


plot(seed.dis.log~eucdis, main="Distance Decay Across 2 Grids", ylab="Bray-Curtis Dissimilarity", xlab="Euclidean Distance")
m.log <- lm(seed.dis.log~eucdis)
abline(m.log)
summary(m.log)

##results with both sites in:
Call:
  lm(formula = species.dis ~ eucdis)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.50040 -0.13222  0.01876  0.14093  0.54221 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 3.120e-01  1.493e-02   20.90   <2e-16 ***
  eucdis      2.835e-04  1.381e-05   20.53   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.207 on 433 degrees of freedom
Multiple R-squared: 0.4933,  Adjusted R-squared: 0.4921 
F-statistic: 421.5 on 1 and 433 DF,  p-value: < 2.2e-16 


OAC Fresh lm:
  Call:
  lm(formula = species.dis ~ eucdis)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.35354 -0.13030 -0.01895  0.12940  0.33256 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.4450641  0.0434421   10.24   <2e-16 ***
  eucdis      0.0011872  0.0006058    1.96   0.0527 .  
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.1803 on 103 degrees of freedom
Multiple R-squared: 0.03595,  Adjusted R-squared: 0.02659 
F-statistic: 3.841 on 1 and 103 DF,  p-value: 0.05271 
