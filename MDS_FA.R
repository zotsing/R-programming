#MDS and FA
##j.z. project4 for Stats503
setwd("C:\\Downloads\\503")
data2<-read.table("walk_data.csv",header=TRUE,sep=",")
attach(data2)
walk = data2[,1:4]
walks.cor= cor(walk)
walks.rec  = 1./walks.cor

library(cluster);
walk.dist=daisy(walk);
library(stats)
pdf("mds&fa.pdf")
walk.mds=cmdscale(walk.dist,k=3)
fitted.dist=daisy(walk.mds)
sqrt(sum((walk.dist-fitted.dist)^2)/sum(fitted.dist^2))
plot(walk.mds[,1],walk.mds[,2])
text(walk.mds[,1:2],row.names(data2))
## non-linear MDS
library(MASS)
walk.isomds <- isoMDS(walk.dist,k=3)
walk.isomds$stress
## !be careful with zero or negative distances
## create Shepard plot - the transformed dissimilarities vs. the # fitted distances.
par(mfrow=c(1,2));
plot(walk.isomds$points, type = "n");
text(walk.isomds$points, labels = as.character(1:nrow(walk)));
walk.sh <- Shepard(walk.dist, walk.isomds$points);
plot(walk.sh, pch = ".");
lines(walk.sh$x, walk.sh$yf, type = "S");


walk.dist=daisy(walk.rec);

walk.mds=cmdscale(walk.dist,k=3)
fitted.dist=daisy(walk.mds)
sqrt(sum((walk.dist-fitted.dist)^2)/sum(fitted.dist^2))
plot(walk.mds[,1],walk.mds[,2])
text(walk.mds[,1:2],row.names(walks.rec))



## non-linear MDS
library(MASS)
walk.isomds <- isoMDS(walk.dist,k=3)
walk.isomds$stress
## !be careful with zero or negative distances
## create Shepard plot - the transformed dissimilarities vs. the # fitted distances.
par(mfrow=c(1,2));
plot(walk.isomds$points, type = "n");
text(walk.isomds$points, labels = as.character(1:nrow(walk)));
walk.sh <- Shepard(walk.dist, walk.isomds$points);
plot(walk.sh, pch = ".");
lines(walk.sh$x, walk.sh$yf, type = "S");


#FA
datanew = transform(data[,1:4],diff = propel/stride)
walks.cor = cor(datanew)

## next you use this matrix for factorial analysis (+ use rotation="varimax", because the one that was in the example is no
#longer used in a new version of R)
walks.mle <- factanal(cov = walks.cor,factors=2,method="mle",rotation="none")
## get loadings

walks.loadings=loadings(walks.mle)

walks.loadings
## get variable names
walks.names <- names(datanew)
## plot
plot(walks.loadings[,1:2],type="n",xlab="Factor 1",ylab="Factor2", xlim=c(-1,1))
text(walks.loadings[,1:2],walks.names)
abline(v=0,lty=2)
abline(h=0,lty=2) 

dev.off()
