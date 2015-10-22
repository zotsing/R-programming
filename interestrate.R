setwd("D:\\study\\503")

inter<-read.table("interest-rates.dat",header=TRUE)
pdf("h2interest.pdf")
pairs(inter)

#Load next the necessary R library.

library(MASS)

#Then apply PCA on the correlation matrix by rescaling and centering the 
#inter using the R function princomp.
#singular
pca.inter<-prcomp(scale(inter[,2:11],scale=TRUE,center=TRUE),cor=FALSE)
row.names(inter) = inter[,1]
#Look at the results
 summary(pca.inter)
 #Importance of components:
#Calculate the loadings. Note that R defines loadings without multiplying them by the square root of lambda 
#Note: Be aware that people use the term 'factor loadings' in both ways.

loadings(pca.inter) 

#Calculate next the principal components.

pcs.inter<-predict(pca.inter)

#Plot the first 2 PCs. 

plot(pcs.inter[,1:2],type="n",xlab='1st PC',ylab='2nd PC') 

text(pcs.inter[,1:2],row.names(inter))

biplot(pca.inter,scale=1,row.names(inter)) 


numericdat <- inter[,2:11]
time = inter[,1]
vpcdat <- princomp(numericdat) 
cpcdat <- princomp(numericdat,cor=T) 
n <- vpcdat$n.obs 
p <- length(vpcdat$sdev) 
vload <- vpcdat$load[,1:2] 
vsdev <- vpcdat$sdev[1:2] 
vevals <- n*vsdev*vsdev 
vscores <- vpcdat$scores[,1:2] 
cload <- cpcdat$load[,1:2] 
csdev <- cpcdat$sdev[1:2] 
cevals <- csdev*csdev 
cscores <- cpcdat$scores[,1:2] 
allcevals <- cpcdat$sdev^2 

cat("First two eigenvalues from variances \n") 
print(vevals) 
cat("\n") 
cat("First two eigenvalues from correlations \n") 
print(cevals) 
cat("\n") 
cat("Loadings for first two principal components") 
cat(" from variances \n") 
print(vload) 
cat("\n") 
cat("Loadings for first two principal components") 
cat(" from correlations \n") 
print(cload)
rownames(vscores) <- t(time) 
rownames(cscores) <- t(time) 

plot(vscores[,1],vscores[,2],type="n") 
text(vscores[,1],vscores[,2],t(time)) 
title("First two principal components from variances") 

plot(cscores[,1],cscores[,2],type="n") 
text(cscores[,1],cscores[,2],t(time)) 
title("First two principal components from correlations") 

barplot(allcevals) 
title("Scree diagram from correlations") 
dev.off()
