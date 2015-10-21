# home work 5
#
library(faraway)
data(sat)
data(stackloss)
#summary(sat)
#summary(stackloss)
model1  = lm(total~expend+salary+ratio+takers+I(takers^2),data=sat)
model2  = lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stackloss)
#summary(model1)
#summary(model2)
#plot the residual against fitted values
par(mfrow=c(2,2))
plot(model1$fit, model1$res, xlab="model1 Fitted", ylab="Residuals")
abline(h=0)
plot(model1$fit, abs(model1$res), xlab="model1 Fitted", ylab="|Residuals|")
plot(model2$fit, model2$res, xlab="model2 Fitted", ylab="Residuals")
abline(h=0)
plot(model2$fit, abs(model2$res), xlab="Fitted", ylab="|Residuals|")
#normality
par(mfrow=c(1,2))
qqnorm(model1$residual, ylab="model1 Residuals")
qqline(model1$residual)
qqnorm(model2$residual, ylab="model2 Residuals")
qqline(model2$residual)
# leverage compare to 2(p+1)/n
par(mfrow=c(1,2))
halfnorm(lm.influence(model1)$hat,nlab=5,ylab="Leverages for model1")
abline(h=10/50)
halfnorm(lm.influence(model2)$hat,nlab=5,ylab="Leverages for model2")
abline(h=8/21)
# outlier, studentized residuals
jack <- rstudent(model1)
jack[order(abs(jack),decreasing=TRUE)][1:5]
# Bonferroni critical value, drop one point
qt(1-0.05/50/2,50-6)
jack <- rstudent(model2)
jack[order(abs(jack),decreasing=TRUE)][1:5]
# Bonferroni critical value:
qt(1-0.05/21/2,21-4)
# influential points based on cook distance
par(mfrow=c(1,2))
cook1 <- cooks.distance(model1)
halfnorm(cook1, nlab = 5, ylab="Cook’s distance in model1")
abline(h=6/43)
cook2 <- cooks.distance(model2)
halfnorm(cook2, nlab = 5, ylab="Cook’s distance in model2")
abline(h=4/16)
# three different regression comparison
#ordinary leat square
summary(model1)
#Huber's method
library(MASS)
ghuber1= rlm(total~expend+salary+ratio+takers+I(takers^2),data=sat)
summary(ghuber1)
#least absolute deviation
library(quantreg)
glad1 = rq(total~expend+salary+ratio+takers+I(takers^2),data=sat)
summary(glad1)
summary(model2)
ghuber2= rlm(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stackloss)
summary(ghuber2)
glad2 = rq(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,data=stackloss)
summary(glad2)
#creat new file
coef1 = lm.influence(model1)$coef
fdir  = "D:\\study\\stat500\\"
newsat = transform(coef1,cook=cook1)
write.table(newsat, file=paste(fdir,"sat.j0z.csv",sep=""),append = FALSE, quote = TRUE, sep = ",",row.names = FALSE,col.names = TRUE);
coef2 = lm.influence(model2)$coef
newstackloss = transform(coef2,cook=cook2)
write.table(newstackloss, file=paste(fdir,"stackloss.j0z.csv",sep=""),append = FALSE, quote = TRUE, sep = ",",row.names = FALSE,col.names = TRUE);

# use weights proportional to 1/takers
gw<-lm(total~expend+salary+ratio+takers+I(takers^2),data=sat,weights = takers**(-1)); 
summary(gw)[[4]]

