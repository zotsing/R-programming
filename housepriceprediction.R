# J.z.'s final project for prediction housing price 
library(faraway)
setwd("D:\\study\\stat500\\final")
data =read.table("Exam2.data.j0z.csv",sep=",",head=F);  #
cName<-c("id", "price", "size","bath","halfbath","bed","age","garage","elem");
colnames(data)<-cName;
attach(data)
model1  = lm(price~bath*size+ bed*size+halfbath*size+age+I(age^2)+garage+elem,data=data)
#AIC selection for model1
step(model1)
#find influential points
cook1   = cooks.distance(model1)
halfnorm(cook1,nlab=7,ylab="Cooks distance")
abline(h=4/65)
take1a  = which(cook1>4/65)
model1a = lm(price~bath*size+ bed*size+halfbath*size+age+I(age^2)+garage+elem,data=data,subset=(1:80)[-take1a]);
step(model1a)

#Q2
model2= lm(price ~ size + bed + halfbath + garage + elem + size:bed + size:halfbath, data = data)
model2a = lm(price ~ size + bed + halfbath + I(age^2) + garage + elem + size:bed + size:halfbath,data = data,subset=(1:80)[-take1a])
summary(model2)
summary(model2a)
#calculate BIC 
extractAIC(model2,k=log(80))
extractAIC(model2a,k=log(74))

#Q3
model3 = lm(price~ size*bath + age + elem + bed + halfbath, data = data)
summary(model3)
 #cutoff Bonferroni correction assume 0.95 confidence interval
cutoff = qt(1-0.05/80/2,69)
 # jackknife residuals
 jack3 = rstudent(model3)
 jack3[order(abs(jack3),decreasing=TRUE)][1:5]
  cook3  = cooks.distance(model3)
  out  = jack3*0
  out[abs(jack3)<cutoff]=0    #0 indicat not outlier
  out[abs(jack3)>=cutoff]=1   #1 indicate outlier
#creat table
newsat = transform(data,tres=jack3,out=out,cook=cook3)
write.table(newsat, file=paste("Exam2.out.j0z.csv",sep=","),append = FALSE, quote = TRUE, sep = ",",row.names = FALSE,col.names = TRUE);

#Q4
take4  = which(cook3>4/69);      #influential points
take4a  = which(abs(jack3)>=cutoff); #outliers
#OLS
summary(model3)
#OLS without influential points
model4  = lm(price~ size*bath + age + elem + bed + halfbath, data = data,subset=(1:80)[-take4])
summary(model4)
#OLS without outlier
model4a = lm(price~ size*bath + age + elem + bed + halfbath, data = data,subset=(1:80)[-take4a])
summary(model4a)
#Huber's method
library(MASS)
ghuber= rlm(price~ size*bath + age + elem + bed + halfbath, data = data)
summary(ghuber)
qt(0.975,70)
#Huber's w/o influential points
ghuber4= rlm(price~ size*bath + age + elem + bed + halfbath, data = data,subset=(1:80)[-take4])
summary(ghuber4)
qt(0.975,64)
#w/o outlier
ghuber4a=rlm(price~ size*bath + age + elem + bed + halfbath, data = data,subset=(1:80)[-take4a])
summary(ghuber4a)
qt(0.975,69)

#Q5
which.max(model3$residual)
which.min(model3$residual)
#maximum residual
predict(model3,data[58,],interval="prediction")
#minimum residual
predict(model3,data[8,],interval="prediction")
data[58,]
data[8,]

#Q6
#dataset excluding influential points
model6  = lm(price~ size*bath + age + I(age^2) + elem + bed + halfbath, data = data,subset=(1:80)[-take4])
anova(model6,model4)
summary(model4)
summary(model6)

#Q7
#check residual pattern and constant variance assumption first
par(mfrow=c(2,2))
plot(model4$fit,model4$res,xlab="Fitted for model4",ylab="Residuals")
abline(h=0)
plot(model4$fit,abs(model4$res),xlab="Fitted for model4",ylab="|Residuals|")
plot(model6$fit,model6$res,xlab="Fitted for model6",ylab="Residuals")
abline(h=0)
plot(model6$fit,abs(model6$res),xlab="Fitted for model6",ylab="|Residuals|")
#check normality
par(mfrow=c(1,2))
qqnorm(model4$residual,ylab="model4 Residuals")
qqline(model4$residual)
qqnorm(model6$residual,ylab="model6 Residuals")
qqline(model6$residual)

#Q8
#categorize bathroom first
bathN=factor(data$bath)
model8 = lm(price~ bathN/size + age + elem + bed + halfbath, data = data,subset=(1:80)[-take4])
confint(model8,"bathN1:size",level=.95)
confint(model8,"bathN2:size",level=.95)
confint(model8,"bathN3:size",level=.95)

#Q9
I1 = 5;  #one bathroom#
I2 = 48; #two bathroom#
I3 = 21; #three bathroom #
df  = 62 # degree of freedom
qt = qt(0.975,62)
SE = 31.4*sqrt(1/I2+1/I3)
#CI for the difference between 2 and 3 bathrooms
c((117.909  - 87.663) - qt*SE,(117.909  - 87.663) + qt*SE)



#Q10
#reasonal prediction using model6
predict(model6,data.frame(size=2.0,bath=2,halfbath=1,bed=3,age=0.0,elem="A"),interval="prediction")
#confidence mean using model6
predict(model6,data.frame(size=2.0,bath=2,halfbath=1,bed=3,age=0.0,elem="A"),interval="confidence")

#reasonal prediction using model8
predict(model8,data.frame(size=2.0,bathN="2",halfbath=1,bed=3,age=0.0,elem="A"),interval="prediction")
#confidence mean using model8
predict(model8,data.frame(size=2.0,bathN="2",halfbath=1,bed=3,age=0.0,elem="A"),interval="confidence")


