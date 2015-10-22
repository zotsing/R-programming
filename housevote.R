setwd("D:\\study\\503")

data<-read.table("HouseVoteData.csv",header=TRUE,sep=",")
attach(data)
j=1
for (j in 2:22) {
 data[,j]  = factor(data[,j]);
}
col.names = data[1,]
#change factor level
data[,9]=factor(data[,9],labels=c("0","1","na"),levels=c("1","0","na"));
data[,11]=factor(data[,11],labels=c("0","1","na"),levels=c("1","0","na"));
data[,18]=factor(data[,18],labels=c("0","1","na"),levels=c("1","0","na"));

write.table(data,file=paste("HouseVoteDatanew.csv",sep=","),append=FALSE,quota=TRUE,sep=",",row.names=FALSE,col.names=TRUE)
for (j in 3:22){
   print(table(interaction(party,data[,j])))
j=j+1;
}
library(faraway)
#item1
ppar   = gl(2,2,labels=c("D","R"))
result = gl(2,1,4,labels=c("0","1"))
cou  = 0
cou  = outer(1:20,1:4)
cou[1,] = c(45,159,214,1)
cou[2,] = c(17,192,207,14)
cou[3,] = c(11,196,220,1)
cou[4,] = c(12,195,218,1)
cou[5,] = c(11,198,215,4)
cou[6,] = c(53,153,198,22)
cou[7,] = c(203,2,69,152)
cou[8,] = c(27,159,214,0)
cou[9,] = c(208,1,20,202)
cou[10,] = c(90,113,208,12)
cou[11,]= c(17,189,216,4)
cou[12,]= c(101,105,158,62)
cou[13,]= c(36,171,186,35)
cou[14,]= c(3,205,214,7)
cou[15,]= c(5,203,220,1)
cou[16,]= c(183,18,183,176)
cou[17,]= c(32,165,184,33)
cou[18,]= c(3,204,210,4)
cou[19,]= c(3,205,212,8)
cou[20,]= c(21,187,194,25)

y=0
for (j in 1:20){
y = cou[j,];
print(j)
ov=xtabs(y~ppar+result)
#independency
pp = prop.table(xtabs(y~ppar))
pr = prop.table(xtabs(y~result))
fv = outer(pp,pr)*sum(y)
g2 = 2*sum(ov*log(ov/fv))
x2 = sum((ov-fv)^2/fv)
df = 1
print(x2)

print(pchisq(x2,df,lower.tail=F))

ct = prop.table(ov,1)
mod = glm(y~ppar+result,data=data)
print(pchisq(deviance(mod),df.residual(mod),lower=F))
print(summary(ct))

whether significant different
odds1 = ov[1,1]/ov[1,2]
odds2 = ov[2,1]/ov[2,2]
odds12 = ov[1,1]*ov[2,2]/(ov[1,2]*ov[2,1])
print(odds1)
print(odds2)
print(odds1)
print(odds2)
print(odds12)
print(fisher.test(ov))
}
