###j.z. project3 for Stats503
###find different origin of olive oil

setwd("D:\\study\\503")
data2<-read.table("olive_train-1.csv",header=TRUE,sep=",")
datat = read.table("olive_test.csv",header=TRUE,sep=",")
attach(data2)
library(rpart)
block.controlcv=rpart.control(minsplit=10,minbucket=3,xval=572)
olive.rf = rpart(Region~Palmitic+Palmitoleic+Stearic+Oleic+Linoleic+Linolenic+Arachidic+Eicosenoic, data=data2
,method="class",parms=list(split='information'),control=block.controlcv)
olive.rfpred = predict(olive.rf,datat)
#LDA
library(MASS)
olive.lda=lda(Region~Palmitic+Palmitoleic+Stearic+Oleic+Linoleic+Linolenic+Arachidic+Eicosenoic,data=data2)
olive.ldaar=predict(olive.lda,data2[,3:10])
olive.ldapred=predict(olive.lda,datat)

#QDA
olive.qda=qda(Region~Palmitic+Palmitoleic+Stearic+Oleic+Linoleic+Linolenic+Arachidic+Eicosenoic,data=data2)
olive.qdaar=predict(olive.qda,data2[,3:10])
olive.qdapred=predict(olive.qda,datat)
#area tree
olive.rfarea  = rpart(Area~Palmitic+Palmitoleic+Stearic+Oleic+Linoleic+Linolenic+Arachidic+Eicosenoic, data=data2
,method="class",parms=list(split='information'),control=block.controlcv)
olive.prunerfarea=prune.rpart(olive.rfarea,cp=.025)

olive.rfpredarea = predict(olive.prunerfarea,datat)
#LDA
olive.ldaarea=lda(Area~Palmitic+Palmitoleic+Stearic+Oleic+Linoleic+Linolenic+Arachidic+Eicosenoic,data=data2)
olive.ldaararea=predict(olive.ldaarea,data2[,3:10])
olive.ldapredarea=predict(olive.ldaarea,datat)
#QDA
olive.qdaarea=qda(Area~Palmitic+Palmitoleic+Stearic+Oleic+Linoleic+Linolenic+Arachidic+Eicosenoic,data=data2)
olive.qdaararea=predict(olive.qdaarea,data2[,3:10])
olive.qdapredarea=predict(olive.qdaarea,datat)


olive.rf
olive.lda
olive.qda
olive.rfarea
olive.ldaarea
olive.qdaarea


#apparent error
printcp(olive.rf)
table(Region,olive.ldaar$class)
table(Region,olive.qdaar$class)
printcp(olive.rfarea)
table(Area,olive.ldaararea$class)
table(Area,olive.qdaararea$class)



#prediction
round(olive.rfpred)
olive.ldapred$class
olive.qdapred$class
round(olive.rfpredarea)
olive.ldapredarea$class
olive.qdapredarea$class

