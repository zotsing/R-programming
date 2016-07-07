##trend hypothesis testing: null: trend = 0                                

# load libraries
require(RSQLite)
require(ggplot2)
require(caTools)
require(tsoutliers)

#!!!To make code work, please change working directory according
setwd("/home/jzhou/Downloads")

mydb = dbConnect(SQLite(),"invite_dataset_ff829852.sqlite")
initExtension(mydb)

#show tables
dbListTables(mydb)

#show column names
dbListFields(mydb,"categories")
dbListFields(mydb,"invites")
dbListFields(mydb,"locations")
dbListFields(mydb,"quotes")
dbListFields(mydb,"requests")
dbListFields(mydb,"users")
 
# get sizes for request, invite and quote, and service providers
sizeRequest = dbGetQuery(mydb,"SELECT count(*) FROM requests") #4961
sizeInvite  = dbGetQuery(mydb,"SELECT count(*) FROM invites")  #24622
sizeQuote   = dbGetQuery(mydb,"SELECT count(*) FROM quotes")   #12819
sizeProvider = dbGetQuery(mydb,"SELECT count(distinct user_id) FROM invites") #994

# create a join table to link invites with requests and quotes
dbSendQuery(mydb,
                "CREATE TABLE IF NOT EXISTS joined AS
                     SELECT t1.request_id as reqID,t1.invite_id as invID,t1.user_id as servID,
                            (case when t3.quote_id is not null then 1 else 0 end) Quoted,t2.category_id as catID,t2.location_id as locID,
                            date(t2.creation_time) as reqDate,date(t1.sent_time) as invDate,
                            (strftime('%s',t1.sent_time)-strftime('%s',t2.creation_time)) as invDelay,
                            (strftime('%s',t3.sent_time)-strftime('%s',t1.sent_time)) as quoteDelay
                     FROM invites t1 INNER JOIN requests t2 ON
                           t1.request_id = t2.request_id 
                     LEFT JOIN quotes t3 ON
                           t1.invite_id = t3.invite_id")

##get 99 quantile of the interval between invite_send and quote_send, 
qDelay = dbGetQuery(mydb,"SELECT quoteDelay FROM joined WHERE quoteDelay IS NOT NULL")
Q99_Response = quantile(qDelay$quoteDelay,probs=0.99,name=FALSE)/3600./24. # convert to days 1.17 days
cat(paste0("Removing last ",ceiling(Q99_Response)," days' invites for consistence \n"))

##get average quote rate during July - August
inv_2_Quote = dbGetQuery(mydb,"SELECT count(invID) as invCnt,sum(Quoted) as quoteCnt FROM joined 
                               WHERE invDate < '2013-09-01'")
avgQuote = inv_2_Quote$quoteCnt / inv_2_Quote$invCnt
cat(paste0("Mean quoting rate is: ",sprintf("%.4f",avgQuote),"\n"))

############################Q1:###################################################################
#Regardless of request, category and location, get daily quote-to-invite rate during July - August
allRst = dbGetQuery(mydb,
                         "SELECT invDate, count(invID) as invCnt, sum(Quoted)*1.0/count(invID) as quoteRate,
                                 avg(quoteDelay) as meanDelay, stdev(quoteDelay) as sdDelay                          
                          FROM joined
                          WHERE invDate < '2013-09-01'
                          GROUP BY invDate
                          ORDER BY invDate ASC")

allRst$invDate = as.Date(allRst$invDate)
allRst$meanDelay = allRst$meanDelay/3600.  # convert to hour
allRst$sdDelay = allRst$sdDelay/3600.

# exploratory plot to show whether there is any trend
allRst$smooth7 = runmean(allRst$quoteRate,7,alg="C",endrule="mean",align="center") # weekly smooth
allRst$smooth15 = runmean(allRst$quoteRate,15,alg="C",endrule="mean",align="center") #bi-weekly smooth

png("fig1_sitewide_quote_TS.png")
p = ggplot(allRst,aes(x=invDate))+geom_line(aes(y=quoteRate,colour="raw"))+geom_point(aes(y=quoteRate,colour="rawpoints"))
p = p+geom_line(aes(y=smooth7,colour="smooth_7day"))
p = p+geom_line(aes(y=smooth15,colour="smooth_15day"))
p = p + geom_line(aes(y=avgQuote,colour="meanrate"))
p = p + scale_colour_manual(name="Legends",values=c(raw="black",rawpoints="black",meanrate="red",smooth_7day="blue",smooth_15day="green"))
p = p + xlab("invitation sent date")
p = p + ggtitle("Time series of quote rate averaged over location and category")
plot(p)
dev.off()          

#outlier detection using tsoutlier, one outlier at invite_send_date 2013-08-15
rateTS = ts(allRst$quoteRate,start=1,frequency=7) # weekly seasonality
rate.outlier = tso(rateTS,types=c("AO","LS","TC"),maxit.iloop=8)
png("fig2_sitewide_outlier.png")
plot(rate.outlier)
dev.off()

#timeseries decompose, using the above corrected outliers 
rateDecomp = decompose(rate.outlier$yadj,type="additive")
png("fig3_sitewide_quoterate_decomposition.png")
plot(rateDecomp,xlab="Weeks since July-01-2013")
dev.off()

t = 1:length(allRst$invDate)  #days since July-01-2013, for trend regression
#linear regression with time beta = 0.00118, p-value << 0.01,significant
model1 = lm(rateDecomp$trend ~ t)
#quadratic regression with time beta1 = 0.16696, beta2 = -0.10640 p-value << 0.01, significant
model2 = lm(rateDecomp$trend ~ poly(t,2))
summary(model1)
confint(model1,level=0.99) 
summary(model2)
confint(model2,level=0.99)
anova(model1,model2) #Pr(>118.24) << 0.01

####waiting time for quotes
avgDelayTS = ts(allRst$meanDelay,start=1,frequency=7)
avgDelay.outlier=tso(avgDelayTS,types=c("AO","LS","TC"),maxit.iloop=8)
summary(avgDelay.outlier)
avgDelay.Decomp = decompose(avgDelay.outlier$yadj,type="additive")
model3=lm(avgDelay.Decomp$trend ~t)
summary(model3)  #  p-value = 0.96 not significant at alpha = 0.01

###variation
sdDelayTS = ts(allRst$sdDelay,start=1,frequency=7)
sdDelay.outlier=tso(sdDelayTS,types=c("AO","LS","TC"),maxit.iloop=8)
summary(sdDelay.outlier)
sdDelay.Decomp = decompose(sdDelay.outlier$yadj,type="additive")
model4=lm(sdDelay.Decomp$trend ~t)
summary(model4)  # p-value = 0.055 not significant at alpha = 0.01

################Q2##################################################################################
#is the trend significant on category level?
days_all = data.frame(invDate=as.Date(allRst$invDate))
get_significance = function(df,factor,days_all) {
  rst = data.frame(ID=integer(),avg=numeric(),dayJul=integer(),dayAug=integer(),slope=numeric(),pvalue=numeric(),rsquared=numeric())
  for (i in 1:length(levels(df[[factor]]))) {
   dat = df[which(df[[factor]]==i),]
   rst[i,1] = i  #category id
   rst[i,2] = mean(dat$quoteRate,na.rm=TRUE) #two-month average rate for the category
   rst[i,3] = length(which(dat$invDate < "2013-08-01")) #days in July
   rst[i,4] = length(which(dat$invDate > "2013-07-31")) #days in Aug
   dat = merge(dat,days_all,by="invDate",all=TRUE)
   dat$smQuote = runmean(dat$quoteRate,7,alg="C",endrule="mean",align="center") 
   if (length(which(is.na(dat$quoteRate))) > length(dat$quoteRate) - 2) {   # if less than 2 points, disgard
       rst[i,5:7] = NA
   } else {
       model = lm(smQuote ~ invDate, data=dat) 
       rst[i,5] = summary(model)$coefficients[2]
       rst[i,6] = summary(model)$coefficients[8]
       rst[i,7] = summary(model)$r.squared
   }
 }
 rst
}
catRst = dbGetQuery(mydb,
                         "SELECT catID,invDate,count(invID) as invCnt,sum(Quoted)*1.0/count(invID) as quoteRate
                          FROM joined
                          WHERE invDate < '2013-09-01' AND invDate != '2013-08-15'
                          GROUP BY catID,invDate
                          ORDER BY catID, invDate")
catRst$invDate = as.Date(catRst$invDate)
catRst$catID = as.factor(catRst$catID)
catCoef = get_significance(catRst,"catID",days_all)
#show result
catList = subset(catCoef[with(catCoef,order(-slope)),],pvalue<0.01,c("ID","avg","slope"))
sig99 = catCoef[which(catCoef$pvalue < 0.01),] #only get categories reject H0 at 0.01 
sig99$diffDay = sig99$dayAug - sig99$dayJul
model5 = lm(avg ~ diffDay, data=sig99)
png("fig4_trend_vs_goodness_of_fit_per_category.png")
plot(sig99$rsquared,sig99$slope,type="n",xlab="r-squared",ylab="slope %/day",main="Fitted quote rate per category")
text(sig99$rsquared,sig99$slope,sig99$ID)
abline(h=model1$coefficients[2],lty=3,col="black") #overall trend
legend("topleft",paste0("overall slope:",sprintf("%.5f",model1$coefficients[2])),col = "black",lty=3)
dev.off()
png("fig5_temporal_characterisitics_of_quote_rate_per_category.png")
plot(sig99$diffDay,sig99$avg,type="n",xlab="Aug extra days relative to Jul",ylab="2-month average rate",main="Category temporal shifts")
text(sig99$diffDay,sig99$avg,sig99$ID)
abline(h=avgQuote,lty=3,col="blue") #average rate
abline(v=0,lty=3)
abline(model5,lty=3,col="red")
legend("bottomleft",paste0("mean rate:",sprintf("%.3f",avgQuote)),col = "blue",lty=3)
legend("bottomright",paste0("y= ",sprintf("%.4f",model5$coefficients[2])," * x + ",sprintf("%.4f",model5$coefficients[1])," R2=",sprintf("%.3f",summary(model5)$r.squared)),col = "red",lty=3)
dev.off()
rm(sig99)
################Q3##################################################################################
# is the trend significant on location level?
locRst = dbGetQuery(mydb,
                         "SELECT locID,invDate,count(invID) as invCnt,sum(Quoted)*1.0/count(invID) as quoteRate
                          FROM joined
                          WHERE invDate < '2013-09-01' and invDate != '2013-08-15'
                          GROUP BY locID,invDate
                          ORDER BY locID, invDate")
locRst$invDate = as.Date(locRst$invDate)
locRst$locID = as.factor(locRst$locID)
locCoef = get_significance(locRst,"locID",days_all)
#show result
locList = subset(locCoef[with(locCoef,order(-slope)),],pvalue<0.01,c("ID","avg","slope"))
sig99 = locCoef[which(locCoef$pvalue < 0.01),] #only get locations reject H0 at 0.01 
sig99$diffDay = sig99$dayAug - sig99$dayJul
model6 = lm(avg ~ diffDay, data=sig99)
png("fig6_trend_vs_goodness_of_fit_per_location.png")
plot(sig99$rsquared,sig99$slope,type="n",xlab="r-squared",ylab="slope %/day",main="Fitted quote rate per location")
text(sig99$rsquared,sig99$slope,sig99$ID)
abline(h=model1$coefficients[2],lty=3,col="black") #overall trend
legend("topleft",paste0("overall slope:",sprintf("%.5f",model1$coefficients[2])),col = "black",lty=3)
dev.off()
png("fig7_temporal_characterisitics_of_quote_rate_per_location.png")
plot(sig99$diffDay,sig99$avg,type="n",xlab="Aug extra days relative to Jul",ylab="2-month average rate",main="Location temporal shifts")
text(sig99$diffDay,sig99$avg,sig99$ID)
abline(h=avgQuote,lty=3,col="blue") #average rate
abline(v=0,lty=3)
abline(model6,lty=3,col="red")
legend("bottomleft",paste0("mean rate:",sprintf("%.3f",avgQuote)),col = "blue",lty=3)
legend("bottomright",paste0("y= ",sprintf("%.4f",model6$coefficients[2])," * x + ",sprintf("%.4f",model6$coefficients[1])," p-value=",sprintf("%.3f",summary(model6)$coefficients[8])),col = "red",lty=3)
dev.off()

################Q4##################################################################################
# is the trend significant per service provider?
servRst = dbGetQuery(mydb,
                         "SELECT servID,invDate,count(invID) as invCnt,sum(Quoted)*1.0/count(invID) as quoteRate
                          FROM joined
                          WHERE invDate < '2013-09-01' and invDate != '2013-08-15'
                          GROUP BY servID,invDate
                          ORDER BY servID, invDate")
servRst$invDate = as.Date(servRst$invDate)
servRst$servID = as.factor(servRst$servID)
servCoef = get_significance(servRst,"servID",days_all)
#show result
servList = subset(servCoef[with(servCoef,order(-slope)),],pvalue<0.01,c("ID","avg","slope"))
sig99 = servCoef[which(servCoef$pvalue < 0.01),] #only get locations reject H0 at 0.01 
sig99$diffDay = sig99$dayAug - sig99$dayJul
model7 = lm(avg ~ diffDay, data=sig99)
png("fig8_trend_vs_goodness_of_fit_per_serviceProvider.png")
plot(sig99$rsquared,sig99$slope,type="n",xlab="r-squared",ylab="slope %/day",main="Fitted quote rate per serviceProvider")
text(sig99$rsquared,sig99$slope,sig99$ID)
abline(h=model1$coefficients[2],lty=3,col="blue") #overall trend
legend("topleft",paste0("overall slope:",sprintf("%.5f",model1$coefficients[2])),col = "blue",lty=3)
dev.off()
png("fig9_temporal_characterisitics_of_quote_rate_per_serviceProvider.png")
plot(sig99$diffDay,sig99$avg,type="n",xlab="Aug extra days relative to Jul",ylab="2-month average rate",main="ServiceProvider behavior temporal shifts")
text(sig99$diffDay,sig99$avg,sig99$ID)
abline(h=avgQuote,lty=3,col="blue") #average rate
abline(v=0,lty=3)
abline(model5,lty=3,col="red")
legend("bottomleft",paste0("mean rate:",sprintf("%.3f",avgQuote)),col = "blue",lty=3)
legend("bottomright",paste0("y= ",sprintf("%.4f",model7$coefficients[2])," * x + ",sprintf("%.4f",model7$coefficients[1])," p-value=",sprintf("%.3f",summary(model7)$coefficients[8])),col = "red",lty=3)
dev.off()

################Q5##################################################################################
#is the trend significantly different from 0 per request
reqRst = dbGetQuery(mydb,"SELECT reqID,reqDate,count(invID) as invCnt, sum(Quoted)*1.0/count(invID) as quoteRate,locID,catID
                          FROM joined
                          WHERE invDate < '2013-09-01' 
                          GROUP BY reqID
                          ORDER BY reqDate")
reqRst$reqDate = as.Date(reqRst$reqDate)
model8 = lm(quoteRate ~ reqDate, data=reqRst)
summary(model8) #p-value << 0.01 trend significant > 0 
png("fig10_quote_to_invite_per_request.png")
par(mfrow=c(2,1),oma=c(2,1,2,0),mar=c(2,2,2,1),mgp=c(2,1,0),xpd=NA)  
plot(reqRst$reqDate,reqRst$quoteRate,xlab="Invite sent date",ylab="invite to quote rate")
reqRst$reqDate = as.factor(reqRst$reqDate)
boxplot(quoteRate~reqDate,data=reqRst,xlab="Invite sent date",ylab="invite to quote rate")
dev.off()                         
dbDisconnect(mydb)
