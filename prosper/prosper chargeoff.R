## charged off loan analysis
summary(prosper$LoanStatus)
chargedoff=prosper[which(prosper$LoanStatus=="Chargedoff"),]
prosper.temp1=prosper
prosper=prosper.temp1

prosper$year=year(prosper$LoanOriginationDate)

start_with_2011= as.character(prosper$LoanOriginationDate) %in% grep("^2011",prosper$LoanOriginationDate,value=T)
prosper2011=subset(prosper,start_with_2011)

####
data=subset(dataset1,select=c(issue_d,last_pymnt_d,funded_amnt,total_rec_prncp,grade,loan_status,term),grade!="",term!="")
data$grade=as.factor(as.character(data$grade))
data$term=as.factor(as.character(data$term))
data$total_rec_prncp=data$funded_amnt-data$total_rec_prncp
names(data)=c("LoanOriginationDate","ClosedDate","LoanOriginalAmount","LP_NetPrincipalLoss","ProsperRating..Alpha.","LoanStatus",
  "Term")
data$LoanOriginationDate=as.Date(data$LoanOriginationDate,"%m/%d/%Y")
data$ClosedDate=as.Date(data$ClosedDate,"%m/%d/%Y")
levels(data$Term)=c("36","60")
summary(data)
###

prosper=subset(prosper,select=c(LoanOriginationDate,ClosedDate,LoanOriginalAmount,LP_NetPrincipalLoss,
  ProsperRating..Alpha.),LoanOriginationDate<as.Date("2014-07-01") & LoanOriginationDate>as.Date("2009-07-01") &
    LoanStatus=="Chargedoff" & Term==36)
lm

data=prosper
begin_date="2009-01-01"
end_date="2014-06-16"
term=36
cf_plot(data,begin_date,end_date,60)
cf_plot(data,begin_date,end_date,36)

cf_plot=function(data,begin_date,end_date,term){
  percentage=function(x) x/sum(x)
  data=subset(data,select=c(LoanOriginationDate,ClosedDate,LoanOriginalAmount,LP_NetPrincipalLoss,ProsperRating..Alpha.),
    LoanOriginationDate<as.Date(end_date) & LoanOriginationDate>as.Date(begin_date) & LoanStatus=="Charged Off" & Term==term)
  data$count=1
  data=na.omit(data)
  data$year=year(data$LoanOriginationDate)
  data$loss.rate=data$LP_NetPrincipalLoss/data$LoanOriginalAmount
  data$cf.time=as.factor(round(as.numeric(data$ClosedDate-data$LoanOriginationDate)/30))
  d1=ddply(subset(data,select=c(ProsperRating..Alpha.,cf.time,loss.rate) ),.(ProsperRating..Alpha.,cf.time),colwise(mean))
  d2=ddply(subset(data,select=c(ProsperRating..Alpha.,cf.time,count) ),.(ProsperRating..Alpha.,cf.time),colwise(sum))
  d3=ddply(subset(d2,select=c(ProsperRating..Alpha.,count)),.(ProsperRating..Alpha.),colwise(percentage))
  d4=data.frame(d1[,1:3],d2[,3],d3[,2])
  names(d4)=c("grade","charge_off_time","avg_loss_rate","count","percentage")
  g1=ggplot(d4,aes(x=charge_off_time,y=avg_loss_rate,colour=grade))+geom_line(aes(group=grade),position="identity")+
    geom_point(aes(size=count))+stat_smooth(aes(colour=grade,group=grade),method=loess,level=0,size=0.8)+
    scale_color_manual(values=palette())+ylab("Average Principal Loss Rate")+ xlab("Charge off time (months)")
  g2=ggplot(d4,aes(x=charge_off_time,y=count,fill=grade))+geom_histogram(stat="identity")+
    scale_fill_manual(values=palette())+ylab("Number of charged off loans")+ xlab("Charge off time (months)")+
    ggtitle("Charge off loans from 2009-01-01 to 2014-06-15")
  g2
}

ggsave(filename="36 months loans(hist).png",width=20,height=10)
lm



##prepaid loan

fp_plot=function(data,begin_date,end_date,term){
  percentage=function(x) x/sum(x)
  data=subset(data,select=c(LoanOriginationDate,ClosedDate,LoanOriginalAmount,LP_NetPrincipalLoss,ProsperRating..Alpha.),
    LoanOriginationDate<as.Date(end_date) & LoanOriginationDate>as.Date(begin_date) & LoanStatus=="Fully Paid" & Term==term)
  data$count=1
  data=na.omit(data)
  data$year=year(data$LoanOriginationDate)
  data$loss.rate=data$LP_NetPrincipalLoss/data$LoanOriginalAmount
  data$fp.time=as.factor(round(as.numeric(data$ClosedDate-data$LoanOriginationDate)/30))
  d1=ddply(subset(data,select=c(ProsperRating..Alpha.,fp.time,loss.rate) ),.(ProsperRating..Alpha.,fp.time),colwise(mean))
  d2=ddply(subset(data,select=c(ProsperRating..Alpha.,fp.time,count) ),.(ProsperRating..Alpha.,fp.time),colwise(sum))
  d3=ddply(subset(d2,select=c(ProsperRating..Alpha.,count)),.(ProsperRating..Alpha.),colwise(percentage))
  d4=data.frame(d1[,1:3],d2[,3],d3[,2])
  names(d4)=c("grade","fully_paid_time","avg_loss_rate","count","percentage")
  g1=ggplot(d4,aes(x=fully_paid_time,y=avg_loss_rate,colour=grade))+geom_line(aes(group=grade),position="identity")+
    geom_point(aes(size=count))+stat_smooth(aes(colour=grade,group=grade),method=loess,level=0,size=0.8)+
    scale_color_manual(values=palette())+ylab("Average Principal Loss Rate")+ xlab("Charge off time (months)")
  g2=ggplot(d4,aes(x=fully_paid_time,y=count,fill=grade))+geom_histogram(stat="identity")+
    scale_fill_manual(values=palette())+ylab("Number of fully paid loans")+ xlab("Fully paid time (months)")+
    ggtitle("Prepaid loans from 2009-01-01 to 2014-06-15")
  g2
}
fp_plot(data,begin_date,end_date,60)
fp_plot(data,begin_date,end_date,36)




ggsave(filename="60 months loans(prepaid).png",width=20,height=10)









prosper$LoanStatus=as.factor(as.character(prosper$LoanStatus))
#levels(prosper.r$LoanStatus)=c("Chargeoff","NotChargeoff","NotChargeoff","Chargeoff"
#  ,"Chargeoff","Chargeoff","Chargeoff","Chargeoff","Chargeoff","Chargeoff","Chargeoff")
prosper$ScheduledEndDate=prosper$LoanOriginationDate+years(3)
prosper$LossRate=prosper$LP_NetPrincipalLoss/prosper$LoanOriginalAmount
prosper$cf.time=as.numeric(prosper$ClosedDate-prosper$LoanOriginationDate)/as.numeric(prosper$ScheduledEndDate-prosper$LoanOriginationDate)

ggplot(prosper,aes(x=cf.time,y=LossRate))+geom_point(colour="black")+stat_smooth(colour=1,method=loess,level=0.95)+
  stat_density2d(aes(alpha=..density..), geom="raster", contour=FALSE)








#################################################################
### analysis the Loan Status
summary(prosper$LoanStatus)

################ now focus on the the non-current data, i.e. the loans already have their results
past=prosper[which(prosper$LoanStatus!="Current"),]
past$LoanStatus=as.factor(as.character(past$LoanStatus))
summary(past$LoanStatus)/sum(summary(past$LoanStatus))*100

ggplot(past)+geom_density(aes(x=DebtToIncomeRatio))+facet_wrap(~LoanStatus)#+xlim(0,2)
ggplot(past,(aes(x=LoanStatus,y=actual.term)))+geom_violin()#+ylim(0,1)
ggplot(past)+geom_histogram(aes(x=LoanStatus),stat="identity")+facet_wrap(~Term)+coord_flip()#+xlim(100,10000)+ylim(0,2500)#+facet_wrap(~LoanStatus)+
+geom_text(aes(label=LoanStatus),stat="identity", vjust=1.5, colour="white")


#### in charged off loans, we check the fico, Income, dti, loan term, 

#1. fico 
chargedoff$fico=(chargedoff$CreditScoreRangeLower+chargedoff$CreditScoreRangeUpper)/2
### can set it as factors
chargedoff$fico=as.factor(chargedoff$fico)
levels(chargedoff$fico)=c("600-620","620-640","640-660","660-680","680-700","700-720","720-740","740-760",
  "760-780","780-800","800-820","820-840","840-860","860-880")
ggplot(chargedoff,aes(x=fico))+geom_histogram()+ggtitle("fico distribution among all chargedoffs")
ggsave("fico distribution among all chargedoffs.png")

#2. Income
summary(chargedoff$StatedMonthlyIncome)
ggplot(chargedoff,aes(x=StatedMonthlyIncome))+geom_density(fill="grey50")+xlim(0,30000)+
  ggtitle("distribution of Monthly Income amoung all charged offs")
ggsave("distribution of Monthly Income amoung all charged offs.png")

#3. loan purpose

#4. credit delinquent
summary(chargedoff$AmountDelinquent)
ggplot(chargedoff)+geom_density(aes(x=AmountDelinquent))+xlim(10000,50000)

### not good ###3

#5. employed length
summary(chargedoff$EmploymentStatusDuration)
chargedoff.employed.no.na=chargedoff[-which(is.na(chargedoff$EmploymentStatusDuration)==1),]

ggplot(chargedoff.employed.no.na,aes(x=EmploymentStatusDuration))+geom_density()+
  ggtitle("length of employment distribution among all prepayments")
ggsave("length of employment distribution among all prepayments.png")

#6. loan grade
summary(prosper$ProsperRating..Alpha.)
chargedoff.grade.no.na=chargedoff[-which(is.na(chargedoff$ProsperRating..Alpha.)==1),]
ggplot(chargedoff.grade.no.na,aes(x=ProsperRating..Alpha.))+geom_histogram()+
  ggtitle("loan grades distribution among all chargedoffs")
ggsave("loan grades distribution among all chargedoffs.png")

#7. debt to income ratio
summary(chargedoff$DebtToIncomeRatio)
chargedoff.dti.no.na=chargedoff[-which(is.na(chargedoff$DebtToIncomeRatio)==1),]
ggplot(chargedoff.dti.no.na,aes(x=DebtToIncomeRatio))+geom_density()+
  ggtitle("dti distribution among all chargedoffs")+xlim(0,1.5)
ggsave("dti distribution among all chargedoffs.png")

completed.dti.no.na=completed[-which(is.na(completed$DebtToIncomeRatio)==1),]
ggplot(completed.dti.no.na,aes(x=DebtToIncomeRatio))+geom_density()+
  ggtitle("dti distribution among all completed")+xlim(0,1.5)
ggsave("dti distribution among all completed loans.png")

cf=chargedoff.dti.no.na
cf=cf[-which(is.na(cf$EmploymentStatusDuration)==1),]


#8. monthlypay / monthly income

chargedoff$pressure=chargedoff$MonthlyLoanPayment/chargedoff$StatedMonthlyIncome
chargedoff.pressure=chargedoff[-which(is.infinite(chargedoff$pressure)==1),]
g1=ggplot(chargedoff.pressure,aes(x=pressure))+geom_density()+xlim(0,0.2)+ylim(0,20)
median(chargedoff.pressure$pressure)
chargedoff
View(cbind(chargedoff$MonthlyLoanPayment,chargedoff$StatedMonthlyIncome))

completed$pressure=completed$MonthlyLoanPayment/completed$StatedMonthlyIncome
completed.pressure=completed[-which(is.infinite(completed$pressure)==1),]
completed.pressure=completed.pressure[-which(is.nan(completed.pressure$pressure)==1),]
g2=ggplot(completed.pressure,aes(x=pressure))+geom_density()+xlim(0,0.2)+ylim(0,20)
median(completed.pressure$pressure)

## look at pressure 
prosper$pressure=prosper$MonthlyLoanPayment/prosper$StatedMonthlyIncome
prosper.pressure=prosper[-which(is.infinite(prosper$pressure)==1),]
prosper.pressure=prosper.pressure[-which(is.nan(prosper.pressure$pressure)==1),]
summary(prosper.pressure$pressure)
prosper.pressure.small=prosper.pressure[which(prosper.pressure$pressure<0.05),]
prosper.pressure.large=prosper.pressure[which(prosper.pressure$pressure>=0.1),]

# drop all the current loans
prosper.pressure.small=prosper.pressure.small[which(prosper.pressure.small$LoanStatus!="Current"),]
prosper.pressure.small$LoanStatus=as.factor(as.character(prosper.pressure.small$LoanStatus))
prosper.pressure.large=prosper.pressure.large[which(prosper.pressure.large$LoanStatus!="Current"),]
prosper.pressure.large$LoanStatus=as.factor(as.character(prosper.pressure.large$LoanStatus))
(summary(prosper.pressure.small$LoanStatus)/sum(summary(prosper.pressure.small$LoanStatus)))[1:3]
(summary(prosper.pressure.large$LoanStatus)/sum(summary(prosper.pressure.large$LoanStatus)))[1:3]

##################################################################3
## analyze the charge off rate
################################################################
#1. chargeoff rate in whole data set
summary(prosper$LoanStatus)
charge.off.rate.general=summary(prosper$LoanStatus)[1]/sum(summary(prosper$LoanStatus)[-3])

#2. chargeoff rate for grades
summary(chargedoff.grade.no.na$ProsperRating..Alpha.)

past=prosper[which(prosper$LoanStatus!="Current"),]
past.grade.no.na=past[-which(is.na(past$ProsperRating..Alpha.)==1),]
summary(past.grade.no.na$ProsperRating..Alpha.)

charge.off.rate=summary(chargedoff.grade.no.na$ProsperRating..Alpha.)/summary(past.grade.no.na$ProsperRating..Alpha.)
temp=as.data.frame(charge.off.rate)
temp$level=as.factor(names(charge.off.rate))
temp$level=relevel(temp$level,"AA")
ggplot(temp,aes(x=level,y=charge.off.rate))+geom_histogram()+ggtitle("charge off rate for loan grades")
ggsave("charge off rate for loan grades.png")

# 3. previous loans vs. no previous loans
# a borrower with prior loans would likely to be charge off or not
levels(prosper$prior)
past=prosper[which(prosper$LoanStatus!="Current"),]
summary(past$LoanStatus)
levels(past$LoanStatus)=c("Chargedoff",rep("Not-Chargedoff",times=10))
levels(past$LoanStatus)=c("Bad","Good","Bad","Bad","Good",rep("Bad",times=6))

n=c()
n[1]=dim(past[which(past$LoanStatus=="Chargedoff" & past$Term=="36" & past$prior=="1"),])[1]
n[2]=dim(past[which(past$LoanStatus!="Chargedoff" & past$Term=="36" & past$prior=="1"),])[1]
n[3]=dim(past[which(past$LoanStatus=="Chargedoff" & past$Term=="60" & past$prior=="1"),])[1]
n[4]=dim(past[which(past$LoanStatus!="Chargedoff" & past$Term=="60" & past$prior=="1"),])[1]
n[5]=dim(past[which(past$LoanStatus=="Chargedoff" & past$Term=="36" & past$prior=="0"),])[1]
n[6]=dim(past[which(past$LoanStatus!="Chargedoff" & past$Term=="36" & past$prior=="0"),])[1]
n[7]=dim(past[which(past$LoanStatus=="Chargedoff" & past$Term=="60" & past$prior=="0"),])[1]
n[8]=dim(past[which(past$LoanStatus!="Chargedoff" & past$Term=="60" & past$prior=="0"),])[1]
p=c(rep("P",times=4),rep("NP",times=4))
t=rep(c("36","36","60","60"),times=2)
cf=rep(c("Y","N"),times=4)
cfr=c(n[1]/sum(n[1:2]),n[2]/sum(n[1:2]),n[3]/sum(n[3:4]),n[4]/sum(n[3:4]),
  n[5]/sum(n[5:6]),n[6]/sum(n[5:6]),n[7]/sum(n[7:8]),n[8]/sum(n[7:8]))
d3=data.frame(p,t,cf,n,cfr)

ggplot(d3, aes(x=interaction(t,p),y=n, fill=cf)) +
  geom_bar(stat="identity") + geom_text(aes(label=round(cfr,2)),vjust=1.5, colour="black")+
  guides(fill=guide_legend(reverse=TRUE))+xlab("loan term + No Prior/Prior loan")+ ylab("# of loans & percentage")
ggsave("prior loans.png")






#past
#i=c(5,6,9,21,22,30,31,32,33,34,36,37,41,42,44,47,50,64,82,83,84)
i=c("Term","LoanStatus","BorrowerRate","IncomeVerifiable","LoanOriginalAmount","EmploymentStatusDuration",
  "IsBorrowerHomeowner","CurrentCreditLines","OpenCreditLines","InquiriesLast6Months",
  "StatedMonthlyIncome","monthlypay","credit.years","prior","fico.numeric","RevolvingCreditBalance",
  "BankcardUtilization","AmountDelinquent","DebtToIncomeRatio")


past=prosper[which(prosper$LoanStatus!="Current"),]
current=prosper[which(prosper$LoanStatus=="Current")]
prosper.r=past[i]
prosper.r=subset(prosper.r,select=i,LoanStatus=="Chargedoff" | LoanStatus=="Completed" | LoanStatus=="Defaulted")

prosper.r$LoanStatus=as.factor(as.character(prosper.r$LoanStatus))



prosper.ir.numeric=prosper[,ir.numeric]
prosper.r=na.omit(prosper.r)
prosper.ir.numeric=na.omit(prosper.ir.numeric)

cluster.result=kmeans(prosper.ir.numeric[,-1],centers=7,iter.max=1000,nstart=5)
prosper.ir.numeric$c.pred=cluster.result$cluster
table(prosper.ir.numeric[,1],cluster.result$cluster)

#feature selection
a=regsubsets(LoanStatus~.,data=prosper.r,method=c("exhaustive"),nvmax=15,really.big=T)
summary(a)
#past1=past[,c(5,6,9,10,15,17,20,21,22,29,30,31,32,33,34,35,36,37,38,41,42,32,44,47,49,50,64,68,82,83,84)]
#i=c(1,2,3,5,7,8,9,12,13,14,15,16,17,18,20,21,22,23,24,26,27,29,30,31)
past1=past[,i]
past1=past[,i3]
past1=past1[,-ii]
past1=na.omit(past1)
past1=past1[which(is.na(past1$DebtToIncomeRatio)==0),]
past1=past1[which(is.na(past1$EmploymentStatusDuration)==0),]
#past1=past1[which(is.na(past1$Occupation)==0),]

past1=prosper.r
past1=na.omit(past1)

train.index=sample(dim(past1)[1],dim(past1)[1]/2)
past1.train=past1[train.index,]
past1.test=past1[-train.index,]
train=past1.train
test=past1.test

formula=LoanStatus~ IncomeVerifiable+IsBorrowerHomeowner

formula=LoanStatus~Term+ 
  BorrowerRate+
  IncomeVerifiable+
  LoanOriginalAmount+
  EmploymentStatusDuration+
  IsBorrowerHomeowner+
  InquiriesLast6Months+InquiriesLast6Months+
  StatedMonthlyIncome+
  monthlypay+
  credit.years+
  prior+
  fico.numeric+
  BankcardUtilization+
  CurrentCreditLines+
  AmountDelinquent+
  RevolvingCreditBalance+OpenCreditLines

glm.fit=glm(LoanStatus~.,data=past1[train.index,],family=binomial,na.action=na.omit)
summary(glm.fit)
ma<-data.frame(rownames(varImp(glm.fit)),varImp(glm.fit)$Overall)
orderma=ma[order(ma$varImp.glm.fit..Overall,decreasing=T),]
orderma

svm.fit=svm(LoanStatus~.,data=past1[train.index,])
pred=fitted(svm.fit)

pred=predict(glm.fit,past1[-train.index,],type="response")
pred=round(pred,3)

past1.test$pred=pred
ggplot(data=past1.test,aes(x=LoanStatus,y=pred))+geom_violin()

pred[which(pred>=0.8)]="NotChargeoff"
pred[which(pred<0.8)]="Chargeoff"
pred=as.factor(pred)
test.table=table(past1[-train.index,]$LoanStatus,pred)
test.err=mean(past1[-train.index,]$LoanStatus!=pred)
test.table
test.err







pred1=predict(glm.fit,past1[train.index,],type="response")
past1.train$pred=pred1
ggplot(data=past1.train,aes(x=LoanStatus,y=pred))+geom_violin()
pred1[which(pred1>=0.5)]="Good"
pred1[which(pred1<=0.5)]="Bad"
pred1=as.factor(pred1)
train.table=table(past1[train.index,]$LoanStatus,pred1)
train.err=mean(past1.train$LoanStatus!=pred1)
train.table
train.err



require(survival)
clg=clogit(LoanStatus~.past1.train)



require(e1071)
nb=naiveBayes(LoanStatus~.,past1.train,labpace=3)
nbpred=predict(nb,past1.train,threshold=0.001)
table(past1.train$LoanStatus,nbpred)
nbpred1=predict(nb,past1.test,thredshold=0.001)
table(past1.test$LoanStatus,nbpred1)

require(MASS)
md2 <- polr(LoanStatus~., data=past1.train,Hess=TRUE) 
md2.all <- as.data.frame(predict(md2,newdata=test,type="p"))
test$md2.pred <- names(md2.all)[apply(md2.all, 1, which.max)]
mean(test$grade==test$md2.pred)
table(test$grade,test$md2.pred)

require(tree)
md3 <- tree(LoanStatus~., data=past1.train, control=tree.control(nobs=dim(past1.train)[1],mindev=0.0000001))
plot(md3)
text(md3,pretty=0)
past1.test$md3.pred <- predict(md3,newdata=past1.test,type="class")
table(past1.test$LoanStatus,past1.test$md3.pred)
mean(past1.test$LoanStatus==past1.test$md3.pred)

### prune a tree, cross validation
cv.md3 <- cv.tree(md3, FUN=prune.tree) 
plot(cv.md3$size,cv.md3$dev,type="b")
md3.prune <- prune.misclass(md3,best=6)
plot(md3.prune)
text(md3.prune,pretty=0)
past1.test$md3.prune.pred <- predict(md3.prune,newdata=past1.test,type="class")
mean(past1.test$LoanStatus==past1.test$md3.prune.pred)

### model4: random forest ntree=100
require(randomForest)
md4 <- randomForest(LoanStatus~.,data=past1.train,ntree=100)
past1.test$md4.pred <- predict(md4,newdata=past1.test,type="response")
table(past1.test$LoanStatus,past1.test$md4.pred)
mean(past1.test$LoanStatus==past1.test$md4.pred)
plot(md4)

### model6: Adaboost (correct rate=0.4077)
require(adabag) 
md6 <- boosting(LoanStatus~.,data=past1.train,boos=TRUE,mfinal=100,control=rpart.control(cp=0.005))
past1.test$md6.pred <- predict.boosting(md6,newdata=past1.test,newmfinal=100)$LoanStatus
mean(past1.test$LoanStatus==past1.test$md6.pred)
table(past1.test$LoanStatus, past1.test$md6.pred) 
md6.cv<-boosting.cv(theformula,data=past1.train,v=10,
  boos=TRUE,mfinal=100,control=rpart.control(cp=0.005))
md6.cv[-1]


