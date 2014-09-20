# First open the data set by excel and delete
dataset1<-read.csv("LoanStats3a.csv",header=TRUE)
dataset2<-read.csv("LoanStats3b.csv",header=TRUE)
dataset3<-read.csv("LoanStats3c.csv",header=TRUE)

dataset.23<-rbind(dataset2,dataset3)
dataset23.1<-dataset.23
#change loan status for 2 and 3
#dataset23.1<-dataset.23[which(dataset.23$loan_status=="Charged Off"|dataset.23$loan_status=="Default"|dataset.23$loan_status=="Fully Paid"),]
#dataset23.1$loan_status<-factor(as.character(dataset23.1$loan_status))
#dataset23.1$Loan_status<-"NA"
#dataset23.1$Loan_status[which(dataset23.1$loan_status=="Default" | dataset23.1$loan_status=="Charged Off")]<-"Default"
#dataset23.1$Loan_status[which(dataset23.1$loan_status=="Fully Paid")]<-"Fully Paid"
#dataset23.1$Loan_status[which(dataset23.1$loan_status=="Charged Off")]<-"Charged Off"
#dataset23.1$Loan_status<-factor(dataset23.1$Loan_status)

# variables selection#######
dataset23.1<-dataset23.1[,-c(1,2,4,5,10,11,15,16,17,18,19,22,23,29,31,32,34,35,36,40,41,42,43,44,45,47,53,63,65:73)]
dataset23.1<-dataset23.1[,-c(12,13)]
dataset23.1<-dataset23.1[,-c(20,21)]
dataset23.1<-dataset23.1[,-c(22,23,24)]
dataset23.1<-dataset23.1[,-23]
dataset23.1<-dataset23.1[,-c(22:55)]
#choose the borrowers who have delinq account
#data_delinq=dataset23.1[which(dataset23.1$acc_now_delinq>=1),]
#do not have delinq account
#data_nodelinq=dataset23.1[which(dataset23.1$acc_now_delinq<1),]

#clean grade
dataset23.1<-dataset23.1[which(dataset23.1$grade!=""),]
dataset23.1$grade<-factor(as.character(dataset23.1$grade))

#add a ditch variable
dataset23.1$ditchrate<-"0"
dataset23.1$ditchrate[which(dataset23.1$grade=="A"|dataset23.1$grade=="B"|dataset23.1$grade=="C"|dataset23.1$grade=="D")]<-"1"
dataset23.1$ditchrate<-factor(dataset23.1$ditchrate)

#change delinq account, 0 means do not have 
dataset23.1$acc_now_delinq[which(dataset23.1$acc_now_delinq>=1)]="1"
dataset23.1$acc_now_delinq[which(dataset23.1$acc_now_delinq<1)]="0"
dataset23.1$acc_now_delinq<-factor(dataset23.1$acc_now_delinq)


#change bc_open_to_buy, using median to replace NA for the loan status
dataset23.1$Loan_status1<-"NA"
dataset23.1$Loan_status1[which(dataset23.1$Loan_status=="Default" | dataset23.1$Loan_status=="Charged Off")]<-"1"
dataset23.1$Loan_status1[which(dataset23.1$Loan_status=="Fully Paid")]<-"2"
#dataset23.1$Loan_status1[which(dataset23.1$Loan_status=="Charged Off")]<-"2"
dataset23.1$Loan_status1<-factor(dataset23.1$Loan_status1)
library(Hmisc)
dataset23.1$bc_open_to_buy<-imputeMedian(dataset23.1$bc_open_to_buy,dataset23.1$Loan_status1,as.numeric(levels(dataset23.1$Loan_status1)))

###change bc_open_to_buy, using median to replace NA for credit ranking
dataset23.1$Loan_status1<-"NA"
dataset23.1$Loan_status1[which(dataset23.1$grade=="A")]<-"1"
dataset23.1$Loan_status1[which(dataset23.1$grade=="B")]<-"2"
dataset23.1$Loan_status1[which(dataset23.1$grade=="C")]<-"3"
dataset23.1$Loan_status1[which(dataset23.1$grade=="D")]<-"4"
dataset23.1$Loan_status1[which(dataset23.1$grade=="E")]<-"5"
dataset23.1$Loan_status1[which(dataset23.1$grade=="F")]<-"6"
dataset23.1$Loan_status1[which(dataset23.1$grade=="G")]<-"7"
dataset23.1$Loan_status1<-factor(dataset23.1$Loan_status1)
library(Hmisc)
dataset23.1$bc_open_to_buy<-imputeMedian(dataset23.1$bc_open_to_buy,dataset23.1$Loan_status1,as.numeric(levels(dataset23.1$Loan_status1)))

#change total_bc_limit, using median to replace NA for the loan status
dataset23.1$total_bc_limit<-imputeMedian(dataset23.1$total_bc_limit,dataset23.1$Loan_status1,as.numeric(levels(dataset23.1$Loan_status1)))

#change term
dataset23.1$term<-substr(dataset23.1$term,1,3)
dataset23.1$term<-as.factor(as.character(dataset23.1$term))
#change emp_length
dataset23.1$emp_length[which(dataset23.1$emp_length=="< 1 year")]<-"1 year"
dataset23.1<-dataset23.1[which(dataset23.1$emp_length!=""),]
dataset23.1<-dataset23.1[which(dataset23.1$emp_length!="n/a"),]
dataset23.1$emp_length<-substr(dataset23.1$emp_length,1,2)
dataset23.1$emp_length<-as.numeric(as.character(dataset23.1$emp_length))


# clean state
dataset23.1<-dataset23.1[which(dataset23.1$addr_state!=""),]
dataset23.1<-dataset23.1[which(dataset23.1$addr_state!="0"),]
dataset23.1$addr_state<-factor(as.character(dataset23.1$addr_state))

#clean home_ownership
dataset23.1<-dataset23.1[which(dataset23.1$home_ownership=="MORTGAGE"|dataset23.1$home_ownership=="OWN"|dataset23.1$home_ownership=="RENT"),]
dataset23.1$home_ownership<-factor(as.character(dataset23.1$home_ownership))
dataset23.1$home_rent<-"0"
dataset23.1$home_rent[which(dataset23.1$home_ownership=="RENT")]<-"1"
dataset23.1$home_rent<-factor(as.character(dataset23.1$home_rent))

#clean grade
dataset23.1<-dataset23.1[which(dataset23.1$grade!=""),]
dataset23.1$grade<-factor(as.character(dataset23.1$grade))

#clean income
dataset23.1<-dataset23.1[which(dataset23.1$annual_inc<1000000),]

#clean pymnt_plan
dataset23.1<-dataset23.1[which(dataset23.1$pymnt_plan=="n"|dataset23.1$pymnt_plan=="y"),]
dataset23.1$pymnt_plan<-factor(as.character(dataset23.1$pymnt_plan))

#clean purpose
dataset23.1<-dataset23.1[which(dataset23.1$purpose!=""),]
dataset23.1<-dataset23.1[which(dataset23.1$purpose!="Paying off my son's dept..."),]
dataset23.1$purpose<-factor(as.character(dataset23.1$purpose))
dataset23.1$small_business<-"0"
dataset23.1$small_business[which(dataset23.1$purpose=="small_business")]<-"1"
dataset23.1$small_business<-factor(as.character(dataset23.1$small_business))

#fico score
dataset23.1$fico<-(dataset23.1$fico_range_low+dataset23.1$fico_range_high)/2
dataset23.1<-dataset23.1[,-c(16,17)]
#clean interest rate
dataset23.1$int_rate<-as.numeric(sub("%","",dataset23.1$int_rate))/100

# revol_util (change character percentages into numeric)
dataset23.1<-dataset23.1[which(dataset23.1$revol_util!=""),]
dataset23.1$revol_util<-as.numeric(sub("%","",dataset23.1$revol_util))/100

dataset23.1<-dataset23.1[,-c(3,4,9,12,20,21,22,23)]

##using sample to split the dataset
train<-sample(nrow(dataset23.1),30000,replace=F)
trainset<-dataset23.1[train,]
testset<-dataset23.1[1:4000,]






#partition the data######
library(caret)
train.rows<-createDataPartition(dataset23.1$Loan_status,p=0.7,list=FALSE)
train.batch<-dataset23.1[train.rows,]
test.batch<-dataset23.1[-train.rows,]

##########binary logistics#########
glm.fit<-glm(Loan_status~.,data=dataset23.1[,-c(2,3,4,6,7,9,10,11,12,13,14,19)],family=binomial)
glm.probs<-predict(glm.fit,type="response")
contrasts(dataset23.1$Loan_status)
TN<-c()
TP<-c()
FN<-c()
FP<-c()
for (i in 1:length(seq(from=0.5,to=0.95,by=0.02))){
  glm.pred=rep("Default",nrow(dataset23.1))
  glm.pred[glm.probs>0.5+0.02*(i-1)]="Fully Paid"
  mat<-table(glm.pred,dataset23.1$Loan_status)
  TN[i]=mat[1,1]/(mat[1,1]+mat[2,1])
  TP[i]=mat[2,2]/(mat[2,2]+mat[1,2])
  FN[i]=mat[2,1]/(mat[1,1]+mat[2,1])
  FP[i]=mat[1,2]/(mat[2,2]+mat[1,2])
}

plot(seq(from=0.5,to=0.95,by=0.02),FN,xlab="Threshold",ylab="Error Rate",type="b",lty=2,pch=20,col="blue",main="The error rate of each group with different threshold")
lines(seq(from=0.5,to=0.95,by=0.02),FP,type="b",lty=4,pch=20,col="red")
legend(x=0.9,y=0.6,legend=c("False Neg","False Pos"),cex=0.7,pch=20,col=c("blue","red"))
glm.pred[glm.probs>0.78]="Fully Paid"
table(glm.pred,dataset23.1$Loan_status)
mean(glm.pred==dataset23.1$Loan_status)
varImp(glm.fit)
barplot(varImp(glm.fit)$Overall,names.arg=rownames(varImp(glm.fit)),cex.names=0.6,col="Lightblue",main="The Relative Importance of each variables")
mat<-as.matrix(cbind(rownames(varImp(glm.fit),varImp(glm.fit)$Overall)))
names(mat)<-c("name","impo")
mat[order(mat$Overall),]


glm.pred=rep("Default",nrow(dataset23.1))
glm.pred[glm.probs>0.78]="Fully Paid"
table(glm.pred,dataset23.1$Loan_status)
mean(glm.pred==dataset23.1$Loan_status)
sub1<-dataset23.1$Loan_status=="Fully Paid" & glm.pred=="Fully Paid"
dataset.rm<-dataset23.1[!sub1,]
glm.fit1<-glm(Loan_status~.,data=dataset.rm[,-c(9,12,14,19,22)],family=binomial)
glm.probs1<-predict(glm.fit1,type="response")
glm.pred1=rep("Default",nrow(dataset.rm))
glm.pred1[glm.probs1>0.9]="Fully Paid"
table(glm.pred1,dataset.rm$Loan_status)
mean(glm.pred1==dataset.rm$Loan_status)
varImp(glm.fit1)

sub2<-dataset.rm$Loan_status=="Fully Paid" & glm.pred1=="Fully Paid"
dataset.rm1<-dataset.rm[!sub2,]
glm.fit2<-glm(Loan_status~.,data=dataset.rm1[,-c(9,12,14,19,22)],family=binomial)
glm.probs2<-predict(glm.fit2,type="response")
glm.pred2=rep("Default",nrow(dataset.rm1))
glm.pred2[glm.probs2>0.8]="Fully Paid"
table(glm.pred2,dataset.rm1$Loan_status)
mean(glm.pred2==dataset.rm1$Loan_status)
varImp(glm.fit2)

sub3<-dataset.rm1$Loan_status=="Fully Paid" & glm.pred2=="Fully Paid"
dataset.rm2<-dataset.rm1[!sub3,]
glm.fit3<-glm(Loan_status~.,data=dataset.rm2[,-c(9,12,14,19,22)],family=binomial)
glm.probs3<-predict(glm.fit3,type="response")
glm.pred3=rep("Default",nrow(dataset.rm2))
glm.pred3[glm.probs3>0.7]="Fully Paid"
table(glm.pred3,dataset.rm2$Loan_status)
mean(glm.pred3==dataset.rm2$Loan_status)
varImp(glm.fit3)

barplot(varImp(glm.fit)$Overall,names.arg=rownames(varImp(glm.fit)),cex.names=0.5,col="Lightblue",main="The Relative Importance of each variables")
######
glm.fit1<-glm(Loan_status~.,data=dataset23.1[,c(1,7,8,11,15,16,18,20,21)],family=binomial)
glm1.probs<-predict(glm.fit1,type="response")
contrasts(dataset23.1$Loan_status)
glm1.pred=rep("Default",nrow(dataset23.1))
glm1.pred[glm1.probs>0.5]="Fully Paid"
table(glm1.pred,dataset23.1$Loan_status)
mean(glm1.pred==dataset23.1$Loan_status)

par(mfrow=c(1,2),mar=c(2,2,2,2))
rng<-range(log(train.fully$annual_inc))
with(train.default,boxplot(log(annual_inc),main="LogAnnual Income.default",ylim=rng))
with(train.fully,boxplot(log(annual_inc),main="LogAnnual Income.fully paid",ylim=rng))
with(train.default,plot(home_ownership,log(annual_inc)))
with(train.fully,plot(home_ownership,log(annual_inc)))

##multinomial#######
library(nnet)
library(reshape2)
library(foreign)
mul<-multinom(Loan_status~.,data=train.batch[,-c(9,12,14,19)])
plan.has<-dataset23.1[which(dataset23.1$pymnt_plan=="y"),]
pred<-predict(mul,newdata=test.batch[,-c(9,12,14,19)],"probs")
pred.list<-data.frame(cbind(row.name=rownames(pred),pred))
pred.list2=data.frame(cbind(pred.list,status=test.batch[,20]))
pred.list2$Fully.Paid<-as.numeric(as.character(pred.list2$Fully.Paid))
pred.list2$Default<-as.numeric(as.character(pred.list2$Default))
pred.list2$Charged.Off<-as.numeric(as.character(pred.list2$Charged.Off))

pred.list2$pred_status[which(pred.list2$Fully.Paid>=0.5)]<-"Fully Paid"
pred.list2$pred_status[which(pred.list2$Default>=0.5)]<-"Default"
pred.list2$pred_status[which(pred.list2$Charged.Off>=0.5)]<-"Charge Off"                           

table(pred=pred.list2$pred_status,true=pred.list2$status)  
test.batch1<-cbind(test.batch,pred_status=pred.list2$pred_status)
sub1<-test.batch1$pred_status=="Fully Paid" & test.batch1$Loan_status=="Fully Paid"
test.rm<-test.batch1[!sub1,]
mul.rm<-multinom(Loan_status~.,data=test.rm[,-c(9,12,14,19,22)],na.rm=T)    
pred.rm<-predict(mul.rm,newdata=test.rm[,-c(9,12,14,19,22)],"probs",na.rm=T)
pred.list.rm<-data.frame(cbind(row.name=rownames(pred.rm),pred.rm))
pred.list2.rm=data.frame(cbind(pred.list.rm,status=test.rm[,20]))
pred.list2.rm$Fully.Paid<-as.numeric(as.character(pred.list2.rm$Fully.Paid))
pred.list2.rm$Default<-as.numeric(as.character(pred.list2.rm$Default))
pred.list2.rm$Charged.Off<-as.numeric(as.character(pred.list2.rm$Charged.Off))

pred.list2.rm$pred_status[which(pred.list2.rm$Fully.Paid>=0.5)]<-"Fully Paid"
pred.list2.rm$pred_status[which(pred.list2.rm$Default>=0.5)]<-"Default"
pred.list2.rm$pred_status[which(pred.list2.rm$Charged.Off>=0.5)]<-"Charge Off"                           

table(pred=pred.list2.rm$pred_status,true=pred.list2.rm$status)  

library(klaR)
library(MASS)
bayes<-train(x=train.batch[,-c(3,4,5,12,14,19,20)],y=train.batch$grade,method="nb")

####SVM for the credit ranking####
library(e1071)
svmfit<-svm(grade~.,data=trainset,kernel="radial",cost=10,gamma=1)
tune.out=tune(svm,grade~.,data=trainset,kernel="radial",ranges=list(cost=c(0.1,1,10,100),gamma=c(0.5,1,2,3)))
table(true=testset$grade,pred=predict(tune.out$best.model,newx=testset))
#####boosting for the credit ranking######
library(adabag)
cntrl<-rpart.control(maxdepth=5,minsplit=0,cp=-1)
trainerror<-c()
testerror<-c()

for (i in 1:length(seq(5,105,by=10))){
  mfinal=5+10*(i-1)
  c.SAMME<-boosting(grade~.,data=trainset,mfinal=mfinal,coeflearn="Zhu",boos=T,control=cntrl)
  c.pred.SAMME<-predict.boosting(c.SAMME,newdata=testset)
  trainerror[i]<-1-sum(c.SAMME$class==trainset$grade)/length(trainset$grade)
  testerror[i]<-c.pred.SAMME$error
}

plot(c(1:8),trainerror,type="l",ylim=c(0,0.7))
points(c(1:8),testerror,type="l")

c.SAMME<-boosting(grade~.,data=trainset,mfinal=mfinal,coeflearn="Zhu",boos=T,control=cntrl)
1-sum(c.SAMME$class==trainset$grade)/length(trainset$grade)
c.pred.SAMME<-predict.boosting(c.SAMME,newdata=testset)


c.adaboost<-boosting(grade~.,data=trainset,mfinal=10,control=rpart.control(maxdepth=1))
barplot(c.adaboost$imp[order(c.adaboost$imp,decreasing=TRUE)],col="blue",main="Relative Importance")
c.predboosting<-predict.boosting(c.adaboost,newdata=test.batch[,-c(12,9)])

######randomforest with preliminary predictions#######
glm.fit.before<-glm(ditchrate~.,data=dataset23.1[,-3],family=binomial)
glm.probs.before<-predict(glm.fit.before,type="response")
contrasts(trainset$ditchrate)
TN1<-c()
TP1<-c()
FN1<-c()
FP1<-c()
for (i in 1:length(seq(from=0.5,to=0.95,by=0.02))){
  glm.pred=rep("0",nrow(dataset23.1))
  glm.pred[glm.probs.before>0.5+0.02*(i-1)]="1"
  mat<-table(glm.pred,dataset23.1$ditchrate)
  TN1[i]=mat[1,1]/(mat[1,1]+mat[2,1])
  TP1[i]=mat[2,2]/(mat[2,2]+mat[1,2])
  FN1[i]=mat[2,1]/(mat[1,1]+mat[2,1])
  FP1[i]=mat[1,2]/(mat[2,2]+mat[1,2])
}

plot(seq(from=0.5,to=0.95,by=0.02),FN1,xlab="Threshold",ylab="Error Rate",type="b",lty=2,pch=20,col="blue",main="The error rate of each group with different threshold")
lines(seq(from=0.5,to=0.95,by=0.02),FP1,type="b",lty=4,pch=20,col="red")
legend(x=0.9,y=0.6,legend=c("False Neg","False Pos"),cex=0.7,pch=20,col=c("blue","red"))
glm.pred.before=rep("0",nrow(dataset23.1))

glm.pred.before[glm.probs.before>intersect(FN1,FP1)]="1"
table(glm.pred.before,dataset23.1$ditchrate)
mean(glm.pred.before==dataset23.1$ditchrate)
dataset23.1.new<-cbind(dataset23.1,prob=glm.pred.before)  #to have data in A-D
dataset23.1.new<-dataset23.1.new[which(dataset23.1.new$prob=="1"),]
dataset23.1.new
dataset23.1.new<-dataset23.1.new[which(dataset23.1.new$grade=="A"|dataset23.1.new$grade=="B"|dataset23.1.new$grade=="C"|dataset23.1.new$grade=="D"),]
dataset23.1.new$grade<-factor(as.character(dataset23.1.new$grade))
train<-sample(nrow(dataset23.1.new),30000)
trainset<-dataset23.1.new[train,]

library(randomForest)
rf.1<-randomForest(x=trainset[,-c(3,17,18)],y=trainset$grade,try=6,importance=TRUE)
varImpPlot(rf.1)
rf.1

#tree
#first
library(tree)
tree.c<-tree(Loan_status~.,data=dataset23.1[,-c(9,12,14)])
plot(tree.c)
text(tree.c)
#second
library(rpart)
fit <- rpart(Loan_status~.,method="class", data=dataset23.1[,-c(9,12,14)])
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE,main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.5)

###########################dataset 1#########################################################
dataset<-rbind(dataset.23,dataset1)
dataset1<-dataset1[,-c(1,2,4,5,11,22,25,26,29,30,31,32,36,40,41,42,43,44,45)]
dataset1<-dataset1[,-c(57:81)]
dataset1<-dataset1[,-c(54,50,52,51)]
dataset1<-dataset1[,-c(46:50)]
dataset1<-dataset1[,-c(34,32,29,12,13,14,17)]
dataset1<-dataset1[,-c(10,11)]
dataset1.1<-dataset1[which(dataset1$loan_status=="Charged Off"|dataset1$loan_status=="Current"|dataset1$loan_status=="Default"|dataset1$loan_status=="Fully Paid"|dataset1$loan_status=="In Grace Period"|dataset1$loan_status=="Late (16-30 days)"|dataset1$loan_status=="Late (31-120 days)"),]
dataset1.1$loan_status<-factor(as.character(dataset1.1$loan_status))
dataset1.1<-dataset1.1[,-c(3,6,14,17,35,37,38)]
#change term
dataset1.1$term<-substr(dataset1.1$term,1,3)
dataset1.1$term<-as.factor(as.character(dataset1.1$term))
#change emp_length
dataset1.1$emp_length[which(dataset1.1$emp_length=="< 1 year")]<-"1 year"
dataset1.1<-dataset1.1[which(dataset1.1$emp_length!=""),]
dataset1.1<-dataset1.1[which(dataset1.1$emp_length!="n/a"),]
dataset1.1$emp_length<-substr(dataset1.1$emp_length,1,2)
dataset1.1$emp_length<-as.numeric(as.character(dataset1.1$emp_length))
#loan_status
dataset1.2<-dataset1.1[which(dataset1.1$loan_status=="Charged Off"|dataset1.1$loan_status=="Default"|dataset1.1$loan_status=="Fully Paid"|dataset1.1$loan_status=="Late (16-30 days)"|dataset1.1$loan_status=="Late (31-120 days)"),]
dataset1.2$loan_status<-factor(as.character(dataset1.2$loan_status))
dataset1.2$Loan_status<-"NA"
dataset1.2$Loan_status[which(dataset1.2$loan_status=="Late (16-30 days)" | dataset1.2$loan_status=="Late (31-120 days)"|dataset1.2$loan_status=="Default")]<-"Default"
dataset1.2$Loan_status[which(dataset1.2$loan_status=="Fully Paid")]<-"Fully Paid"
dataset1.2$Loan_status[which(dataset1.2$loan_status=="Charged Off")]<-"Charged Off"
dataset1.2$Loan_status<-factor(dataset1.2$Loan_status)


# clean state
dataset1.2<-dataset1.2[which(dataset1.2$addr_state!=""),]
dataset1.2<-dataset1.2[which(dataset1.2$addr_state!="0"),]
dataset1.2$addr_state<-factor(as.character(dataset1.2$addr_state))

#clean home_ownership
dataset1.2<-dataset1.2[which(dataset1.2$home_ownership=="MORTGAGE"|dataset1.2$home_ownership=="OWN"|dataset1.2$home_ownership=="RENT"),]
dataset1.2$home_ownership<-factor(as.character(dataset1.2$home_ownership))

#clean grade
dataset1.2<-dataset1.2[which(dataset1.2$grade!=""),]
dataset1.2$grade<-factor(as.character(dataset1.2$grade))

#clean income
dataset1.2<-dataset1.2[which(dataset1.2$annual_inc<1000000),]

#clean pymnt_plan
dataset1.2<-dataset1.2[which(dataset1.2$pymnt_plan=="n"|dataset1.2$pymnt_plan=="y"),]
dataset1.2$pymnt_plan<-factor(as.character(dataset1.2$pymnt_plan))

#clean purpose
dataset1.2<-dataset1.2[which(dataset1.2$purpose!=""),]
dataset1.2<-dataset1.2[which(dataset1.2$purpose!="Paying off my son's dept..."),]
dataset1.2$purpose<-factor(as.character(dataset1.2$purpose))

#fico score
dataset1.2$fico<-(dataset1.2$fico_range_low+dataset1.2$fico_range_high)/2
dataset1.2<-dataset1.2[,-c(14,15)]

# revol_util (change character percentages into numeric)
dataset1.2<-dataset1.2[which(dataset1.2$revol_util!=""),]
dataset1.2$revol_util<-as.numeric(sub("%","",dataset1.2$revol_util))/100

#revol_bal
dataset1.2$revol_bal<-as.numeric(as.character(dataset1.2$revol_bal))

#total_acc
dataset1.2<-dataset1.2[which(dataset1.2$total_acc!=""),]
dataset1.2<-dataset1.2[which(dataset1.2$total_acc!="f"),]
dataset1.2$total_acc<-as.numeric(as.character(dataset1.2$total_acc))

#collection_recovery_fee
dataset1.2$collection_recovery_fee<-as.numeric(as.character(dataset1.2$collection_recovery_fee))

#last_pymnt_amnt
dataset1.2$last_pymnt_amnt<-as.numeric(as.character(dataset1.2$last_pymnt_amnt))

#get rid of some vars
dataset1.2<-dataset1.2[,-c(16,17,20,21)]

#partition the data
library(caret)
train.rows<-createDataPartition(dataset1.2$Loan_status,p=0.7,list=FALSE)
train.batch<-dataset1.2[train.rows,]
test.batch<-dataset1.2[-train.rows,]

#randomforest
library(randomForest)
rf.1<-randomForest(x=train.batch[,-c(8,30,11)],y=train.batch$Loan_status,xtest=test.batch[,-c(8,30,11)],ytest=test.batch$Loan_status,try=6,importance=TRUE)
varImpPlot(rf.1)
rf.1

#boosting
library(adabag)
c.adaboost<-boosting(Loan_status~.,data=train.batch[,-8],mfinal=500,control=rpart.control(maxdepth=1))
barplot(c.adaboost$imp[order(c.adaboost$imp,decreasing=TRUE)],col="blue",main="Relative Importance")

#svm
library(ROCR)
library(e1071)

tune.out<-tune(svm,Loan_status~.-loan_status,data=train.batch,kernel="radial",ranges=list(cost=c(0.1,1,10,100),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=test.batch$Loan_status,pred=predict(tune.out$best.model,newx=test.batch))
#ROC curve
svmfit.opt=svm(Loan_status~.-loan_status,data=train.batch,kernel="radial",gamma,cost,decision.values=T)
fitted=attributes(predict(svmfit.opt,train.batch,decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rcoplot(fitted,train.batch$Loan)

png("loan_amount.png")
with(data.ca,hist(loan_amnt))
dev.off()





dataset1<-dataset[which(dataset$addr_state=="AK"|dataset$addr_state=="AL"|dataset$addr_state=="AR"|dataset$addr_state=="AZ"|dataset$addr_state=="CA"|dataset$addr_state=="CO"|dataset$addr_state=="CT"|dataset$addr_state=="DC"|dataset$addr_state=="DE"|dataset$addr_state=="FL"|dataset$addr_state=="GA"|dataset$addr_state=="HI"|dataset$addr_state=="IA"|dataset$addr_state=="ID"|dataset$addr_state=="IL"|dataset$addr_state=="IN"|dataset$addr_state=="KS"|dataset$addr_state=="KY"|dataset$addr_state=="LA"|dataset$addr_state=="MA"|dataset$addr_state=="MD"|dataset$addr_state=="ME"|dataset$addr_state=="MI"|dataset$addr_state=="MN"|dataset$addr_state=="MO"|dataset$addr_state=="MS"|dataset$addr_state=="MT"|dataset$addr_state=="NC"|dataset$addr_state=="ND"|dataset$addr_state=="NE"|dataset$addr_state=="NH"|dataset$addr_state=="NJ"|dataset$addr_state=="NM"|dataset$addr_state=="NV"|dataset$addr_state=="NY"|dataset$addr_state=="OH"|dataset$addr_state=="OK"|dataset$addr_state=="OR"|dataset$addr_state=="PA"|dataset$addr_state=="RI"|dataset$addr_state=="SC"|dataset$addr_state=="SD"|dataset$addr_state=="TN"|dataset$addr_state=="TX"|dataset$addr_state=="UT"|dataset$addr_state=="VA"|dataset$addr_state=="VT"|dataset$addr_state=="WA"|dataset$addr_state=="WI"|dataset$addr_state=="WV"|dataset$addr_state=="WY"),]
