### prosper data rating system ###
prosper.temp2=prosper
prosper=prosper.temp2
prosper$ProsperScore=as.numeric(prosper$ProsperScore)
require(ggplot2)
prosper14=prosper[which(prosper$LoanOriginationDate>as.Date("2014-01-01")),]
prosper14.temp=prosper14
#prosper14=na.omit(prosper14)
### use prosper14 to train
names(prosper)
View(cbind(prosper14$monthlypay,prosper14$MonthlyLoanPayment)[which(abs(prosper14$monthlypay-prosper14$MonthlyLoanPayment)>1)])
summary(prosper14$monthlypay-prosper14$MonthlyLoanPayment)
which(abs(prosper14$monthlypay-prosper14$MonthlyLoanPayment)>1)

#feature selection
i=c(64,8,17,15,5,84,47,30,31,41,42,34,36,37,18,50,22,21,19,12,82,83,85)
i2=c(47,16,30,41,42,37,22,21,34,50,85)
prosper14=prosper14[,i2]
prosper=prosper[,i2]
prosper=na.omit(prosper)
prosper14=na.omit(prosper14)
require(caret)
require(leaps)
a=regsubsets(ProsperScore~.,data=prosper14,method=c("exhaustive"),nvmax=15,really.big=T)
summary(a)
plot(a)
plot(a,scale="r2")

glm.fit=glm(ProsperScore~.,data=train,family=gaussian())
summary(glm.fit)
ma<-data.frame(rownames(varImp(glm.fit)),varImp(glm.fit)$Overall)
orderma=ma[order(ma$varImp.glm.fit..Overall,decreasing=T),]
orderma


#past1=past1[which(is.na(past1$Occupation)==0),]
train.index=sample(dim(prosper14)[1],dim(prosper14)[1]/2)
prosper14.train=prosper14[train.index,]
prosper14.test=prosper14[-train.index,]
train=prosper14.train
test=prosper14.test

train.index=sample(dim(prosper)[1],dim(prosper)[1]/2)
prosper.train=prosper[train.index,]
prosper.test=prosper[-train.index,]
train=prosper.train
test=prosper.test

train.index=sample(dim(dat)[1],dim(dat)[1]/2)
dat.train=dat[train.index,]
dat.test=dat[-train.index,]
train=dat.train
test=dat.test

require(ggplot2)
ggplot(data=train,aes(x=as.numeric(ProsperScore)))+geom_density()+facet_wrap(~Term)+coord_flip()

#naive bayes
require(e1071)
nb=naiveBayes(ProsperScore~.,train)
nbpred=predict(nb,test)
table(test$ProsperScore,nbpred)
mean(test$ProsperScore!=nbpred)

nbpred1=predict(nb,train)
table(train$ProsperScore,nbpred1)
mean(train$ProsperScore!=nbpred1)

svm.fit=svm(ProsperScore~.,data=train)
pred=fitted(svm.fit)
pred1=predict(svm.fit,test)
table(train$ProsperScore,pred)
mean(train$ProsperScore!=pred)

table(test$ProsperRating..Alpha.,pred1)
mean(test$ProsperRating..Alpha.!=pred1)


require(tree)
md3 <- tree(ProsperScore~., data=train, control=tree.control(nobs=dim(train)[1],mincut=2,minsize=4,mindev=0.0000001))
plot(md3)
text(md3,pretty=0)
test$md3.pred <- predict(md3,newdata=test,type="class")
train$md3.pred <- predict(md3,newdata=train,type="class")
table(test$ProsperScore,test$md3.pred)
table(train$ProsperScore,train$md3.pred)
mean(test$ProsperScore==test$md3.pred)
mean(train$ProsperScore==train$md3.pred)

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
md4 <- randomForest(ProsperScore~.,data=train,ntree=1000)
test$md4.pred <- predict(md4,newdata=test,type="response")
train$md4.pred <- predict(md4,newdata=train,type="response")
table(test$ProsperScore,test$md4.pred)
table(train$ProsperScore,train$md4.pred)
table(train$ProsperScore,md4$predicted)
mean(test$ProsperScore==test$md4.pred)
mean(train$ProsperScore==md4$predicted)

plot(md4)

### model6: Adaboost (correct rate=0.4077)
require(adabag) 
md6 <- boosting(ProsperScore~.,data=train,boos=TRUE,mfinal=100,control=rpart.control(cp=0.005))
test$md6.pred <- predict.boosting(md6,newdata=test,newmfinal=100)$class

mean(test$ProsperScore==test$md6.pred)
table(test$ProsperScore, test$md6.pred) 
table(train$ProsperScore, md6$class) 

md6.cv<-boosting.cv(theformula,data=past1.train,v=10,
  boos=TRUE,mfinal=100,control=rpart.control(cp=0.005))
md6.cv[-1]


require(MASS)
md7=polr(ProsperScore~. ,data=train)
