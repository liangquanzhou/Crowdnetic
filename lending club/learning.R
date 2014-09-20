## lending club data cleaning 
# First open the data set by excel and delete

library("plyr")
library("foreach")
library("gbm")
library("snow")
library("doSNOW")
library("verification")
library("reshape2")
library("ROCR")
library("caret")
library("RRF")
library("randomForest")
setwd("C:/Users/Liangquan Zhou/Desktop/Crowdnetic/lending club data")
dataset1<-read.csv("LoanStatsNew.csv",header=TRUE)

data=dataset1

## data selection for interest rate prediction
data=subset(data,select=-c(accept_d,  addr_city, addr_state,  collection_recovery_fee,	desc,	emp_title,	exp_d,	id,
  initial_list_status,	issue_d,	last_pymnt_amnt,	last_pymnt_d,	member_id,	next_pymnt_d,	out_prncp,	out_prncp_inv,
  pct_tl_nvr_dlq,	recoveries,	sub_grade,	title, total_pymnt, total_pymnt_inv,	total_rec_int,	total_rec_late_fee,
  total_rec_prncp,	url,tot_coll_amt,policy_code,num_tl_120dpd_2m),list_d!="")

data1=subset(data,select=c(mths_since_last_delinq, mths_since_last_record, mths_since_recent_revol_delinq,
  mths_since_last_major_derog,mo_sin_old_il_acct,mths_since_recent_bc_dlq,mths_since_recent_inq))

data$mths_since_last_delinq[is.na(data$mths_since_last_delinq)]=0
data$mths_since_last_record[is.na(data$mths_since_last_record)]=0
data$mths_since_recent_revol_delinq[is.na(data$mths_since_recent_revol_delinq)]=0
data$mths_since_last_major_derog[is.na(data$mths_since_last_major_derog)]=0
data$mo_sin_old_il_acct[is.na(data$mo_sin_old_il_acct)]=0
data$mths_since_recent_bc_dlq[is.na(data$mths_since_recent_bc_dlq)]=0
data$mths_since_recent_inq[is.na(data$mths_since_recent_inq)]=0

set.level=function(data){
  data[is.na(data)]=0 # missing values
  data[which(data<=6)]=1 # in 6 months
  data[which(data>6&data<=12)]=2 # 6 months to 12 months
  data[which(data>12&data<=24)]=3 # 1 year to 2 years
  data[which(data>24&data<=36)]=3 # 2 years to 3 years
  data[which(data>36&data<=48)]=4 # 3 years to 4 years
  data[which(data>48&data<=60)]=5 # 4 years to 5 years
  data[which(data>60)]=6 # more than 5 years
  data=as.factor(data)
}
data$mths_since_last_delinq=set.level(data$mths_since_last_delinq)
data$mths_since_last_record=set.level(data$mths_since_last_record)
data$mths_since_recent_revol_delinq=set.level(data$mths_since_recent_revol_delinq)
data$mths_since_last_major_derog=set.level(data$mths_since_last_major_derog)
data$mo_sin_old_il_acct=set.level(data$mo_sin_old_il_acct)
data$mths_since_recent_bc_dlq=set.level(data$mths_since_recent_bc_dlq)
data$mths_since_recent_inq=set.level(data$mths_since_recent_inq)


set.level=function(data){
  threshold=c(1,max(data),3)
  level=c(1:length(threshold)+1)
  data.level=rep(level[1],length(data))
  for (i in 1:length(level)){
    data.level[data>=threshold[i]]=level[i+1]
  }
  data.level=factor(data.level,levels=level)
  return(data.level)
}


data$list_d=as.Date(as.character(data$list_d),"%m/%d/%Y")
data$earliest_cr_line=as.Date(as.character(data$earliest_cr_line),"%m/%d/%Y")
data$last_credit_pull_d=as.Date(as.character(data$last_credit_pull_d),"%m/%d/%Y")
data$credit_years=as.numeric(data$last_credit_pull_d-data$earliest_cr_line)/365
data$fico=(data$fico_range_low+data$fico_range_high)/2
data$last_fico=(data$last_fico_range_low+data$last_fico_range_high)


data=subset(data,list_d>"2012-08-10")

data=subset(data,select=-c(funded_amnt,funded_amnt_inv,apr,grade,list_d,loan_status,pymnt_plan,
  earliest_cr_line,last_credit_pull_d,installment,fico_range_low,fico_range_high,
  last_fico_range_low,last_fico_range_high))


nmissing <- function(x) sum(is.na(x))
data=subset(data,is.na(num_rev_tl_bal_gt_0)==F)
data=subset(data,is.na(bc_open_to_buy)==F)

data=na.omit(data)


data$int_rate=as.numeric(sub("%","",data$int_rate))
data$revol_util=as.numeric(sub("%","",data$revol_util))
data$total_acc=as.numeric(as.character(data$total_acc))
data$delinq_amnt=as.numeric(as.character(data$delinq_amnt))
data$revol_bal=as.numeric(as.character(data$revol_bal))

categorical=c("term","emp_length","home_ownership","is_inc_v","purpose")
data1=subset(data,select=-c(term,emp_length,home_ownership,is_inc_v,purpose))
data2=subset(data1,select=-c(int_rate))

corr.matrix = cor(data2, use = "pairwise.complete.obs")
library(corrplot)

corrplot(corr.matrix,tl.cex=0.01)


model.fit = function(Input, Target, run){   
  fit = gbm.fit(x = Input, y = Target,
    distribution ="bernoulli",
    n.trees = 250,
    shrinkage = 0.05,
    interaction.depth = 8,
    n.minobsinnode = 100,
    verbose = F,
    bag.fraction = 0.8,
    keep.data = F)
}

model.predict = function(fit, Input, run){
  predict.gbm(object = fit, newdata = Input, fit$n.trees, type="response")
}

fit = gbm(int_rate~.,data=data[1001:50000,],distribution="gaussian",
  n.trees = 500,
  shrinkage = 0.05,
  interaction.depth = 5,
  n.minobsinnode = 50,
  verbose = F,
  bag.fraction = 0.8,
  keep.data = F)
pred=predict(fit,data[1:1000,],n.trees=500,type="response")
mean(abs(data$int_rate[1:1000]-pred))
View(cbind(data$int_rate[10101:10201],pred))

#3# percent_bc_gt_75=NA then percent_bc_gt_75==0.because percent_bc_gt_75 has bc_util=0, all bu_til =0 has percent_bc_gt_75==0
#data$percent_bc_gt_75[which(is.na(data$percent_bc_gt_75))]=0
## bc_util missing value estimate
#bc_util_fit=lm(data$bc_util~data$percent_bc_gt_75)
#a=data$percent_bc_gt_75[is.na(data$bc_util)]
#data$bc_util[is.na(data$bc_util)]=bc_util_fit$coef[1]+bc_util_fit$coef[2]*a

summary(data)
