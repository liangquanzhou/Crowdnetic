## lending club data cleaning 
# First open the data set by excel and delete
setwd("C:/Users/Liangquan Zhou/Desktop/Crowdnetic/lending club data")
dataset1<-read.csv("LoanStatsNew.csv",header=TRUE)

data=dataset1

names(data)
## data selection for interest rate prediction
data=subset(dataset1,select=-c(accept_d,  addr_city, addr_state,	collection_recovery_fee,	desc,	emp_title,	exp_d,	id,
  initial_list_status,	issue_d,	last_pymnt_amnt,	last_pymnt_d,	member_id,	next_pymnt_d,	out_prncp,	out_prncp_inv,
  pct_tl_nvr_dlq,	recoveries,	sub_grade,	title, total_pymnt, total_pymnt_inv,	total_rec_int,	total_rec_late_fee,
  total_rec_prncp,	url),list_d!="")
summary(data$list_d)

data$list_d=as.Date(as.character(data$list_d),"%m/%d/%Y")
data=subset(data,list_d>"2012-08-10")
nmissing <- function(x) sum(is.na(x))
data=subset(data,is.na(num_rev_tl_bal_gt_0)==F)
data=subset(data,is.na(bc_open_to_buy)==F)

View(t(colwise(nmissing)(data)))
View(subset(data,is.na(mths_since_recent_bc)))

# percent_bc_gt_75=NA then percent_bc_gt_75==0.because percent_bc_gt_75 has bc_util=0, all bu_til =0 has percent_bc_gt_75==0
data$percent_bc_gt_75[which(is.na(data$percent_bc_gt_75))]=0
## bc_util missing value estimate
bc_util_fit=lm(data$bc_util~data$percent_bc_gt_75)
a=data$percent_bc_gt_75[is.na(data$bc_util)]
data$bc_util[is.na(data$bc_util)]=bc_util_fit$coef[1]+bc_util_fit$coef[2]*a



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
#dataset23.1<-dataset23.1[which(dataset23.1$revol_util!=""),]
#dataset23.1$revol_util<-as.numeric(sub("%","",dataset23.1$revol_util))/100

dataset23.1<-dataset23.1[,-c(3,4,9,12,20,21,22,23)]

require(ggvis)

