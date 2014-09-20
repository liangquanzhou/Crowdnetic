##########################################
#  we only use data after july 09 #############
###############################3
prosper=prosper.temp1
prosper=prosper[which(prosper$LoanOriginationDate>as.Date("2009-07-01")),]

###relevel the loan grade 
prosper$ProsperRating..Alpha.<-relevel(prosper$ProsperRating..Alpha.,ref="AA")
#### set fico levels
prosper$fico=(prosper$CreditScoreRangeLower+prosper$CreditScoreRangeUpper)/2
prosper$fico=as.factor(prosper$fico)
levels(prosper$fico)=c("600-620","620-640","640-660","660-680","680-700","700-720","720-740","740-760",
  "760-780","780-800","800-820","820-840","840-860","860-880","880-900")





ggplot(data=prosper[which(prosper$LoanStatus=="Chargedoff"| prosper$LoanStatus=="Completed" | prosper$LoanStatus=="Defaulted"),]
  ,aes(x=fico,fill=LoanStatus))+geom_histogram()+ggtitle("fico distribution")

ggplot(data=prosper[which(prosper$LoanStatus=="Chargedoff"| prosper$LoanStatus=="Completed"),]
  ,aes(x=fico,fill=LoanStatus))+geom_histogram(position="dodge")+ggtitle("fico distribution")

#View(prosper)
#write.csv(prosper,file="prosper data after july 2009.csv")

##############################################################
## these are the data we want to drop in general  ###############
#1. drop all the fico score smaller than 600, which already meet the situation.

#2. Term. drop all the 12 months data.
prosper=prosper[-which(prosper$Term=="12"),]
prosper$Term=as.factor(as.character(prosper$Term))
#3. LoanStatus. Drop all the cancelled data. Already droped
#prosper=prosper[-which(prosper$LoanStatus=="Cancelled"),]
prosper$LoanStatus=as.factor(as.character(prosper$LoanStatus))
#4. Drop all data with interest rate < 6% 
prosper=prosper[which(prosper$BorrowerRate>=0.06),]


write.csv(prosper,file="prosper data after july 09.csv")

##########################################################3
### find all prepayment data #####################

############################################
## Loan Status: Find all the loans with status completed #################
## want to find the prepayment rate
summary(prosper$LoanStatus)
completed=prosper[which(prosper$LoanStatus=="Completed"),]
# check its closed date, loan origination date, and term to find whether it is prepaid
completed$ClosedDate=as.Date(completed$ClosedDate)
completed$LoanOriginationDate=as.Date(completed$LoanOriginationDate)
completed$actual.term=(completed$ClosedDate-completed$LoanOriginationDate)/30
#View(completed$actual.term-as.numeric(as.character(completed$Term)))
difference=completed$actual.term-as.numeric(as.character(completed$Term))
prepaid=completed[which(difference<0),]
prepayment_rate=dim(prepaid)[1]/dim(completed)[1]
prepayment_rate
##88.48% are prepayment 
# check 36 and 60 months' prepayment rates
summary(prepaid$Term)/sum(summary(prepaid$Term))*100

## want to now how early for every prapayment data
#View(prepaid)
prepaid$early.rate=1-prepaid$actual.term/as.numeric(as.character(prepaid$Term))
prepaid$early.rate=as.numeric(prepaid$early.rate)
######## density plot for the early rate 
ggplot(data=prepaid)+geom_density(aes(x=early.rate),fill="grey50")+facet_wrap(~Term) ### density plot is ugly
####### hist plot
ggplot(data=prepaid)+geom_histogram(aes(x=early.rate),binwidth=0.1,fill="grey50")+facet_wrap(~Term)
#### violin plot for 36 and 60 months  
ggplot(prepaid,aes(y=early.rate,x=Term))+geom_violin()+geom_point()  ### great!
ggsave("early rate of prepayment loans.png")

###########################3
### among all these prepaid loans, want to see the fico score, years employed, interest, loan grade,
### and previous loans
##############################3
# 1. fico score
######## define the fico score
prepaid$fico=(prepaid$CreditScoreRangeLower+prepaid$CreditScoreRangeUpper)/2
### can set it as factors
prepaid$fico=as.factor(prepaid$fico)
levels(prepaid$fico)=c("600-620","620-640","640-660","660-680","680-700","700-720","720-740","740-760",
  "760-780","780-800","800-820","820-840","840-860","860-880","880-900")
ggplot(prepaid,aes(x=fico))+geom_histogram()+ggtitle("fico distribution among all prepayments")
ggsave("fico distribution among all prepayments.png")

# 2. loan grade
#### first drop all NAs in grades
prepaid.grade.no.na=prepaid[-which(is.na(prepaid$ProsperRating..Alpha.)==1),]
ggplot(prepaid.grade.no.na,aes(x=ProsperRating..Alpha.))+geom_histogram()+
  ggtitle("loan grades distribution among all prepayments")
ggsave("loan grades distribution among all prepaymengs.png")

# 3. years employed
summary(prepaid.employed.no.na$EmploymentStatusDuration)
prepaid.employed.no.na=prepaid[-which(is.na(prepaid$EmploymentStatusDuration)==1),]
ggplot(prepaid.employed.no.na,aes(x=EmploymentStatusDuration))+geom_density(fill="grey50")+
  ggtitle("employed duration(months) among all prepayments")
ggsave("employed duration(months) among all prepayments.png")

# 4. previous loans vs. no previous loans
### find out how many precent at prepayments has previous loans
previous.count=length(which(is.na(prepaid$TotalProsperLoans)==1))
no.previous.count=dim(prepaid)[1]-previous.count
previous.percent=c(previous.count,no.previous.count)/sum(c(previous.count,no.previous.count))
png(file="prior loans in prepayments")
pie(c(previous.count,no.previous.count),col=gray(c(0.1,0.9)),labels=c("68%","32%"),main="prior loans in prepayments")
legend("topright",title="Previous Loans",inset=0.05,c("have previous loans","no previous loans"),cex=0.8,
  fill=gray(c(0.1,0.9)))
rect(1, 5, 3, 7, col = "white")
dev.off()

# 5. interest rate 
summary(prepaid$BorrowerRate)
ggplot(prepaid,aes(x=BorrowerRate))+geom_density(fill="grey50")+
  ggtitle("Interest Rate among all prepayments")
ggsave("intereste rate among all prepayments.png")

####################################3#############################################333
### now looking at the charged off loans
#######################################################################################
summary(prosper$LoanStatus)
chargedoff=prosper[which(prosper$LoanStatus=="Chargedoff"),]

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


#8. monthlypay / monthly income

chargedoff$pressure=chargedoff$MonthlyLoanPayment/chargedoff$StatedMonthlyIncome
chargedoff.pressure=chargedoff[-which(is.infinite(chargedoff$pressure)==1),]
ggplot(chargedoff.pressure,aes(x=pressure))+geom_density()+xlim(0,0.2)
mean(chargedoff.pressure$pressure)

completed$pressure=completed$MonthlyLoanPayment/completed$StatedMonthlyIncome
completed.pressure=completed[-which(is.infinite(completed$pressure)==1),]
completed.pressure=completed.pressure[-which(is.nan(completed.pressure$pressure)==1),]
ggplot(completed.pressure,aes(x=pressure))+geom_density()+xlim(0,0.2)

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




#####################################################################################33
############# calculate the ROI #############################
#################################################################
prosper$remaining.principle=prosper$LoanOriginalAmount-prosper$LP_CustomerPrincipalPayments

factor=c()
factor[which(prosper$LoanStatus=="Chargedoff")]=1
factor[which(prosper$LoanStatus=="Completed")]=0
factor[which(prosper$LoanStatus=="Current")]=0
factor[which(prosper$LoanStatus=="Defaulted")]=1
factor[which(prosper$LoanStatus=="FinalPaymentInProgress")]=0
factor[which(prosper$LoanStatus=="Past Due (>120 days)")]=0.75
factor[which(prosper$LoanStatus=="Past Due (1-15 days)")]=0.25
factor[which(prosper$LoanStatus=="Past Due (16-30 days)")]=0.5
factor[which(prosper$LoanStatus=="Past Due (31-60 days)")]=0.75
factor[which(prosper$LoanStatus=="Past Due (61-90 days)")]=0.75
factor[which(prosper$LoanStatus=="Past Due (91-120 days)")]=0.75

prosper$factor=factor
prosper$estimated.loss=prosper$remaining.principle* prosper$factor
#View(prosper.p1$estimated.loss)
##### just use the LP_NetPricipalLoss as estimated loss

################################################################################
#### clean the LP_ columns and set them into their correct range
prosper$LP_CustomerPayments[which(prosper$LP_CustomerPayments<0)]= -prosper$LP_CustomerPayments[which(prosper$LP_CustomerPayments<0)]
prosper$LP_ServiceFee[which(prosper$LP_ServiceFee>0)] = -prosper$LP_ServiceFee[which(prosper$LP_ServiceFee>0)] 
prosper$LP_NetPrincipalLoss[which(prosper$LP_NetPrincipalLoss<0)] = -prosper$LP_NetPrincipalLoss[which(prosper$LP_NetPrincipalLoss<0)] 


####################3#####################################
prosper$interest.paid=prosper$LP_CustomerPayments+prosper$LP_CustomerPrincipalPayments
prosper$net.gain=prosper$interest.paid-prosper$LP_ServiceFees

prosper$roi=prosper$net.gain/(prosper$interest.paid/prosper$BorrowerRate+prosper$LP_NetPrincipalLoss)
summary(prosper$roi)

prosper.roi=prosper

prosper.roi$roi=as.character(prosper.roi$roi)

prosper.roi=prosper.roi[which(prosper.roi$roi!="NaN"),]
prosper.roi=prosper.roi[which(prosper.roi$roi!="Inf"),]
prosper.roi$roi=as.numeric(prosper.roi$roi)
summary(prosper.roi$roi)

################################################33
###### roi vs fico score
#################################################33

fico=(prosper.roi$CreditScoreRangeLower+prosper.roi$CreditScoreRangeUpper)/2
prosper.roi$fico=fico

prosper.roi$fico=as.factor(prosper.roi$fico)



ave.roi=c()
for (i in 1:length(levels(prosper.roi$fico))){
  ave.roi[i]=mean(prosper.roi[prosper.roi$fico==levels(prosper.roi$fico)[i],]$roi)
}

png(file="fico vs roi.png")
plot(ave.roi~levels(prosper.roi$fico),type="l",xlim=c(600,900),xlab="fico score",ylab="average roi",
  main="average roi by fico score")
rect(1, 5, 3, 7, col = "white")
dev.off()


###########################3
#####3 roi vs. grade
#######################

### drop all the data without grades
prosper.roi.grade.no.na=prosper.roi[-which(is.na(prosper.roi$ProsperRating..Alpha.)==1),]
prosper.roi.grade.no.na$ProsperRating..Alpha.<-relevel(prosper.roi.grade.no.na$ProsperRating..Alpha.,ref="AA")


ggplot(prosper.roi.grade.no.na,aes(y=roi,x=ProsperRating..Alpha.  ))+geom_violin()+
  ggtitle("loan grade ~ roi")+facet_wrap(~Term)
ggsave("loan grade ~ roi.png")


prosper.temp2=prosper
levels(prosper.temp2$ProsperRating..Alpha.)
summary(prosper.temp2$ProsperRating..Alpha.)
prosper.temp2$ProsperRating..Alpha.<-relevel(prosper.temp2$ProsperRating..Alpha.,ref="AA")



###########################################3
####### roi vs dti
######################################3
summary(prosper.roi$DebtToIncomeRatio)
prosper.roi.dti.no.na=prosper.roi[-which(is.na(prosper.roi$DebtToIncomeRatio)==1),]

ggplot(prosper.roi.dti.no.na,aes(x=DebtToIncomeRatio,y=roi))+geom_point()

###########################3
#### roi vs. loan term
ggplot(prosper.roi,aes(x=Term,y=roi))+geom_violin()


######################################333
###########  time series #############333
####################################
prosper.t=prosper
View(prosper.t)
prosper.t=prosper.t[order(prosper.t$ListingCreationDate,prosper.t$LoanOriginationDate),]
date=seq(as.Date("2011/01/01"),as.Date("2014/06/16"),by="day")

date.issued.value=c()
every.day.issued=c()

date.issued.value.w=c()
every.day.issued.w=c()

for (i in 1:length(date)){
  date.issued.value[i]=sum(prosper.t[which(prosper.t$LoanOriginationDate<=date[i]),]$LoanOriginalAmount)
  every.day.issued[i]=sum(prosper.t[which(prosper.t$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
  date.issued.value.w[i]=sum(prosper.t[which(prosper.t$LoanOriginationDate<=date[i]),]$LoanOriginalAmount*
      prosper.t[which(prosper.t$LoanOriginationDate<=date[i]),]$BorrowerRate)
  every.day.issued.w[i]=sum(prosper.t[which(prosper.t$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
    prosper.t[which(prosper.t$LoanOriginationDate==date[i]),]$BorrowerRate)
}

index=which(every.day.issued!=0)
work.date=date[which(every.day.issued!=0)]


qplot(date, date.issued.value,geom="point")
qplot(date,every.day.issued,geom="point")

qplot(date, date.issued.value.w,geom="point")
qplot(date,every.day.issued.w,geom="point")

date.issued.value.index=date.issued.value/date.issued.value[1]*100
date.issued.value.index.w=date.issued.value.w/date.issued.value.w[1]*100


## set up some fake test data

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 5) + 0.1)

lim1=c(min(date.issued.value.index)*0.9,max(date.issued.value.index)*1.1)
lim2=c(min(date.issued.value.index.w)*0.9,max(date.issued.value.index.w)*1.1)

lim3=c(min(every.day.issued[index])*0.9,max(every.day.issued)*1.1)
lim4=c(min(every.day.issued.w[index])*0.9,max(every.day.issued.w)*1.1)

work.date=date[which(every.day.issued!=0)]

plot(date.issued.value.index,type="l")
lines(date.issued.value.index.w,col=2,type="l")  
  
## Plot first set of data and draw its axis
plot(date.issued.value.index, pch=1, axes=FALSE, ylim=lim1, xlab="", ylab="", 
  type="l",col="black", main="unweighted vs. weighted(red)")
axis(2, ylim=lim1,col="black",las=1)  ## las=1 makes horizontal labels
mtext("",side=2,line=2.5)
box()


## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(date.issued.value.index.w, pch=1,  xlab="", ylab="", ylim=lim2, 
  axes=FALSE, type="l", col="red")
## a little farther out (line=4) to make room for labels
mtext("",side=4,col="red",line=4) 
axis(4, ylim=lim2, col="red",col.axis="red",las=1)

## Draw the time axis
axis(1,1:length(date),as.Date(as.character(date)))
mtext("Date",side=1,col="black",line=2.5)  

## Add Legend
legend("topleft",legend=c("unweighted","weighted"),
  text.col=c("black","red"),pch=c(1,1),col=c("black","red"),lty=1)






#################################################################
### analyze all the attributes in terms of 36 and 60 months
#####################################################################
require(ggplot2)

###############################
## Grade
ggplot(data=prosper.p1,aes(x=Grade))+geom_histogram()+facet_wrap(~Term)


############ try to combine the charts: fico vs. charged off and fico vs. prepayments

chargedoff.completed=prosper[which(prosper$LoanStatus=="Chargedoff" || prosper$LoanStatus=="Chargedoff"),]








#################################################################
### analysis the Loan Status
summary(prosper.p1$LoanStatus)
##### regroup the Loan Status, only set 4 categories: Chargedoff, Completed, Current, Defaulted.
loanstatus=prosper.p1$LoanStatus
levels(loanstatus)=c("Chargedoff","Completed","Current","Defaulted","Current","Defaulted","Defaulted","Defaulted",
  "Defaulted","Defaulted","Defaulted")
prosper.p1$LoanStatus=loanstatus

################ now focus on the the non-current data, i.e. the loans already have their results
past=prosper.p1[which(prosper.p1$LoanStatus!="Current"),]
past$LoanStatus=as.factor(as.character(past$LoanStatus))
summary(past$LoanStatus)/sum(summary(past$LoanStatus))*100

ggplot(past)+geom_density(aes(x=DebtToIncomeRatio))+facet_wrap(~LoanStatus)#+xlim(0,2)
ggplot(past,(aes(x=LoanStatus,y=actual.term)))+geom_violin()#+ylim(0,1)
ggplot(past)+geom_histogram(aes(x=LoanStatus))+facet_wrap(~Term)#+xlim(100,10000)+ylim(0,2500)#+facet_wrap(~LoanStatus)+
