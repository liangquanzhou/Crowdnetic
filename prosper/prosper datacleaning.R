setwd("C:/Users/Liangquan Zhou/Desktop/Crowdnetic/prosper data") # set the work directory
prosper1=read.csv("PrivateCSV.csv", header=T, stringsAsFactors=T) # read the data set
prosper=prosper1

######  1. Change every feature to their appropriate format ###################

prosper$ListingKey=as.factor(prosper$ListingKey)
prosper$ListingKey[prosper$ListingKey==""]=NA
prosper$ListingKey=as.factor(as.character(prosper$ListingKey))

prosper$ListingNumber=as.factor(prosper$ListingNumber)
prosper$ListingNumber[prosper$ListingNumber==""]=NA
prosper$ListingNumber=as.factor(as.character(prosper$ListingNumber))

prosper$ListingCreationDate=as.Date(prosper$ListingCreationDate)
prosper$CreditGrade=as.factor(prosper$CreditGrade)
prosper$CreditGrade[prosper$CreditGrade==""]=NA
prosper$CreditGrade=as.factor(as.character(prosper$CreditGrade))

prosper$Term=as.factor(prosper$Term)
prosper$Term[prosper$Term==""]=NA
prosper$Term=as.factor(as.character(prosper$Term))

prosper$LoanStatus=as.factor(prosper$LoanStatus)
prosper$LoanStatus[prosper$LoanStatus==""]=NA
prosper$LoanStatus=as.factor(as.character(prosper$LoanStatus))

prosper$ClosedDate[prosper$ClosedDate==""]=NA
prosper$ClosedDate=as.Date(prosper$ClosedDate)

prosper$BorrowerAPR=as.numeric(prosper$BorrowerAPR)
prosper$BorrowerRate=as.numeric(prosper$BorrowerRate)
prosper$LenderYield=as.numeric(prosper$LenderYield)
prosper$EstimatedEffectiveYield=as.numeric(prosper$EstimatedEffectiveYield)
prosper$EstimatedLoss=as.numeric(prosper$EstimatedLoss)
prosper$EstimatedReturn=as.numeric(prosper$EstimatedReturn)
prosper$ProsperRating..numeric.=as.factor(prosper$ProsperRating..numeric.)
prosper$ProsperRating..numeric.[prosper$ProsperRating..numeric.==""]=NA
prosper$ProsperRating..numeric.=as.factor(as.character(prosper$ProsperRating..numeric.))

prosper$ProsperRating..Alpha.=as.factor(prosper$ProsperRating..Alpha.)
prosper$ProsperRating..Alpha.[prosper$ProsperRating..Alpha.==""]=NA
prosper$ProsperRating..Alpha.=as.factor(as.character(prosper$ProsperRating..Alpha.))

prosper$ProsperScore=as.factor(prosper$ProsperScore)
prosper$ProsperScore[prosper$ProsperScore==""]=NA
prosper$ProsperScore=as.factor(as.character(prosper$ProsperScore))

prosper$ListingCategory..numeric.=as.factor(prosper$ListingCategory..numeric.)
prosper$ListingCategory..numeric.[prosper$ListingCategory..numeric.==""]=NA
prosper$ListingCategory..numeric.=as.factor(as.character(prosper$ListingCategory..numeric.))

prosper$BorrowerState=as.factor(prosper$BorrowerState)
prosper$BorrowerState[prosper$BorrowerState==""]=NA
prosper$BorrowerState=as.factor(as.character(prosper$BorrowerState))

prosper$Occupation=as.factor(prosper$Occupation)
prosper$Occupation[prosper$Occupation==""]=NA
prosper$Occupation=as.factor(as.character(prosper$Occupation))

prosper$EmploymentStatus=as.factor(prosper$EmploymentStatus)
prosper$EmploymentStatus[prosper$EmploymentStatus==""]=NA
prosper$EmploymentStatus=as.factor(as.character(prosper$EmploymentStatus))

prosper$EmploymentStatusDuration=as.numeric(prosper$EmploymentStatusDuration)
prosper$IsBorrowerHomeowner=as.factor(prosper$IsBorrowerHomeowner)
prosper$IsBorrowerHomeowner[prosper$IsBorrowerHomeowner==""]=NA
prosper$IsBorrowerHomeowner=as.factor(as.character(prosper$IsBorrowerHomeowner))

prosper$CurrentlyInGroup=as.factor(prosper$CurrentlyInGroup)
prosper$CurrentlyInGroup[prosper$CurrentlyInGroup==""]=NA
prosper$CurrentlyInGroup=as.factor(as.character(prosper$CurrentlyInGroup))

prosper$GroupKey=as.factor(prosper$GroupKey)
prosper$GroupKey[prosper$GroupKey==""]=NA
prosper$GroupKey=as.factor(as.character(prosper$GroupKey))

prosper$DateCreditPulled=as.Date(prosper$DateCreditPulled)
prosper$CreditScoreRangeLower=as.numeric(prosper$CreditScoreRangeLower)
prosper$CreditScoreRangeUpper=as.numeric(prosper$CreditScoreRangeUpper)
prosper$FirstRecordedCreditLine=as.Date(prosper$FirstRecordedCreditLine)
prosper$CurrentCreditLines=as.integer(prosper$CurrentCreditLines)
prosper$OpenCreditLines=as.integer(prosper$OpenCreditLines)
prosper$TotalCreditLinespast7years=as.integer(prosper$TotalCreditLinespast7years)
prosper$OpenRevolvingAccounts=as.integer(prosper$OpenRevolvingAccounts)
prosper$OpenRevolvingMonthlyPayment=as.numeric(prosper$OpenRevolvingMonthlyPayment)
prosper$InquiriesLast6Months=as.integer(prosper$InquiriesLast6Months)
prosper$TotalInquiries=as.integer(prosper$TotalInquiries)
prosper$CurrentDelinquencies=as.integer(prosper$CurrentDelinquencies)
prosper$AmountDelinquent=as.numeric(prosper$AmountDelinquent)
prosper$DelinquenciesLast7Years=as.integer(prosper$DelinquenciesLast7Years)
prosper$PublicRecordsLast10Years=as.integer(prosper$PublicRecordsLast10Years)
prosper$PublicRecordsLast12Months=as.integer(prosper$PublicRecordsLast12Months)
prosper$RevolvingCreditBalance=as.numeric(prosper$RevolvingCreditBalance)
prosper$BankcardUtilization=as.numeric(prosper$BankcardUtilization)
prosper$AvailableBankcardCredit=as.numeric(prosper$AvailableBankcardCredit)
prosper$TotalTrades=as.integer(prosper$TotalTrades)
prosper$TradesNeverDelinquent..percentage.=as.integer(prosper$TradesNeverDelinquent..percentage.)
prosper$TradesOpenedLast6Months=as.integer(prosper$TradesOpenedLast6Months)
prosper$DebtToIncomeRatio=as.numeric(prosper$DebtToIncomeRatio)
prosper$IncomeRange=as.factor(prosper$IncomeRange)
prosper$IncomeRange[prosper$IncomeRange==""]=NA
prosper$IncomeRange=as.factor(as.character(prosper$IncomeRange))

prosper$IncomeVerifiable=as.factor(prosper$IncomeVerifiable)
prosper$IncomeVerifiable[prosper$IncomeVerifiable==""]=NA
prosper$IncomeVerifiable=as.factor(as.character(prosper$IncomeVerifiable))

prosper$StatedMonthlyIncome=as.numeric(prosper$StatedMonthlyIncome)
prosper$LoanKey=as.factor(prosper$LoanKey)
prosper$LoanKey[prosper$LoanKey==""]=NA
prosper$LoanKey=as.factor(as.character(prosper$LoanKey))

prosper$TotalProsperLoans=as.numeric(prosper$TotalProsperLoans)
prosper$TotalProsperPaymentsBilled=as.numeric(prosper$TotalProsperPaymentsBilled)
prosper$OnTimeProsperPayments=as.numeric(prosper$OnTimeProsperPayments)
prosper$ProsperPaymentsLessThanOneMonthLate=as.numeric(prosper$ProsperPaymentsLessThanOneMonthLate)
prosper$ProsperPaymentsOneMonthPlusLate=as.numeric(prosper$ProsperPaymentsOneMonthPlusLate)
prosper$ProsperPrincipalBorrowed=as.numeric(prosper$ProsperPrincipalBorrowed)
prosper$ProsperPrincipalOutstanding=as.numeric(prosper$ProsperPrincipalOutstanding)
prosper$ScorexChangeAtTimeOfListing=as.numeric(prosper$ScorexChangeAtTimeOfListing)
prosper$LoanCurrentDaysDelinquent=as.integer(prosper$LoanCurrentDaysDelinquent)
prosper$LoanFirstDefaultedCycleNumber=as.numeric(prosper$LoanFirstDefaultedCycleNumber)
prosper$LoanMonthsSinceOrigination=as.integer(prosper$LoanMonthsSinceOrigination)
prosper$LoanNumber=as.factor(prosper$LoanNumber)
prosper$LoanNumber[prosper$LoanNumber==""]=NA
prosper$LoanNumber=as.factor(as.character(prosper$LoanNumber))

prosper$LoanOriginalAmount=as.numeric(prosper$LoanOriginalAmount)
prosper$LoanOriginationDate=as.Date(prosper$LoanOriginationDate)
prosper$LoanOriginationQuarter=as.factor(prosper$LoanOriginationQuarter)
prosper$LoanOriginationQuarter[prosper$LoanOriginationQuarter==""]=NA
prosper$LoanOriginationQuarter=as.factor(as.character(prosper$LoanOriginationQuarter))

prosper$MemberKey=as.factor(prosper$MemberKey)
prosper$MemberKey[prosper$MemberKey==""]=NA
prosper$MemberKey=as.factor(as.character(prosper$MemberKey))

prosper$MonthlyLoanPayment=as.numeric(prosper$MonthlyLoanPayment)
prosper$LP_CustomerPayments=as.numeric(prosper$LP_CustomerPayments)
prosper$LP_CustomerPrincipalPayments=as.numeric(prosper$LP_CustomerPrincipalPayments)
prosper$LP_InterestandFees=as.numeric(prosper$LP_InterestandFees)
prosper$LP_ServiceFees=as.numeric(prosper$LP_ServiceFees)
prosper$LP_CollectionFees=as.numeric(prosper$LP_CollectionFees)
prosper$LP_GrossPrincipalLoss=as.numeric(prosper$LP_GrossPrincipalLoss)
prosper$LP_NetPrincipalLoss=as.numeric(prosper$LP_NetPrincipalLoss)
prosper$LP_NonPrincipalRecoverypayments=as.numeric(prosper$LP_NonPrincipalRecoverypayments)
prosper$PercentFunded=as.numeric(prosper$PercentFunded)
prosper$Recommendations=as.integer(prosper$Recommendations)
prosper$InvestmentFromFriendsCount=as.integer(prosper$InvestmentFromFriendsCount)
prosper$InvestmentFromFriendsAmount=as.numeric(prosper$InvestmentFromFriendsAmount)
prosper$Investors=as.integer(prosper$Investors)

# use summary() to see the data set

##### Create a prosper.temp1 to use prosper ( in case of error )
prosper.temp1=prosper

prosper=prosper.temp1

##### Only use the data after July 2009 
#prosper=prosper[which(prosper$LoanOriginationDate>as.Date("2009-07-01")),]


### 2. Relevel the loan grade and set fico levels ######
##### relevel the loan grade
prosper$ProsperRating..Alpha.<-relevel(prosper$ProsperRating..Alpha.,ref="AA")
#### set fico levels
prosper$fico=(prosper$CreditScoreRangeLower+prosper$CreditScoreRangeUpper)/2
prosper$fico=as.factor(prosper$fico)
levels(prosper$fico)=c("600-620","620-640","640-660","660-680","680-700","700-720","720-740","740-760",
  "760-780","780-800","800-820","820-840","840-860","860-880","880-900")
prosper$fico.numeric=(prosper$CreditScoreRangeLower+prosper$CreditScoreRangeUpper)/2


#### 3. Drop some data which is not useful and will cause the error in analysis ####

# Term. drop all the 12 months data.

prosper=prosper[which(prosper$Term!="12"),]
prosper$Term=as.factor(as.character(prosper$Term))
# LoanStatus. Drop all the cancelled data. (Already droped)

prosper=prosper[which(prosper$LoanStatus!="Cancelled"),]
#prosper$LoanStatus=as.factor(as.character(prosper$LoanStatus))


# Drop all data with interest rate < 6% 
#prosper=prosper[which(prosper$BorrowerRate>=0.06),]
# Drop all data without loan grades
#prosper=prosper[which(is.na(prosper$ProsperRating..Alpha.)!=1),]

# Every loan grade should have a correct interest rate.
#ggplot(data=prosper)+geom_boxplot(aes(x=ProsperRating..Alpha.,y=BorrowerRate),outlier.size=0) # see every loan grades' interest rate 
# the interest rate range is ralated to 3 elements: loan grade, loan term, previous loans
# to see whether the borrower has a previous loans.
#summary(prosper$TotalProsperLoans)
# we can see that there is a 0. this should be a mistake
#iii=which(prosper$TotalProsperLoans==0)
#View(prosper[which(prosper$TotalProsperLoans==0),])
#prosper[which(prosper$TotalProsperLoans==0),][,c(52:59)]=NA
# after this, we should create a variable to see whether the borrower has a previous loan
prior=rep(0,dim(prosper)[1])
prior[which(prosper$TotalProsperLoans>=1)]=1
prior=as.factor(prior)
prosper$prior=prior

#### prosper must has changed their interest rate before
## every range should be correct
#rateindex1=which(prosper$ProsperRating..Alpha.=="AA" & prosper$BorrowerRate>=0.0605 & prosper$BorrowerRate<=0.0809)
#rateindex2=which(prosper$ProsperRating..Alpha.=="A" & prosper$BorrowerRate>=0.0874 & prosper$BorrowerRate<=0.1199)
#rateindex3=which(prosper$ProsperRating..Alpha.=="B" & prosper$BorrowerRate>=0.1224 & prosper$BorrowerRate<=0.1485)
#rateindex4=which(prosper$ProsperRating..Alpha.=="C" & prosper$BorrowerRate>=0.1535 & prosper$BorrowerRate<=0.1920)
#rateindex5=which(prosper$ProsperRating..Alpha.=="D" & prosper$BorrowerRate>=0.1960 & prosper$BorrowerRate<=0.2344)
#rateindex6=which(prosper$ProsperRating..Alpha.=="E" & prosper$BorrowerRate>=0.2424 & prosper$BorrowerRate<=0.2800)
#rateindex7=which(prosper$ProsperRating..Alpha.=="HR" & prosper$BorrowerRate>=0.2874 & prosper$BorrowerRate<=0.3009)
#prosper.c=prosper[c(rateindex1,rateindex2,rateindex3,rateindex4,rateindex5,rateindex6,rateindex7),]


## since some scheduled monthly payment is not correct, so we just calculate the monthly payment and use 
## this value to replace the montly value in the data set.
monthlypay.n=(prosper$LoanOriginalAmount*(1+prosper$BorrowerRate/12)^as.numeric(as.character(prosper$Term)))*(prosper$BorrowerRate/12)
monthlypay.d=(1+prosper$BorrowerRate/12)^as.numeric(as.character(prosper$Term))-1
monthlypay=monthlypay.n/monthlypay.d
c=monthlypay-prosper$MonthlyLoanPayment
prosper$monthlypay=monthlypay
#index.temp=which(prosper$ProsperRating..Alpha.=="AA" & prosper$BorrowerRate>0.1)
#View(prosper[index.temp,])


# see every season's loan numbers since jluy 2009
#install.packages("ggplot2")
require(ggplot2)
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q3 2009"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q4 2009"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q1 2010"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q2 2010"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q3 2010"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q4 2010"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q1 2011"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q2 2011"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q3 2011"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q4 2011"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q1 2012"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q2 2012"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q3 2012"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q4 2012"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q1 2013"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q2 2013"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q3 2013"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q4 2013"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q1 2014"))
prosper$LoanOriginationQuarter=relevel(prosper$LoanOriginationQuarter,ref=c("Q2 2014"))
#ggplot(data=prosper)+geom_histogram(aes(x=LoanOriginationQuarter))+coord_flip()

#### credit years
credit.years=as.Date(prosper$FirstRecordedCreditLine)-as.Date(prosper$ListingCreationDate)
credit.years=-as.numeric(credit.years)/365
prosper$credit.years=credit.years

#mean((as.Date(as.character(prosper14$ListingCreationDate))-as.Date(as.character(prosper14$LoanOriginationDate))))

#credit.years_threshold=c(2,5,8,12,15)
#credit.years_levels=c("<2 years","2-5 years", "5-8 years","8-12 years","12-15", ">15 years")
#prosper.r$credit.years_level=set.level(prosper.r$credit.years,credit.years_threshold,credit.years_levels)

i=c("Term","LoanStatus","BorrowerRate","IncomeVerifiable","LoanOriginalAmount","EmploymentStatusDuration",
  "IsBorrowerHomeowner","OpenCreditLines","InquiriesLast6Months",
  "StatedMonthlyIncome","monthlypay","credit.years","prior","fico.numeric","RevolvingCreditBalance",
  "BankcardUtilization","DebtToIncomeRatio")


past=prosper[which(prosper$LoanStatus!="Current"),]
prosper.r=past[i]
prosper.r=subset(prosper.r,select=i,LoanStatus=="Chargedoff" | LoanStatus=="Completed" | LoanStatus=="Defaulted")

prosper.r$LoanStatus=as.factor(as.character(prosper.r$LoanStatus))


##rename 
name=(c("term","status","rate","verify","amount","employ_length","home","open_credit_lines",
  "inq6","income","monthpay","credit_years","prior","fico","revolving_credit_balance",
  "resvolving_line_utilization","dti"))

names(prosper.r)=name



## 
cf=function(x) sum(x=="Chargeoff")/sum(is.na(x)==0)
ddply(prosper.r[c("credit.years_level","LoanStatus")],.(credit.years_level),colwise(cf))

ggplot(data=prosper.r)


###########################################
###################### lending club data 