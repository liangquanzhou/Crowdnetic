data=read.csv("crowdnetic_7_31.csv",header=T,stringsAsFactors=T)
data1=read.csv("loan_7_30.csv",header=T,stringsAsFactors=T)
data1$id=as.factor(data1$id)
data1$portal_listing_id=as.factor(data1$portal_listing_id)
summary(data1)
dat=data1

data1=data1[,c()]
data=data[,c(1:14)]
dat=data
data.temp=data
data=data.temp
dat=data.temp

dat=dat[,c(2,9,4,6,14)]
dat=dat[which(dat$status!="CANCELLED"),]
#summary(dat)
dat$amount=as.numeric(dat$amount)
dat$amount_funded=as.numeric(dat$amount_funded)

#dat$date_created=as.Date(dat$date_created,"%m/%d/%Y")
dat$listing_end_date=as.Date(dat$listing_end_date,"%m/%d/%Y")
#dat$listing_end_date=as.Date(dat$listing_end_date,"%Y-%m-%d")
dat$listing_start_date=as.Date(dat$listing_start_date,"%m/%d/%Y")
#dat$listing_start_date=as.Date(dat$listing_start_date,"%Y-%m-%d")

dat$date_issued=as.Date(dat$date_issued,"%m/%d/%Y")
dat$term=as.factor(dat$term)
dat=na.omit(dat)
summary(dat)
dat=dat[which(dat$term=="36" | dat$term=="60"),]


dat$percent_funded=as.factor(dat$percent_funded)
#dat$percent_funded=as.numeric(dat$percent_funded)
dat[which(dat$percent_funded=="NULL"),]$percent_funded="100.0000"
percent=dat$percent_funded
#head(levels(dat$percent_funded))
#summary(as.numeric(levels(dat$percent_funded)))
dat$interest_rate=as.character(dat$interest_rate)
dat$interest_rate=as.numeric(dat$interest_rate)
dat$interest_rate=dat$interest_rate/100
dat=na.omit(dat)

dat=dat[,c(4,12,7,9,8)]

View(dat$interest_rate)
names(dat)=c("LoanOriginalAmount","Term","BorrowerRate","ListingCreationDate","LoanOriginationDate") # rename
dat$Term=as.factor(dat$Term)

date=seq(as.Date("2014/01/01"),as.Date("2014/06/30"),by="day")


### every day new listed ###
for (i in 1:length(date)){
  
  date.listed.value[i]=sum(data[which(data$ListingCreationDate<=date[i]),]$LoanOriginalAmount)
  every.day.listed[i]=sum(data[which(data$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  every.day.listed.ave[i]=mean(data[which(data$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  date.listed.value.w[i]=sum(data[which(data$ListingCreationDate<=date[i]),]$LoanOriginalAmount*
      data[which(data$ListingCreationDate<=date[i]),]$BorrowerRate)
  every.day.listed.w[i]=sum(data[which(data$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      data[which(data$ListingCreationDate==date[i]),]$BorrowerRate)
  every.day.listed.ave.w[i]=mean(data[which(data$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      data[which(data$ListingCreationDate==date[i]),]$BorrowerRate)
  
  date.listed.value3[i]=sum(data3[which(data3$ListingCreationDate<=date[i]),]$LoanOriginalAmount)
  every.day.listed3[i]=sum(data3[which(data3$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  every.day.listed.ave3[i]=mean(data3[which(data3$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  date.listed.value.w3[i]=sum(data3[which(data3$ListingCreationDate<=date[i]),]$LoanOriginalAmount*
      data3[which(data3$ListingCreationDate<=date[i]),]$BorrowerRate)
  every.day.listed.w3[i]=sum(data3[which(data3$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      data3[which(data3$ListingCreationDate==date[i]),]$BorrowerRate)
  every.day.listed.ave.w3[i]=mean(data3[which(data3$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      data3[which(data3$ListingCreationDate==date[i]),]$BorrowerRate)
  
  date.listed.value5[i]=sum(data5[which(data5$ListingCreationDate<=date[i]),]$LoanOriginalAmount)
  every.day.listed5[i]=sum(data5[which(data5$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  every.day.listed.ave5[i]=mean(data5[which(data5$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  date.listed.value.w5[i]=sum(data5[which(data5$ListingCreationDate<=date[i]),]$LoanOriginalAmount*
      data5[which(data5$ListingCreationDate<=date[i]),]$BorrowerRate)
  every.day.listed.w5[i]=sum(data5[which(data5$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      data5[which(data5$ListingCreationDate==date[i]),]$BorrowerRate)
  every.day.listed.ave.w5[i]=mean(data5[which(data5$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      data5[which(data5$ListingCreationDate==date[i]),]$BorrowerRate)
}
