######################################333
###########  P2P index #############333
####################################
prosper.t=prosper
names(prosper.t) # see variables' names
prosper.t=prosper[,c(64,5,9,3,65,17)] # only keep the variables we want
prosper.t=prosper.t[which(prosper.t$ListingCategory..numeric.=="1"),]
prosper.t=prosper.t[,-6]
# here we only keep loan amount, term, interest rate, and loan issued date
names(prosper.t)
prosper.t=na.omit(prosper.t) # drop all the missing values


#prosper.t=prosper.t[order(prosper.t$ListingCreationDate,prosper.t$LoanOriginationDate),]
date=seq(as.Date("2012/01/01"),as.Date("2014/03/31"),by="day") # create a time line ( x axis )
# once we have the updated data, we just need to change the time line to update 

lc1=read.csv("LoanStats3a.csv")
lc2=read.csv("LoanStats3b.csv")
lc3=read.csv("LoanStats3c.csv")
lc.temp=rbind(lc1,lc2,lc3)
lc.temp=lc
lc=lc.temp
lc=lc[,c(3,6,7,18,19,24)]
lc=lc[which(lc$purpose=="debt_consolidation"),]
lc=lc[,-6]
lc$term<-substr(lc$term,1,3)
lc=lc[which(lc$term!=""),]
lc$term<-as.factor(as.character(lc$term))
levels(lc$term)=c("36","60")
lc=na.omit(lc)
lc$int_rate=str_sub(lc$int_rate,end=str_length(lc$int_rate)-1)
lc$int_rate=as.numeric(lc$int_rate)
names(lc)=c("LoanOriginalAmount","Term","BorrowerRate","ListingCreationDate","LoanOriginationDate") # rename
lc$LoanOriginationDate=as.Date(lc$LoanOriginationDate, "%m/%d/%Y") 
lc$ListingCreationDate=as.Date(lc$ListingCreationDate, "%m/%d/%Y") 
lc$BorrowerRate=lc$BorrowerRate/100
summary(lc)  # now all the variables are in their appropriate format




prosper.t=rbind2(prosper.t,lc) # combine two data set together, now it's prosper + lc





prosper3=prosper.t[which(prosper.t$Term=="36"),]

prosper5=prosper.t[which(prosper.t$Term=="60"),]

date.issued.value=c() # the cumulative loan amount
every.day.issued=c() # the sum of everyday's loan amount 
every.day.issued.ave=c() # the average amount of everyday's loan

date.issued.value.w=c() # weighted value
every.day.issued.w=c()
every.day.issued.ave.w=c()

date.issued.value3=c()
every.day.issued3=c()
every.day.issued.ave3=c()

date.issued.value.w3=c()
every.day.issued.w3=c()
every.day.issued.ave.w3=c()

date.issued.value5=c()
every.day.issued5=c()
every.day.issued.ave5=c()

date.issued.value.w5=c()
every.day.issued.w5=c()
every.day.issued.ave.w5=c()

date.listed.value=c() # the cumulative loan amount
every.day.listed=c() # the sum of everyday's loan amount 
every.day.listed.ave=c() # the average amount of everyday's loan

date.listed.value.w=c() # weighted value
every.day.listed.w=c()
every.day.listed.ave.w=c()

date.listed.value3=c()
every.day.listed3=c()
every.day.listed.ave3=c()

date.listed.value.w3=c()
every.day.listed.w3=c()
every.day.listed.ave.w3=c()

date.listed.value5=c()
every.day.listed5=c()
every.day.listed.ave5=c()

date.listed.value.w5=c()
every.day.listed.w5=c()
every.day.listed.ave.w5=c()



for (i in 1:length(date)){
  date.issued.value[i]=sum(prosper.t[which(prosper.t$LoanOriginationDate<=date[i]),]$LoanOriginalAmount)
  every.day.issued[i]=sum(prosper.t[which(prosper.t$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
  every.day.issued.ave[i]=mean(prosper.t[which(prosper.t$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
  date.issued.value.w[i]=sum(prosper.t[which(prosper.t$LoanOriginationDate<=date[i]),]$LoanOriginalAmount*
      prosper.t[which(prosper.t$LoanOriginationDate<=date[i]),]$BorrowerRate)
  every.day.issued.w[i]=sum(prosper.t[which(prosper.t$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
      prosper.t[which(prosper.t$LoanOriginationDate==date[i]),]$BorrowerRate)
  every.day.issued.ave.w[i]=mean(prosper.t[which(prosper.t$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
      prosper.t[which(prosper.t$LoanOriginationDate==date[i]),]$BorrowerRate)
  
  date.issued.value3[i]=sum(prosper3[which(prosper3$LoanOriginationDate<=date[i]),]$LoanOriginalAmount)
  every.day.issued3[i]=sum(prosper3[which(prosper3$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
  every.day.issued.ave3[i]=mean(prosper3[which(prosper3$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
  date.issued.value.w3[i]=sum(prosper3[which(prosper3$LoanOriginationDate<=date[i]),]$LoanOriginalAmount*
      prosper3[which(prosper3$LoanOriginationDate<=date[i]),]$BorrowerRate)
  every.day.issued.w3[i]=sum(prosper3[which(prosper3$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
      prosper3[which(prosper3$LoanOriginationDate==date[i]),]$BorrowerRate)
  every.day.issued.ave.w3[i]=mean(prosper3[which(prosper3$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
      prosper3[which(prosper3$LoanOriginationDate==date[i]),]$BorrowerRate)
  
  date.issued.value5[i]=sum(prosper5[which(prosper5$LoanOriginationDate<=date[i]),]$LoanOriginalAmount)
  every.day.issued5[i]=sum(prosper5[which(prosper5$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
  every.day.issued.ave5[i]=mean(prosper5[which(prosper5$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
  date.issued.value.w5[i]=sum(prosper5[which(prosper5$LoanOriginationDate<=date[i]),]$LoanOriginalAmount*
      prosper5[which(prosper5$LoanOriginationDate<=date[i]),]$BorrowerRate)
  every.day.issued.w5[i]=sum(prosper5[which(prosper5$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
      prosper5[which(prosper5$LoanOriginationDate==date[i]),]$BorrowerRate)
  every.day.issued.ave.w5[i]=mean(prosper5[which(prosper5$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
      prosper5[which(prosper5$LoanOriginationDate==date[i]),]$BorrowerRate)
  
#####################################
  date.listed.value[i]=sum(prosper.t[which(prosper.t$ListingCreationDate<=date[i]),]$LoanOriginalAmount)
  every.day.listed[i]=sum(prosper.t[which(prosper.t$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  every.day.listed.ave[i]=mean(prosper.t[which(prosper.t$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  date.listed.value.w[i]=sum(prosper.t[which(prosper.t$ListingCreationDate<=date[i]),]$LoanOriginalAmount*
      prosper.t[which(prosper.t$ListingCreationDate<=date[i]),]$BorrowerRate)
  every.day.listed.w[i]=sum(prosper.t[which(prosper.t$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      prosper.t[which(prosper.t$ListingCreationDate==date[i]),]$BorrowerRate)
  every.day.listed.ave.w[i]=mean(prosper.t[which(prosper.t$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      prosper.t[which(prosper.t$ListingCreationDate==date[i]),]$BorrowerRate)
  
  date.listed.value3[i]=sum(prosper3[which(prosper3$ListingCreationDate<=date[i]),]$LoanOriginalAmount)
  every.day.listed3[i]=sum(prosper3[which(prosper3$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  every.day.listed.ave3[i]=mean(prosper3[which(prosper3$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  date.listed.value.w3[i]=sum(prosper3[which(prosper3$ListingCreationDate<=date[i]),]$LoanOriginalAmount*
      prosper3[which(prosper3$ListingCreationDate<=date[i]),]$BorrowerRate)
  every.day.listed.w3[i]=sum(prosper3[which(prosper3$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      prosper3[which(prosper3$ListingCreationDate==date[i]),]$BorrowerRate)
  every.day.listed.ave.w3[i]=mean(prosper3[which(prosper3$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      prosper3[which(prosper3$ListingCreationDate==date[i]),]$BorrowerRate)
  
  date.listed.value5[i]=sum(prosper5[which(prosper5$ListingCreationDate<=date[i]),]$LoanOriginalAmount)
  every.day.listed5[i]=sum(prosper5[which(prosper5$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  every.day.listed.ave5[i]=mean(prosper5[which(prosper5$ListingCreationDate==date[i]),]$LoanOriginalAmount)
  date.listed.value.w5[i]=sum(prosper5[which(prosper5$ListingCreationDate<=date[i]),]$LoanOriginalAmount*
      prosper5[which(prosper5$ListingCreationDate<=date[i]),]$BorrowerRate)
  every.day.listed.w5[i]=sum(prosper5[which(prosper5$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      prosper5[which(prosper5$ListingCreationDate==date[i]),]$BorrowerRate)
  every.day.listed.ave.w5[i]=mean(prosper5[which(prosper5$ListingCreationDate==date[i]),]$LoanOriginalAmount*
      prosper5[which(prosper5$ListingCreationDate==date[i]),]$BorrowerRate)
}

# because there are some weekends and holidays, and no loans are issued at weekends, so when we 
# want to create the daily index charts, we have to drop these days
index=which(every.day.issued!=0)
work.date=date[which(every.day.issued!=0)]

# set the first day's index = 100 
date.issued.value.index=date.issued.value/date.issued.value[1]*100
date.issued.value.w.index=date.issued.value.w/date.issued.value.w[1]*100

every.day.issued.ave.index=every.day.issued.ave[index]/every.day.issued.ave[index][1]*100
every.day.issued.ave.w.index=every.day.issued.ave.w[index]/every.day.issued.ave.w[index][1]*100


date.issued.value.index3=date.issued.value3/date.issued.value3[1]*100
date.issued.value.w.index3=date.issued.value.w3/date.issued.value.w3[1]*100

every.day.issued.ave.index3=every.day.issued.ave3[index]/every.day.issued.ave3[index][1]*100
every.day.issued.ave.w.index3=every.day.issued.ave.w3[index]/every.day.issued.ave.w3[index][1]*100


date.issued.value.index5=date.issued.value5/date.issued.value5[1]*100
date.issued.value.w.index5=date.issued.value.w5/date.issued.value.w5[1]*100

every.day.issued.ave.index5=every.day.issued.ave5[index]/every.day.issued.ave5[index][1]*100
every.day.issued.ave.w.index5=every.day.issued.ave.w5[index]/every.day.issued.ave.w5[index][1]*100

########################## listing ###

date.listed.value.index=date.listed.value/date.listed.value[1]*100
date.listed.value.w.index=date.listed.value.w/date.listed.value.w[1]*100

every.day.listed.ave.index=every.day.listed.ave /every.day.listed.ave[1]*100
every.day.listed.ave.w.index=every.day.listed.ave.w/every.day.listed.ave.w[1]*100


date.listed.value.index3=date.listed.value3/date.listed.value3[1]*100
date.listed.value.w.index3=date.listed.value.w3/date.listed.value.w3[1]*100

every.day.listed.ave.index3=every.day.listed.ave3/every.day.listed.ave3[1]*100
every.day.listed.ave.w.index3=every.day.listed.ave.w3/every.day.listed.ave.w3[1]*100


date.listed.value.index5=date.listed.value5/date.listed.value5[1]*100
date.listed.value.w.index5=date.listed.value.w5/date.listed.value.w5[1]*100

every.day.listed.ave.index5=every.day.listed.ave5/every.day.listed.ave5[1]*100
every.day.listed.ave.w.index5=every.day.listed.ave.w5/every.day.listed.ave.w5[1]*100

  
  data1=data.frame(date.listed.value.index,date,rep("date.listed.value.index"))
  names(data1)=c("index","date","group")
  
  data2=data.frame(date.listed.value.w.index,date,rep("date.listed.value.w.index"))
  names(data2)=c("index","date","group")
  
  data3=data.frame(every.day.listed.ave.index,date,rep("every.day.listed.ave.index"))
  names(data3)=c("index","date","group")
  
  data4=data.frame(every.day.listed.ave.w.index,date,rep("every.day.listed.ave.w.index"))
  names(data4)=c("index","date","group")
  
  data5=data.frame(date.listed.value.index3,date,rep("date.listed.value.index3"))
  names(data5)=c("index","date","group")
  
  data6=data.frame(date.listed.value.w.index3,date,rep("date.listed.value.w.index3"))
  names(data6)=c("index","date","group")
  
  data7=data.frame(every.day.listed.ave.index3,date,rep("every.day.listed.ave.index3"))
  names(data7)=c("index","date","group")
  
  data8=data.frame(every.day.listed.ave.w.index3,date,rep("every.day.listed.ave.w.index3"))
  names(data8)=c("index","date","group")
  
  data9=data.frame(date.listed.value.index5,date,rep("date.listed.value.index5"))
  names(data9)=c("index","date","group")
  
  data10=data.frame(date.listed.value.w.index5,date,rep("date.listed.value.w.index5"))
  names(data10)=c("index","date","group")
  
  data11=data.frame(every.day.listed.ave.index5,date,rep("every.day.listed.ave.index5"))
  names(data11)=c("index","date","group")
  
  data12=data.frame(every.day.listed.ave.w.index5,date,rep("every.day.listed.ave.w.index5"))
  names(data12)=c("index","date","group")
  
  list.index=rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12)
  
  data1=data.frame(date.issued.value.index,date,rep("date.issued.value.index"))
  names(data1)=c("index","date","group")
  
  data2=data.frame(date.issued.value.w.index,date,rep("date.issued.value.w.index"))
  names(data2)=c("index","date","group")
  
  data3=data.frame(every.day.issued.ave.index,date,rep("every.day.issued.ave.index"))
  names(data3)=c("index","date","group")
  
  data4=data.frame(every.day.issued.ave.w.index,date,rep("every.day.issued.ave.w.index"))
  names(data4)=c("index","date","group")
  
  data5=data.frame(date.issued.value.index3,date,rep("date.issued.value.index3"))
  names(data5)=c("index","date","group")
  
  data6=data.frame(date.issued.value.w.index3,date,rep("date.issued.value.w.index3"))
  names(data6)=c("index","date","group")
  
  data7=data.frame(every.day.issued.ave.index3,date,rep("every.day.issued.ave.index3"))
  names(data7)=c("index","date","group")
  
  data8=data.frame(every.day.issued.ave.w.index3,date,rep("every.day.issued.ave.w.index3"))
  names(data8)=c("index","date","group")
  
  data9=data.frame(date.issued.value.index5,date,rep("date.issued.value.index5"))
  names(data9)=c("index","date","group")
  
  data10=data.frame(date.issued.value.w.index5,date,rep("date.issued.value.w.index5"))
  names(data10)=c("index","date","group")
  
  data11=data.frame(every.day.issued.ave.index5,date,rep("every.day.issued.ave.index5"))
  names(data11)=c("index","date","group")
  
  data12=data.frame(every.day.issued.ave.w.index5,date,rep("every.day.issued.ave.w.index5"))
  names(data12)=c("index","date","group")
  
  issued.index=rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12)
  


### create charts and save them

png(filename="cumulative + all loans.png",width=1000,height=600)
plot(date.issued.value.index,type="l",main="from 2014-01-01, cumulative, all loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(date.issued.value.w.index,col=2,type="l")  
legend("topleft",lty=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="daily ave + all loans.png",width=1000,height=600)
plot(every.day.issued.ave.index,type="l",main="from 2014-01-01, daily ave, all loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.issued.ave.w.index,col=2,type="l")
legend("topleft",lty=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="cumulative + 3 year loans.png",width=1000,height=600)
plot(date.issued.value.index3,type="l",main="from 2014-01-01, cumulative, 3 year loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(date.issued.value.w.index3,col=2,type="l")  
legend("topleft",lty=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="daily ave + 3 year loans.png",width=1000,height=600)
plot(every.day.issued.ave.index3,type="l",main="from 2014-01-01, daily ave, 3 year loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.issued.ave.w.index3,col=2,type="l")
legend("topleft",lty=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="cumulative + 5 year loans.png",width=1000,height=600)
plot(date.issued.value.index5,type="l",main="from 2014-01-01, cumulative, 5 year loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(date.issued.value.w.index5,col=2,type="l")  
legend("topleft",lty=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="daily ave + 3 year loans.png",width=1000,height=600)
plot(every.day.issued.ave.index3,type="l",main="from 2014-01-01, daily ave, 3 year loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.issued.ave.w.index5,col=2,type="l")
legend("bottomleft",lty=1,lwd=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="daily ave + 5 year loans.png",width=1000,height=600)
plot(every.day.issued.ave.index5,type="l",main="from 2014-01-01, daily ave, 5 year loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.issued.ave.w.index5,col=2,type="l")
legend("bottomleft",lty=1,lwd=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()



png(filename="clumulative + unweighted.png",width=1000,height=600)
plot(date.issued.value.index,type="l",main="from 2014-01-01, cumulative, unweighted",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(date.issued.value.index3,type="l",col=2,)
lines(date.issued.value.index5,type="l",col=4,)
legend("topleft",lty=1,col=c(1,2,4),legend=c("all loans","3 year loans","5 year loans"),cex=0.8)
dev.off()

png(filename="clumulative + weighted.png",width=1000,height=600)
plot(date.issued.value.w.index,type="l",main="from 2014-01-01, cumulative, weighted",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(date.issued.value.w.index3,type="l",col=2,)
lines(date.issued.value.w.index5,type="l",col=4,)
legend("topleft",lty=1,col=c(1,2,4),legend=c("all loans","3 year loans","5 year loans"),cex=0.8)
dev.off()

png(filename="daily ave + unweighted.png",width=1000,height=600)
plot(every.day.issued.ave.index,type="l",main="from 2014-01-01, daily ave, unweighted",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.issued.ave.index3,col=2,type="l")
lines(every.day.issued.ave.index5,col=4,type="l")
legend("bottomleft",lty=1,col=c(1,2,4),legend=c("all loans","3 year loans","5 year loans"),cex=0.8)
dev.off()

png(filename="daily ave + weighted.png",width=1000,height=600)
plot(every.day.issued.ave.w.index,type="l",main="from 2014-01-01, daily ave, weighted",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.issued.ave.w.index3,col=2,type="l")
lines(every.day.issued.ave.w.index5,col=4,type="l")
legend("bottomleft",lty=1,col=c(1,2,4),legend=c("all loans","3 year loans","5 year loans"),cex=0.8)
dev.off()


data1=data.frame(every.day.issued.ave.w.index,work.date,rep("index1",61))
names(data1)=c("index","date","group")
data2=data.frame(every.day.issued.ave.w.index3,work.date,rep("index2",61))
names(data2)=c("index","date","group")
data3=rbind.data.frame(data1,data2)
ggplot(data3,aes(x=date,y=index,colour=group))+geom_line()

qplot(data1,x=work.date,y=every.day.issued.ave.w.index)+geom_line()
data1[,2]=as.Date(data1[,2],origin="1900-01-01")



##################### listings ############
png(filename="cumulative + all loans.png",width=1000,height=600)
plot(date.listed.value.index,type="l",main="from 2014-01-01, cumulative, all loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(date.listed.value.w.index,col=2,type="l")  
legend("topleft",lty=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="daily ave + all loans.png",width=1000,height=600)
plot(every.day.listed.ave.index,type="l",main="from 2014-01-01, daily ave, all loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.listed.ave.w.index,col=2,type="l")
legend("topleft",lty=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="cumulative + 3 year loans.png",width=1000,height=600)
plot(date.listed.value.index3,type="l",main="from 2014-01-01, cumulative, 3 year loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(date.listed.value.w.index3,col=2,type="l")  
legend("topleft",lty=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="daily ave + 3 year loans.png",width=1000,height=600)
plot(every.day.listed.ave.index3,type="l",main="from 2014-01-01, daily ave, 3 year loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.listed.ave.w.index3,col=2,type="l")
legend("topleft",lty=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="cumulative + 5 year loans.png",width=1000,height=600)
plot(date.listed.value.index5,type="l",main="from 2014-01-01, cumulative, 5 year loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(date.listed.value.w.index5,col=2,type="l")  
legend("topleft",lty=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="daily ave + 3 year loans.png",width=1000,height=600)
plot(every.day.listed.ave.index3,type="l",main="from 2014-01-01, daily ave, 3 year loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.listed.ave.w.index5,col=2,type="l")
legend("bottomleft",lty=1,lwd=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()

png(filename="daily ave + 5 year loans.png",width=1000,height=600)
plot(every.day.listed.ave.index5,type="l",main="from 2014-01-01, daily ave, 5 year loans",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.listed.ave.w.index5,col=2,type="l")
legend("bottomleft",lty=1,lwd=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)
dev.off()


png(filename="clumulative + unweighted.png",width=1000,height=600)
plot(date.listed.value.index,type="l",main="from 2014-01-01, cumulative, unweighted",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(date.listed.value.index3,type="l",col=2,)
lines(date.listed.value.index5,type="l",col=4,)
legend("topleft",lty=1,col=c(1,2,4),legend=c("all loans","3 year loans","5 year loans"),cex=0.8)
dev.off()

png(filename="clumulative + weighted.png",width=1000,height=600)
plot(date.listed.value.w.index,type="l",main="from 2014-01-01, cumulative, weighted",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(date.listed.value.w.index3,type="l",col=2,)
lines(date.listed.value.w.index5,type="l",col=4,)
legend("topleft",lty=1,col=c(1,2,4),legend=c("all loans","3 year loans","5 year loans"),cex=0.8)
dev.off()

png(filename="daily ave + unweighted.png",width=1000,height=600)
plot(every.day.listed.ave.index,type="l",main="from 2014-01-01, daily ave, unweighted",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.listed.ave.index3,col=2,type="l")
lines(every.day.listed.ave.index5,col=4,type="l")
legend("bottomleft",lty=1,col=c(1,2,4),legend=c("all loans","3 year loans","5 year loans"),cex=0.8)
dev.off()

png(filename="daily ave + weighted.png",width=1000,height=600)
plot(every.day.listed.ave.w.index,type="l",main="from 2014-01-01, daily ave, weighted",
  xlab="# of days after 2014-01-01",ylab="index value")
lines(every.day.listed.ave.w.index3,col=2,type="l")
lines(every.day.listed.ave.w.index5,col=4,type="l")
legend("bottomleft",lty=1,col=c(1,2,4),legend=c("all loans","3 year loans","5 year loans"),cex=0.8)
dev.off()



data1=data.frame(date.listed.value.index,date,rep("date.listed.value.index"))
names(data1)=c("index","date","group")

data2=data.frame(date.listed.value.w.index,date,rep("date.listed.value.w.index"))
names(data2)=c("index","date","group")

data3=data.frame(every.day.listed.ave.index,date,rep("every.day.listed.ave.index"))
names(data3)=c("index","date","group")

data4=data.frame(every.day.listed.ave.w.index,date,rep("every.day.listed.ave.w.index"))
names(data4)=c("index","date","group")

data5=data.frame(date.listed.value.index3,date,rep("date.listed.value.index3"))
names(data5)=c("index","date","group")

data6=data.frame(date.listed.value.w.index3,date,rep("date.listed.value.w.index3"))
names(data6)=c("index","date","group")

data7=data.frame(every.day.listed.ave.index3,date,rep("every.day.listed.ave.index3"))
names(data7)=c("index","date","group")

data8=data.frame(every.day.listed.ave.w.index3,date,rep("every.day.listed.ave.w.index3"))
names(data8)=c("index","date","group")

data9=data.frame(date.listed.value.index5,date,rep("date.listed.value.index5"))
names(data9)=c("index","date","group")

data10=data.frame(date.listed.value.w.index5,date,rep("date.listed.value.w.index5"))
names(data10)=c("index","date","group")

data11=data.frame(every.day.listed.ave.index5,date,rep("every.day.listed.ave.index5"))
names(data11)=c("index","date","group")

data12=data.frame(every.day.listed.ave.w.index5,date,rep("every.day.listed.ave.w.index5"))
names(data12)=c("index","date","group")

data13=rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12)



ggplot(rbind(data2,data4,data6,data8,data10,data12),aes(x=date,y=index,colour=group))+geom_line(size=1)

