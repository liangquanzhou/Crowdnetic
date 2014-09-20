data=read.csv("crowdnetic_index_01_01_14.csv",header=T,stringsAsFactors=T)
data$count=rep(1,dim(data)[1])
date=seq(as.Date("2014/01/01"),as.Date("2014/06/27"),by="day") # create a time line ( x axis )

names(data)
data=subset(data,listed_date>="2014-01-01")
data$listed_date=as.Date(data$listed_date,"%Y-%m-%d")
data$term=as.factor(data$term)
data$rate=as.numeric(as.character(data$rate))
summary(data)

data.temp1=data
data=data.temp1

#dc=data[which(data$purpose=="DEBT_CONSOLIDATION"),]
#ccr=data[which(data$purpose=="CREDIT_CARD_REFINANCING"),]

#1. 
#data=ccr

data3=data[which(data$term=="36"),]

data5=data[which(data$term=="60"),]



every.day.listed=c() # the sum of everyday's loan amount 
every.day.listed.w=c()

every.day.listed3=c()
every.day.listed.w3=c()

every.day.listed5=c()
every.day.listed.w5=c()

for (i in 1:length(date)){
  #####################################
  every.day.listed[i]=sum(data[which(data$listed_date==date[i]),]$amount)
  every.day.listed.w[i]=sum(data[which(data$listed_date==date[i]),]$amount*
      data[which(data$listed_date==date[i]),]$rate)

  every.day.listed3[i]=sum(data3[which(data3$listed_date==date[i]),]$amount)
  every.day.listed.w3[i]=sum(data3[which(data3$listed_date==date[i]),]$amount*
      data3[which(data3$listed_date==date[i]),]$rate)

  every.day.listed5[i]=sum(data5[which(data5$listed_date==date[i]),]$amount)
  every.day.listed.w5[i]=sum(data5[which(data5$listed_date==date[i]),]$amount*
      data5[which(data5$listed_date==date[i]),]$rate)
}

# because there are some weekends and holidays, and no loans are issued at weekends, so when we 
# want to create the daily index charts, we have to drop these days

########################## listing ###

every.day.listed.index=every.day.listed/mean(every.day.listed)*100
every.day.listed.w.index=every.day.listed.w/mean(every.day.listed.w)*100

every.day.listed.index3=every.day.listed3/mean(every.day.listed3)*100
every.day.listed.w.index3=every.day.listed.w3/mean(every.day.listed.w3)*100

every.day.listed.index5=every.day.listed5/mean(every.day.listed5)*100
every.day.listed.w.index5=every.day.listed.w5/mean(every.day.listed.w5)*100


# ##
## create a function. input the data set, output it's amount and count by day and by week
###
day=function(data, date){
  colsum=function(x) sum(as.numeric(x)) # a function to calculate the colsum
  #create to dataframe to store every day's amount and count
  d3.temp1=ddply(data,.(listed_date),colwise(colsum))
  for (i in 1:length(date)){
    if (d3.temp1$listed_date[i]!=date[i]) d3.temp1=rbind2(d3.temp1[1:i,],d3.temp1[i:dim(d3.temp1)[1],])
    if (d3.temp1$listed_date[i]!=date[i]) d3.temp1[i,]$count=0
    if (d3.temp1$listed_date[i]!=date[i]) d3.temp1[i,]$amount=0
    if (d3.temp1$listed_date[i]!=date[i]) d3.temp1[i,]$listed_date=date[i]
  }
  result=data.frame(d3.temp1$listed_date,d3.temp1$amount,d3.temp1$count)
  names(result)=c("listed_date","amount","count")
  return(result)
}

require(data.table)
## input every day's amount and count, get every week's amount and count
week=function(d3.temp1){
  #1.split the date into weeks
  week=rep(0,length(d3.temp1$listed_date))
  for (i in 2:length(date)){
    if (weekdays(date[i])=="Saturday") gap=1 else gap=0
    week[i]=week[i-1]+gap
  }
  week=as.factor(week)# count how many weeks in this period
  d3.temp1$week=week
  colsum=function(x) sum(as.numeric(x)) # a function to calculate the colsum
  
  d4=ddply(d3.temp1,.(week),colwise(colsum))
  week_start_date=c()
  week_end_date=c()
  for (i in 1:dim(d4)[1]){
    week_start_date[i]=as.character(d3.temp1$listed_date[which(d3.temp1$week==(i-1))[1]])
    week_end_date[i]=as.character(d3.temp1$listed_date[last(which(d3.temp1$week==i-1))])
  }
  d4$week_start_date=as.Date(week_start_date)
  d4$week_end_date=as.Date(week_end_date)
  result=data.frame(d4$week,d4$amount,d4$count,d4$week_start_date,d4$week_end_date)
  names(result)=c("week","amount","count","week_start_date","week_end_date")
  return(result)
}

d3=day(data,date)
ggplot(data=d3)+geom_point(data=d3,aes(x=listed_date,y=count,group=1))+
  geom_line(data=d3,aes(x=listed_date,y=count,group=1))+geom_smooth(aes(listed_date,count,group=1))
# every day's amount
ggplot(data=d3)+geom_point(data=d3,aes(x=listed_date,y=amount,group=1))+
  geom_line(data=d3,aes(x=listed_date,y=amount,group=1))+geom_smooth(aes(listed_date,amount,group=1))
# every day's average
ggplot(data=d3)+geom_point(data=d3,aes(x=listed_date,y=amount/count,group=1))+
  geom_line(data=d3,aes(x=listed_date,y=amount/count,group=1))+geom_smooth(aes(listed_date,amount/count,group=1))


### every every week's number of loans

d4=week(d3)
#every week's amount
ggplot(data=d4)+geom_point(data=d4,aes(x=week,y=amount,group=1))+
  geom_line(data=d4,aes(x=week,y=amount,group=1))+geom_smooth(aes(week,amount,group=1))
#every week's volumn
ggplot(data=d4)+geom_point(data=d4,aes(x=week_start_date,y=count,group=1))+
  geom_line(data=d4,aes(x=week_start_date,y=count,group=1))+geom_smooth(aes(week_start_date,count,group=1))
# every week's average
ggplot(data=d4)+geom_point(data=d4,aes(x=week,y=amount/count,group=1))+
  geom_line(data=d4,aes(x=week,y=amount/count,group=1))+geom_smooth(aes(week,amount/count,group=1))

