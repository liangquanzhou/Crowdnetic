#### index function ####
### input dataset and date (time line), output a list contains 4 index data frames, and every data frame 
### has 6 columns: all loans, all loans weighted, 3 year loans, 3 year loans weighted, 5 year loans, 5 year loans weighted
### 1. listed + culumative , 2. listed + every day ave, 3. issued + cululative, 4. issued + every day ave

index=function(data,date){
  ## data should have these columns: loan amount, term, borrower rate, list date, issued date ##
  data3=data[which(data$Term=="36"),]
  
  data5=data[which(data$Term=="60"),]
  
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
    
    date.listed.value[i]=sum(data[which(data$ListingCreationDate<=date[i]),]$LoanOriginalAmount)
    every.day.listed[i]=sum(data[which(data$ListingCreationDate<=date[i] & data$LoanOriginationDate>=date[i]),]$LoanOriginalAmount)
    every.day.listed.ave[i]=mean(data[which(data$ListingCreationDate<=date[i] & data$LoanOriginationDate>=date[i]),]$LoanOriginalAmount)
    date.listed.value.w[i]=sum(data[which(data$ListingCreationDate<=date[i]),]$LoanOriginalAmount*
        data[which(data$ListingCreationDate<=date[i]),]$BorrowerRate)
    every.day.listed.w[i]=sum(data[which(data$ListingCreationDate<=date[i] & data$LoanOriginationDate>=date[i]),]$LoanOriginalAmount*
        data[which(data$ListingCreationDate<=date[i] & data$LoanOriginationDate>=date[i]),]$BorrowerRate)
    every.day.listed.ave.w[i]=mean(data[which(data$ListingCreationDate<=date[i] & data$LoanOriginationDate>=date[i]),]$LoanOriginalAmount*
        data[which(data$ListingCreationDate<=date[i] & data$LoanOriginationDate>=date[i]),]$BorrowerRate)
    
    date.listed.value3[i]=sum(data3[which(data3$ListingCreationDate<=date[i]),]$LoanOriginalAmount)
    every.day.listed3[i]=sum(data3[which(data3$ListingCreationDate<=date[i] & data3$LoanOriginationDate>=date[i]),]$LoanOriginalAmount)
    every.day.listed.ave3[i]=mean(data3[which(data3$ListingCreationDate<=date[i] & data3$LoanOriginationDate>=date[i]),]$LoanOriginalAmount)
    date.listed.value.w3[i]=sum(data3[which(data3$ListingCreationDate<=date[i]),]$LoanOriginalAmount*
        data3[which(data3$ListingCreationDate<=date[i]),]$BorrowerRate)
    every.day.listed.w3[i]=sum(data3[which(data3$ListingCreationDate<=date[i] & data3$LoanOriginationDate>=date[i]),]$LoanOriginalAmount*
        data3[which(data3$ListingCreationDate<=date[i] & data3$LoanOriginationDate>=date[i]),]$BorrowerRate)
    every.day.listed.ave.w3[i]=mean(data3[which(data3$ListingCreationDate<=date[i] & data3$LoanOriginationDate>=date[i]),]$LoanOriginalAmount*
        data3[which(data3$ListingCreationDate<=date[i] & data3$LoanOriginationDate>=date[i]),]$BorrowerRate)
    
    date.listed.value5[i]=sum(data5[which(data5$ListingCreationDate<=date[i]),]$LoanOriginalAmount)
    every.day.listed5[i]=sum(data5[which(data5$ListingCreationDate<=date[i] & data5$LoanOriginationDate>=date[i]),]$LoanOriginalAmount)
    every.day.listed.ave5[i]=mean(data5[which(data5$ListingCreationDate<=date[i] & data5$LoanOriginationDate>=date[i]),]$LoanOriginalAmount)
    date.listed.value.w5[i]=sum(data5[which(data5$ListingCreationDate<=date[i]),]$LoanOriginalAmount*
        data5[which(data5$ListingCreationDate<=date[i]),]$BorrowerRate)
    every.day.listed.w5[i]=sum(data5[which(data5$ListingCreationDate<=date[i] & data5$LoanOriginationDate>=date[i]),]$LoanOriginalAmount*
        data5[which(data5$ListingCreationDate<=date[i] & data5$LoanOriginationDate>=date[i]),]$BorrowerRate)
    every.day.listed.ave.w5[i]=mean(data5[which(data5$ListingCreationDate<=date[i] & data5$LoanOriginationDate>=date[i]),]$LoanOriginalAmount*
        data5[which(data5$ListingCreationDate<=date[i] & data5$LoanOriginationDate>=date[i]),]$BorrowerRate)
  }

  date.listed.value.index=date.listed.value/date.listed.value[1]*100
  date.listed.value.w.index=date.listed.value.w/date.listed.value.w[1]*100
  
  every.day.listed.index=every.day.listed/every.day.listed[1]*100
  every.day.listed.w.index=every.day.listed.w/every.day.listed.w[1]*100
  
  every.day.listed.ave.index=every.day.listed.ave /every.day.listed.ave[1]*100
  every.day.listed.ave.w.index=every.day.listed.ave.w/every.day.listed.ave.w[1]*100
  
  
  date.listed.value.index3=date.listed.value3/date.listed.value3[1]*100
  date.listed.value.w.index3=date.listed.value.w3/date.listed.value.w3[1]*100
  
  every.day.listed.index3=every.day.listed3/every.day.listed3[1]*100
  every.day.listed.w.index3=every.day.listed.w3/every.day.listed.w3[1]*100
  
  every.day.listed.ave.index3=every.day.listed.ave3/every.day.listed.ave3[1]*100
  every.day.listed.ave.w.index3=every.day.listed.ave.w3/every.day.listed.ave.w3[1]*100
  
  
  date.listed.value.index5=date.listed.value5/date.listed.value5[1]*100
  date.listed.value.w.index5=date.listed.value.w5/date.listed.value.w5[1]*100
  
  every.day.listed.index5=every.day.listed5/every.day.listed5[1]*100
  every.day.listed.w.index5=every.day.listed.w5/every.day.listed.w5[1]*100
  
  every.day.listed.ave.index5=every.day.listed.ave5/every.day.listed.ave5[1]*100
  every.day.listed.ave.w.index5=every.day.listed.ave.w5/every.day.listed.ave.w5[1]*100
  
  
  
  
  
  #index.c=data.frame(date.listed.value.w.index,
   # date.listed.value.w.index3,date.listed.value.w.index5,date)
#  index.c1=c(date.listed.value.w.index,date.listed.value.w.index3,date.listed.value.w.index5)
#  date1=rep(date,3)
#  groups=c(rep("all_loans",length(date)),rep("3_year_loans",length(date)),rep("5_year_loans",length(date)))
#  index.c=data.frame(index.c1,date1,groups)
#  names(index.c)=c("index","date","group")
  
  #index.e=data.frame(every.day.listed.ave.w.index,
   # every.day.listed.ave.w.index3,every.day.listed.ave.w.index5,date)
#  index.e1=c(every.day.listed.ave.w.index,
#     every.day.listed.ave.w.index3,every.day.listed.ave.w.index5)
#  index.e=data.frame(index.e1,date1,groups)
#  names(index.e)=c("index","date","group")
  
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
  
  
  
  for (i in 1:length(date)){
    
    date.issued.value[i]=sum(data[which(data$LoanOriginationDate<=date[i]),]$LoanOriginalAmount)
    every.day.issued[i]=sum(data[which(data$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
    every.day.issued.ave[i]=mean(data[which(data$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
    date.issued.value.w[i]=sum(data[which(data$LoanOriginationDate<=date[i]),]$LoanOriginalAmount*
        data[which(data$LoanOriginationDate<=date[i]),]$BorrowerRate)
    every.day.issued.w[i]=sum(data[which(data$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
        data[which(data$LoanOriginationDate==date[i]),]$BorrowerRate)
    every.day.issued.ave.w[i]=mean(data[which(data$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
        data[which(data$LoanOriginationDate==date[i]),]$BorrowerRate)
    
    date.issued.value3[i]=sum(data3[which(data3$LoanOriginationDate<=date[i]),]$LoanOriginalAmount)
    every.day.issued3[i]=sum(data3[which(data3$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
    every.day.issued.ave3[i]=mean(data3[which(data3$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
    date.issued.value.w3[i]=sum(data3[which(data3$LoanOriginationDate<=date[i]),]$LoanOriginalAmount*
        data3[which(data3$LoanOriginationDate<=date[i]),]$BorrowerRate)
    every.day.issued.w3[i]=sum(data3[which(data3$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
        data3[which(data3$LoanOriginationDate==date[i]),]$BorrowerRate)
    every.day.issued.ave.w3[i]=mean(data3[which(data3$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
        data3[which(data3$LoanOriginationDate==date[i]),]$BorrowerRate)
    
    date.issued.value5[i]=sum(data5[which(data5$LoanOriginationDate<=date[i]),]$LoanOriginalAmount)
    every.day.issued5[i]=sum(data5[which(data5$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
    every.day.issued.ave5[i]=mean(data5[which(data5$LoanOriginationDate==date[i]),]$LoanOriginalAmount)
    date.issued.value.w5[i]=sum(data5[which(data5$LoanOriginationDate<=date[i]),]$LoanOriginalAmount*
        data5[which(data5$LoanOriginationDate<=date[i]),]$BorrowerRate)
    every.day.issued.w5[i]=sum(data5[which(data5$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
        data5[which(data5$LoanOriginationDate==date[i]),]$BorrowerRate)
    every.day.issued.ave.w5[i]=mean(data5[which(data5$LoanOriginationDate==date[i]),]$LoanOriginalAmount*
        data5[which(data5$LoanOriginationDate==date[i]),]$BorrowerRate)
  }
  
  # because there are some weekends and holidays, and no loans are issued at weekends, so when we 
  # want to create the daily index charts, we have to drop these days
  index=which(every.day.issued!=0)
  work.date=date[which(every.day.issued!=0)]
  
  # set the first day's index = 100 
  date.issued.value.index=date.issued.value/date.issued.value[1]*100
  date.issued.value.w.index=date.issued.value.w/date.issued.value.w[1]*100
  
  every.day.issued.index=every.day.issued[index]/every.day.issued[index][1]*100
  every.day.issued.w.index=every.day.issued.w[index]/every.day.issued.w[index][1]*100
  
  every.day.issued.ave.index=every.day.issued.ave[index]/every.day.issued.ave[index][1]*100
  every.day.issued.ave.w.index=every.day.issued.ave.w[index]/every.day.issued.ave.w[index][1]*100
  
  
  date.issued.value.index3=date.issued.value3/date.issued.value3[1]*100
  date.issued.value.w.index3=date.issued.value.w3/date.issued.value.w3[1]*100
  
  every.day.issued.index3=every.day.issued3[index]/every.day.issued3[index][1]*100
  every.day.issued.w.index3=every.day.issued.w3[index]/every.day.issued.w3[index][1]*100
  
  every.day.issued.ave.index3=every.day.issued.ave3[index]/every.day.issued.ave3[index][1]*100
  every.day.issued.ave.w.index3=every.day.issued.ave.w3[index]/every.day.issued.ave.w3[index][1]*100
  
  
  date.issued.value.index5=date.issued.value5/date.issued.value5[1]*100
  date.issued.value.w.index5=date.issued.value.w5/date.issued.value.w5[1]*100
  
  every.day.issued.index5=every.day.issued5[index]/every.day.issued5[index][1]*100
  every.day.issued.w.index5=every.day.issued.w5[index]/every.day.issued.w5[index][1]*100
  
  every.day.issued.ave.index5=every.day.issued.ave5[index]/every.day.issued.ave5[index][1]*100
  every.day.issued.ave.w.index5=every.day.issued.ave.w5[index]/every.day.issued.ave.w5[index][1]*100
  
  
  issued.c=data.frame(date.issued.value.index,
  date.issued.value.w.index,
  date.issued.value.index3,
  date.issued.value.w.index3,
  date.issued.value.index5,
  date.issued.value.w.index5,
  date)
  names(issued.c)=c("all_loans","all_loans_w","three_year_loans","three_year_loans_w","five_year_loans",
    "five_year_loans_w","date")
  
  issued.e.a=data.frame(every.day.issued.ave.index,
    every.day.issued.ave.w.index,
    every.day.issued.ave.index3,
    every.day.issued.ave.w.index3,
    every.day.issued.ave.index5,
    every.day.issued.ave.w.index5,
    work.date)
  names(issued.e.a)=c("all_loans","all_loans_w","three_year_loans","three_year_loans_w","five_year_loans",
    "five_year_loans_w","date")
  
  issued.e=data.frame(every.day.issued.index,
    every.day.issued.w.index,
    every.day.issued.index3,
    every.day.issued.w.index3,
    every.day.issued.index5,
    every.day.issued.w.index5,
    work.date)
  names(issued.e)=c("all_loans","all_loans_w","three_year_loans","three_year_loans_w","five_year_loans",
    "five_year_loans_w","date")
  
  listed.c=data.frame(date.listed.value.index,
    date.listed.value.w.index,
    date.listed.value.index3,
    date.listed.value.w.index3,
    date.listed.value.index5,
    date.listed.value.w.index5,
    date)
  names(listed.c)=c("all_loans","all_loans_w","three_year_loans","three_year_loans_w","five_year_loans",
    "five_year_loans_w","date")
  
  listed.e.a=data.frame(every.day.listed.ave.index,
    every.day.listed.ave.w.index,
    every.day.listed.ave.index3,
    every.day.listed.ave.w.index3,
    every.day.listed.ave.index5,
    every.day.listed.ave.w.index5,
    date)
  names(listed.e.a)=c("all_loans","all_loans_w","three_year_loans","three_year_loans_w","five_year_loans",
    "five_year_loans_w","date")
  
  listed.e=data.frame(every.day.listed.index,
    every.day.listed.w.index,
    every.day.listed.index3,
    every.day.listed.w.index3,
    every.day.listed.index5,
    every.day.listed.w.index5,
    date)
  names(listed.e)=c("all_loans","all_loans_w","three_year_loans","three_year_loans_w","five_year_loans",
    "five_year_loans_w","date")
  
  return(list(issued.c,issued.e,listed.c,listed.e))
  
}

ggplot(index.e[1:90,],aes(x=date,y=index,colour=group))+geom_line(shape=1)+geom_point()

plot(date,listed.c$three_year_loans,type="l",ylab="index value")
+legend("topleft",lty=1,col=c(1,2),legend=c("unweighted","weighted"),cex=0.8)

anew=index(dat,date)
issued.c=anew[[1]]
issued.e=anew[[2]]
listed.c=anew[[3]]
listed.e=anew[[4]]

g1=
ggplot(data=listed.c)+geom_line(data=listed.c,aes(x=date,y=all_loans,colour="all loans"))+
  geom_point(data=listed.c,aes(x=date,y=all_loans,colour="all loans"))+
  geom_line(data=listed.c,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_point(data=listed.c,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_line(data=listed.c,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  geom_point(data=listed.c,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  scale_colour_manual("Group",values = c("all loans" = "black","all loans w" = "red","3 year loans"="blue"))+
  scale_x_date(limits = as.Date(c("2014-05-01","2014-06-30")),labels = date_format("%Y-%m-%d"))+
  xlab("Date")+ylab("Index Value")+ggtitle("listed .c")
g1
ggsave(filename="listed + cumulative.png")

g2=
  ggplot(data=listed.e)+geom_line(data=listed.e,aes(x=date,y=all_loans,colour="all loans"))+
  geom_point(data=listed.e,aes(x=date,y=all_loans,colour="all loans"))+
  geom_line(data=listed.e,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_point(data=listed.e,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_line(data=listed.e,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  geom_point(data=listed.e,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  scale_colour_manual("Group",values = c("all loans" = "black","all loans w" = "red","3 year loans"="blue"))+
  scale_x_date(limits = as.Date(c("2014-05-01","2014-06-30")),labels = date_format("%Y-%m-%d"))+
  xlab("Date")+ylab("Index Value")+ggtitle("on listing.e")
g2
ggsave(filename="on listing  + every day.png")

g3=
  ggplot(data=listed.e.a)+geom_line(data=listed.e.a,aes(x=date,y=all_loans,colour="all loans"))+
  geom_point(data=listed.e.a,aes(x=date,y=all_loans,colour="all loans"))+
  geom_line(data=listed.e.a,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_point(data=listed.e.a,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_line(data=listed.e.a,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  geom_point(data=listed.e.a,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  scale_colour_manual("Group",values = c("all loans" = "black","all loans w" = "red","3 year loans"="blue"))+
  scale_x_date(limits = as.Date(c("2014-05-01","2014-06-30")),labels = date_format("%Y-%m-%d"))+
  xlab("Date")+ylab("Index Value")+ggtitle("on listing .e.a")
g3
ggsave(filename="on listing  + every day's average amount.png")

g4=
  ggplot(data=issued.c)+geom_line(data=issued.c,aes(x=date,y=all_loans,colour="all loans"))+
  geom_point(data=issued.c,aes(x=date,y=all_loans,colour="all loans"))+
  geom_line(data=issued.c,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_point(data=issued.c,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_line(data=issued.c,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  geom_point(data=issued.c,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  scale_colour_manual("Group",values = c("all loans" = "black","all loans w" = "red","3 year loans"="blue"))+
  scale_x_date(limits = as.Date(c("2014-01-01","2014-07-29")),labels = date_format("%Y-%m-%d"))+
  xlab("Date")+ylab("Index Value")+ggtitle("issued.c")
g4
ggsave("issued + cumulative.png")

g5=
  ggplot(data=issued.e)+geom_line(data=issued.e,aes(x=date,y=all_loans,colour="all loans"))+
  geom_point(data=issued.e,aes(x=date,y=all_loans,colour="all loans"))+
  geom_line(data=issued.e,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_point(data=issued.e,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_line(data=issued.e,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  geom_point(data=issued.e,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  scale_colour_manual("Group",values = c("all loans" = "black","all loans w" = "red","3 year loans"="blue"))+
  scale_x_date(limits = as.Date(c("2014-01-01","2014-07-29")),labels = date_format("%Y-%m-%d"))+
  xlab("Date")+ylab("Index Value")+ggtitle("issued.e")
g5
ggsave("issued + everyday.png")

g6=
  ggplot(data=issued.e.a)+geom_line(data=issued.e.a,aes(x=date,y=all_loans,colour="all loans"))+
  geom_point(data=issued.e.a,aes(x=date,y=all_loans,colour="all loans"))+
  geom_line(data=issued.e.a,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_point(data=issued.e.a,aes(x=date,y=all_loans_w,colour="all loans w"))+
  geom_line(data=issued.e.a,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  geom_point(data=issued.e.a,aes(x=date,y=three_year_loans,colour="3 year loans"))+
  scale_colour_manual("Group",values = c("all loans" = "black","all loans w" = "red","3 year loans"="blue"))+
  scale_x_date(limits = as.Date(c("2014-01-01","2014-07-29")),labels = date_format("%Y-%m-%d"))+
  xlab("Date")+ylab("Index Value")+ggtitle("issued.e.a")
g6
ggsave("issued + everyday's average amount.png")