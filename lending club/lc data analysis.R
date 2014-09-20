lc.dat=read.table("LoanStats3c.csv",header=T, stringsAsFactors=T, sep=",")
lc.dat.gradeA1=lc.dat[which(lc.dat$sub_grade=="A1"),]
lc.dat.gradeA=lc.dat[which(lc.dat$grade=="A"),]
test.dat=lc.dat.gradeA[which(lc.dat.gradeA$loan_amnt<5000),]
View(test.dat[which(test.dat$sub_grade=="A1"),])
