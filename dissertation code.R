####Infant ancova code
#load data
library(readxl)
infant_sleep <- read_excel("dissertation/infant sleep.xlsx")
names(infant_sleep)


#####3(2-4) month baby sleep outcome analysis (ancova)
data=subset(infant_sleep,infant_sleep$babyAge==3|infant_sleep$babyAge==2|infant_sleep$babyAge==4)
#Generate model residuals and predictions
library(zoo)
library(lmtest)
lmresults1=lm(TotaltimeR~NBT*location,data = data)
res1<-rstandard(lmresults1)

lmresults2=lm(TotaltimeC~NBT*location,data = data)
res2<-rstandard(lmresults2)

lmresults3=lm(LongestPeriod~NBT*location,data = data)
res3<-rstandard(lmresults3)

lmresults4=lm(sleepDaytime~NBT*location,data = data)
res4<-rstandard(lmresults4)

lmresults5=lm(STNT~NBT*location,data = data)
res5<-rstandard(lmresults5)

lmresults6=lm(Nightwake~NBT*location,data = data)
res6<-rstandard(lmresults6)
#add to dataframe
data0=data.frame(data,res1,res2,res3,res4,res5,res6)
#seprate by location
data1=subset(data0,data0$location=="SS")
data2=subset(data0,data0$location=="RS")
data3=subset(data0,data0$location=="BS")
# 将location因子化
data0$location <- factor(data0$location)

# 2-4 month infant acnova
#1、TotaltimeR
#1.1 Outlier determination
boxplot(data0$res1~data0$location,
        xlab = c("location"), ylab = expression("res1"))
#1.2 Normality test
shapiro.test(data0$res1)
#1.3Chi-squared test
library(carData)
library(car)
leveneTest(res1~location,data = data0)
#1.4 Linear relationship test
#Plotting the correlation between NBT and TotaltimeR
library(ggplot2)
ggplot(data=data0, aes(x=NBT, y=TotaltimeR,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
## Correlation coefficient test  ##
cor.test(tdata1$TotaltimeR,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$TotaltimeR,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$TotaltimeR,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$TotaltimeR,data0$NBT,method = "pearson",alternative = "two.sided")
###Check if there is a relationship between NBT and location###
NBTaov <- aov(NBT~location, data = data0)
summary(NBTaov)

##1.5 Parallelism test
library(mvtnorm)
library(survival)
library(TH.data)
library(MASS)
library(TH.data)
library(multcomp)
fit1<- aov(TotaltimeR ~ NBT *location, data = data0)
summary(fit1)

##1.6、Statistical description
#Overall mean variance
mean(data$TotaltimeR)
sd(data$TotaltimeR)
#Mean variance of each group
library(psych)
describeBy(data$TotaltimeR, group = data$location)
##1.7Analysis of covariance
fit1_2=aov(TotaltimeR~NBT+location,data=data0)
summary(fit1_2)

#2、TotaltimeC
#2.1 Outlier determination
boxplot(data0$res2~data0$location,
        xlab = c("location"), ylab = expression("res2"))
#2.2 Normality test
shapiro.test(data0$res2)#p-value = 0.08659>0.01
#2.3Chi-squared test
leveneTest(res2~location,data = data0)

##2.4Linear relationship test
#Plotting the correlation between NBT and TotaltimeC
ggplot(data=data0, aes(x=NBT, y=TotaltimeC,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
## Correlation coefficient test ##
cor.test(data1$TotaltimeC,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$TotaltimeC,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$TotaltimeC,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$TotaltimeC,data0$NBT,method = "pearson",alternative = "two.sided")
##2.5 Parallelism test
fit2<- aov(TotaltimeC ~ NBT *location, data = data0)
summary(fit2)
##2.6、Statistical description
#Overall mean variance
mean(data$TotaltimeC)
sd(data$TotaltimeC)
#Mean variance of each group
describeBy(data$TotaltimeC, group = data$location)
##2.7Analysis of covariance
fit2_2=aov(TotaltimeC~NBT+location,data=data0)
summary(fit2_2)


#3、LongestPeriod
#3.1 Outlier determination
boxplot(data0$res3~data0$location,
        xlab = c("location"), ylab = expression("res3"))
#3.2 Normality test
shapiro.test(data0$res3)
#3.3Chi-squared test
leveneTest(res3~location,data = data0)

##3.4 Linear relationship test
#Plotting the correlation between NBT and LongestPeriod
ggplot(data=data0, aes(x=NBT, y=LongestPeriod,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
## Correlation coefficient test  ##
cor.test(data1$LongestPeriod,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$LongestPeriod,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$LongestPeriod,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$LongestPeriod,data0$NBT,method = "pearson",alternative = "two.sided")
##3.5 Parallelism test
fit3<- aov(LongestPeriod ~ NBT *location, data = data0)
summary(fit3)
##3.6、Statistical description
mean(data$LongestPeriod)
sd(data$LongestPeriod)
describeBy(data$LongestPeriod, group = data$location)
##3.7Analysis of covariance
fit3_2=aov(LongestPeriod~NBT+location,data=data0)
summary(fit3_2)
##3.8 Post hoc analysis
fNBT=factor(data0$NBT)
data0=data.frame(data0,fNBT)
TukeyHSD(aov(LongestPeriod~fNBT+location,data=data0),"location")

##4、sleepDaytime
#4.1 Outlier determination
boxplot(data0$res4~data0$location,
        xlab = c("location"), ylab = expression("res4"))
#4.2 Normality test
shapiro.test(data0$res4)#p-value = 7.766e-14<0.01
#4.3Chi-squared test
leveneTest(res4~location,data = data0)
##4.4Linear relationship test
#Plot the correlation between NBT and sleepDaytime
ggplot(data=data0, aes(x=NBT, y=sleepDaytime,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
## Correlation coefficient test  ##
cor.test(data1$sleepDaytime,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$sleepDaytime,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$sleepDaytime,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$sleepDaytime,data0$NBT,method = "pearson",alternative = "two.sided")
##4.5 Parallelism test
fit4<- aov(sleepDaytime ~ NBT *location, data = data0)
summary(fit4)
##4.6、Statistical description
mean(data$sleepDaytime)
sd(data$sleepDaytime)
describeBy(data$sleepDaytime, group = data$location)
##4.7Analysis of covariance
fit4_2=aov(sleepDaytime~NBT+location,data=data0)
summary(fit4_2)

#5、STNT
#5.1 Outlier determination
boxplot(data0$res5~data0$location,
        xlab = c("location"), ylab = expression("res5"))
##Remove outliers
quartiles <- quantile(data3$STNT, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data3$STNT)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
data3_no_outlier <- subset(data3,data3$STNT > Lower & data3$STNT < Upper)
data0_no_outlier<-rbind(data1,data2,data3_no_outlier)
#After Checking Outlier Removal
data0_no_outlier$location=factor(data0_no_outlier$location)
boxplot(data0_no_outlier$res5~data0_no_outlier$location,
        xlab = c("location"), ylab = expression("res5"))
#5.2 Normality test
shapiro.test(data0_no_outlier$res5)
#5.3 Chi-squared test
leveneTest(res5~location,data = data0_no_outlier)
##5.4Linear relationship test
ggplot(data=data0_no_outlier, aes(x=NBT, y=STNT,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$STNT,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$STNT,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$STNT,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0_no_outlier$STNT,data0_no_outlier$NBT,method = "pearson",alternative = "two.sided")
##5.5 Parallelism test
fit5<- aov(STNT ~ NBT *location, data = data0_no_outlier)
summary(fit5)
##5.6、Statistical description
mean(data0_no_outlier$STNT)
sd(data0_no_outlier$STNT)
describeBy(data0_no_outlier$STNT, group = data0_no_outlier$location)
##5.7 Analysis of covariance
fit5_2=aov(STNT~NBT+location,data=data0_no_outlier)
summary(fit5_2)
##5.8 Post hoc analysis
library(emmeans)
model=lm(LongestPeriod ~ factor(NBT)+location, data = data0_no_outlier)
modelemm=emmeans(model,"location")
pairs(modelemm)

#6、Nightwake
#6.1 Outlier determination
boxplot(data0$res6~data0$location,
        xlab = c("location"), ylab = expression("res6"))
#6.2 Normality test
shapiro.test(data0$res6)#p-value = 3.315e-12<0.01
#6.3 Chi-squared test
leveneTest(res6~location,data = data0)
##6.4 Linear relationship test
ggplot(data=data0, aes(x=NBT, y=Nightwake,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$Nightwake,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$Nightwake,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$Nightwake,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$Nightwake,data0$NBT,method = "pearson",alternative = "two.sided")
##6.5 Parallelism test
fit6<- aov(Nightwake ~ NBT *location, data = data0)
summary(fit6)
##6.6、Statistical description
mean(data$Nightwake)
sd(data$Nightwake)
describeBy(data$Nightwake, group =data$location)
##6.7Analysis of covariance
fit6_2=aov(Nightwake~NBT+location,data=data0)
summary(fit6_2)





#####6(5-7) month baby sleep outcome analysis (ancova)

data=subset(infant_sleep,infant_sleep$babyAge==6|infant_sleep$babyAge==5|infant_sleep$babyAge==7)

#Generate model residuals and predictions
library(zoo)
library(lmtest)
lmresults1=lm(TotaltimeR~NBT*location,data = data)
res1<-rstandard(lmresults1)
lmresults2=lm(TotaltimeC~NBT*location,data = data)
res2<-rstandard(lmresults2)
lmresults3=lm(LongestPeriod~NBT*location,data = data)
res3<-rstandard(lmresults3)
lmresults4=lm(sleepDaytime~NBT*location,data = data)
res4<-rstandard(lmresults4)
lmresults5=lm(STNT~NBT*location,data = data)
res5<-rstandard(lmresults5)
lmresults6=lm(Nightwake~NBT*location,data = data)
res6<-rstandard(lmresults6)
#add to dataframe
data0=data.frame(data,res1,res2,res3,res4,res5,res6)
#seprate by location
data1=subset(data0,data0$location=="SS")
data2=subset(data0,data0$location=="RS")
data3=subset(data0,data0$location=="BS")
# Factorize location
data0$location <- factor(data0$location)


#1、TotaltimeR
#1.1 Outlier determination
boxplot(data0$res1~data0$location,
        xlab = c("location"), ylab = expression("res1"))
#1.2 Normality test
shapiro.test(data0$res1)
#1.3 Chi-squared test
library(carData)
library(car)
leveneTest(res1~location,data = data0)
##1.4Linear relationship test
library(ggplot2)
ggplot(data=data0, aes(x=NBT, y=TotaltimeR,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(tdata1$TotaltimeR,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$TotaltimeR,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$TotaltimeR,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$TotaltimeR,data0$NBT,method = "pearson",alternative = "two.sided")
###Check if there is a relationship between NBT and location###
NBTaov <- aov(NBT~location, data = data0)
summary(NBTaov)

##1.5 Parallelism test
library(mvtnorm)
library(survival)
library(TH.data)
library(MASS)
library(TH.data)
library(multcomp)
fit1<- aov(TotaltimeR ~ NBT *location, data = data0)
summary(fit1)
##1.6、Statistical description
mean(data$TotaltimeR)
sd(data$TotaltimeR)
library(psych)
describeBy(data$TotaltimeR, group = data$location)
##1.7Analysis of covariance
fit1_2=aov(TotaltimeR~NBT+location,data=data0)
summary(fit1_2)
#1.8 Post hoc analysis
fNBT=factor(data0$NBT)
data0=data.frame(data0,fNBT)
TukeyHSD(aov(TotaltimeR~fNBT+location,data=data0),"location")

#2、TotaltimeC
#2.1 Outlier determination
boxplot(data0$res2~data0$location,
        xlab = c("location"), ylab = expression("res2"))
#2.2 Normality test
shapiro.test(data0$res2)#W = 0.87349, p-value = 6e-16
#2.3Chi-squared test
leveneTest(res2~location,data = data0)
##2.4 Linear relationship test
ggplot(data=data0, aes(x=NBT, y=TotaltimeC,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$TotaltimeC,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$TotaltimeC,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$TotaltimeC,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$TotaltimeC,data0$NBT,method = "pearson",alternative = "two.sided")
##2.5 Parallelism test
fit2<- aov(TotaltimeC ~ NBT *location, data = data0)
summary(fit2)
##2.6、Statistical description
mean(data$TotaltimeC)
sd(data$TotaltimeC)
describeBy(data$TotaltimeC, group = data$location)
##2.7Analysis of covariance
fit2_2=aov(TotaltimeC~NBT+location,data=data0)
summary(fit2_2)
#2.8Post hoc analysis
library(emmeans)
model=lm(TotaltimeC ~ factor(NBT)+location, data = data0)
modelemm=emmeans(model,"location")
pairs(modelemm)


#3、LongestPeriod
#3.1 Outlier determination
boxplot(data0$res3~data0$location,
        xlab = c("location"), ylab = expression("res3"))#bs:很多
#Removal of outliers
quartiles <- quantile(data3$LongestPeriod, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data3$LongestPeriod)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
data3_no_outlier <- subset(data3,data3$LongestPeriod > Lower & data3$LongestPeriod< Upper)
data0_no_outlier<-rbind(data1,data2,data3_no_outlier)
#After checking for outliers
data0_no_outlier$location=factor(data0_no_outlier$location)
boxplot(data0_no_outlier$res3~data0_no_outlier$location,
        xlab = c("location"), ylab = expression("res3"))
#3.2 Normality test
shapiro.test(data0_no_outlier$res3)#p-value = 5.179e-06>0.01
#3.3 Chi-squared test
leveneTest(res3~location,data = data0_no_outlier)#p=5.779e-11 *** 本案例组间因变量的方差不齐
##3.4 Linear relationship test
ggplot(data=data0_no_outlier, aes(x=NBT, y=LongestPeriod,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$LongestPeriod,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$LongestPeriod,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$LongestPeriod,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0_no_outlier$LongestPeriod,data0_no_outlier$NBT,method = "pearson",alternative = "two.sided")
##3.5 Parallelism test
fit3<- aov(LongestPeriod ~ NBT *location, data = data0_no_outlier)
summary(fit3)
##3.6、Statistical description
mean(data0_no_outlier$LongestPeriod)
sd(data0_no_outlier$LongestPeriod)
describeBy(data0_no_outlier$LongestPeriod, group = data0_no_outlier$location)
fit3_2=aov(LongestPeriod~NBT+location,data=data0_no_outlier)
summary(fit3_2)#F=16.38 1.68e-07 ***
##3.8Post hoc analysis
fNBT=factor(data0_no_outlier$NBT)
data0_no_outlier=data.frame(data0_no_outlier,fNBT)
TukeyHSD(aov(LongestPeriod~fNBT+location,data=data0_no_outlier),"location")


#4、sleepDaytime
#4.1 Outlier determination
boxplot(data0$res4~data0$location,
        xlab = c("location"), ylab = expression("res4"))
#4.2 Normality test
shapiro.test(data0$res4#p-value = 1.75e-14<0.01
#4.3Chi-squared test
leveneTest(res4~location,data = data0)
##4.4Linear relationship test
ggplot(data=data0, aes(x=NBT, y=sleepDaytime,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$sleepDaytime,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$sleepDaytime,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$sleepDaytime,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$sleepDaytime,data0$NBT,method = "pearson",alternative = "two.sided")
##4.5 Parallelism test
fit4<- aov(sleepDaytime ~ NBT *location, data = data0)
summary(fit4)
##4.6 Statistical description
mean(data$sleepDaytime)
sd(data$sleepDaytime)
 describeBy(data$sleepDaytime, group = data$location)
##4.7Analysis of covariance
fit4_2=aov(sleepDaytime~NBT+location,data=data0)
summary(fit4_2)
#4.8 Post hoc analysis
TukeyHSD(aov(sleepDaytime~fNBT+location,data=data0),"location")

##5、response:STNT
#5.1 Outlier determination
boxplot(data0$res5~data0$location,
       xlab = c("location"), ylab = expression("res5"))#bs有很多异常值
#5.2 Normality test
shapiro.test(data0$res5)#p-value = 7.862e-13>0.01
#5.3Chi-squared test
leveneTest(res5~location,data = data0)
##5.4Linear relationship test
ggplot(data=data0, aes(x=NBT, y=STNT,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$STNT,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$STNT,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$STNT,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$STNT,data0$NBT,method = "pearson",alternative = "two.sided")
##5.5 Parallelism test
fit5<- aov(STNT ~ NBT *location, data = data)
summary(fit5)
##5.6、Statistical description
mean(data0$STNT)
sd(data0$STNT)
describeBy(data0$STNT, group = sixdata0$location)
##5.7Analysis of covariance
fit5_2=aov(STNT~NBT+location,data=data)
summary(fit5_2)


#6、Nightwake
#6.1 Outlier determination
boxplot(data0$res6~data0$location,
        xlab = c("location"), ylab = expression("res6"))
#6.2 Normality test
shapiro.test(data0$res6)#p-value = W = 0.90407, p-value = 9.79e-14<0.01
#6.3Chi-squared test
leveneTest(res6~location,data = data0)
##6.4Linear relationship test
ggplot(data=data0, aes(x=NBT, y=Nightwake,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$Nightwake,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$Nightwake,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$Nightwake,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$Nightwake,data0$NBT,method = "pearson",alternative = "two.sided")
#cor=0.5347749   ,p=< 2.2e-16
##6.5 Parallelism test
fit6<- aov(Nightwake ~ NBT *location, data = data0)
summary(fit6)
##6.6、Statistical description
mean(data$Nightwake)
sd(data$Nightwake)
describeBy(data$Nightwake, group =data$location)
##6.7Analysis of variance
fit6_2=aov(Nightwake~location,data=data0)
summary(fit6_2)
##6.8 Post hoc analysis
TukeyHSD(aov(Nightwake~location,data=data0),"location")




#####9(8-10) month baby sleep outcome analysis (ancova)
data=subset(infant_sleep,infant_sleep$babyAge==9|infant_sleep$babyAge==8|infant_sleep$babyAge==10)
#Generate model residuals and predictions
library(zoo)
library(lmtest)
lmresults1=lm(TotaltimeR~NBT*location,data = data)
res1<-rstandard(lmresults1)
lmresults2=lm(TotaltimeC~NBT*location,data = data)
res2<-rstandard(lmresults2)
lmresults3=lm(LongestPeriod~NBT*location,data = data)
res3<-rstandard(lmresults3)
lmresults4=lm(sleepDaytime~NBT*location,data = data)
res4<-rstandard(lmresults4)
lmresults5=lm(STNT~NBT*location,data = data)
res5<-rstandard(lmresults5)
lmresults6=lm(Nightwake~NBT*location,data = data)
res6<-rstandard(lmresults6)
#add to dataframe
data0=data.frame(data,res1,res2,res3,res4,res5,res6)
#seprate by location
data1=subset(data0,data0$location=="SS")
data2=subset(data0,data0$location=="RS")
data3=subset(data0,data0$location=="BS")
# Factorize location
data0$location <- factor(data0$location)

#1、sleep total time(reported)
#1.1 Outlier determination
boxplot(data0$res1~data0$location,
        xlab = c("location"), ylab = expression("res1"))#bs:3.rs:1
#1.2 Normality test
shapiro.test(data0$res1)
#1.3Chi-squared test
library(carData)
library(car)
leveneTest(res1~location,data = data0)
#p=0.002942 **本案例组间因变量的方差齐
##1.4Linear relationship test
#绘制NBT和TotaltimeR的相关关系图
library(ggplot2)
ggplot(data=data0, aes(x=NBT, y=TotaltimeR,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$TotaltimeR,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$TotaltimeR,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$TotaltimeR,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$TotaltimeR,data0$NBT,method = "pearson",alternative = "two.sided")
###Check if there is a relationship between NBT and location###
NBTaov <- aov(NBT~location, data = data0)
summary(NBTaov)
##1.5 Parallelism test
library(mvtnorm)
library(survival)
library(TH.data)
library(MASS)
library(TH.data)
library(multcomp)
fit1<- aov(TotaltimeR ~ NBT *location, data = data0)
summary(fit1)#p=0.420 交互项不显著
##1.6、Statistical description
mean(data$TotaltimeR)
sd(data$TotaltimeR)
library(psych)
describeBy(data$TotaltimeR, group = data$location)
##1.7 Analysis of covariance
fit1_2=aov(TotaltimeR~NBT+location,data=data)
summary(fit1_2)


#2、response:sleep total time(calculated)
#2.1 Outlier determination
boxplot(data0$res2~data0$location,
        xlab = c("location"), ylab = expression("res2"))#bs:3,rs:3,ss:3
#2.2 Normality check
shapiro.test(data0$res2)#p-value = 1.319e-14>0.01
#2.3Chi-squared test
leveneTest(res2~location,data = data0)#p=0.1352 本案例组间因变量的方差齐
##2.4Linear relationship test
ggplot(data=data0, aes(x=NBT, y=TotaltimeC,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$TotaltimeC,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$TotaltimeC,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$TotaltimeC,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$TotaltimeC,data0$NBT,method = "pearson",alternative = "two.sided")
##2.5 Parallelism test
fit2<- aov(TotaltimeC ~ NBT *location, data = data0)
summary(fit2)
##2.6、Statistical description
mean(data$TotaltimeC)
sd(data$TotaltimeC)
describeBy(data$TotaltimeC, group = data$location)
##2.7Analysis of covariance
fit2_2=aov(TotaltimeC~NBT+location,data=data0)
summary(fit2_2)


##3、response:LongestPeriod
#3.1 Outlier determination
boxplot(data0$res3~data0$location,
        xlab = c("location"), ylab = expression("res3"))
#3.2 Normality test
shapiro.test(data0$res3)
#3.3 Chi-squared test
leveneTest(res3~location,data = data0)
##3.4Linear relationship test
ggplot(data=data0, aes(x=NBT, y=LongestPeriod,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$LongestPeriod,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$LongestPeriod,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$LongestPeriod,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$LongestPeriod,data0$NBT,method = "pearson",alternative = "two.sided")
##3.5 Parallelism test
fit3<- aov(LongestPeriod ~ NBT *location, data = data0)
summary(fit3)
##3.6、Statistical description
mean(data$LongestPeriod)
sd(data$LongestPeriod)
describeBy(data$LongestPeriod, group = data$location)
##3.7Analysis of covariance
fit3_2=aov(LongestPeriod~NBT+location,data=data0)
summary(fit3_2)#F=11.93 1.01e-05 ***
##3.8 Post hoc analysis
library(emmeans)
model=lm(LongestPeriod ~ factor(NBT)+location, data = data0)
modelemm=emmeans(model,"location")
pairs(modelemm)

##4.sleepDaytime
#4.1 Outlier determination
boxplot(data0$res4~data0$location,
        xlab = c("location"), ylab = expression("res4"))
#4.2 Normality test
shapiro.test(data0$res4)
#4.3Chi-squared test
leveneTest(res4~location,data = data0)#p=0.08114 *= 本案例组间因变量的方差不齐
##4.4 Linear relationship test
ggplot(data=data0, aes(x=NBT, y=sleepDaytime,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$sleepDaytime,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$sleepDaytime,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$sleepDaytime,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$sleepDaytime,data0$NBT,method = "pearson",alternative = "two.sided")
##4.5 Parallelism test
fit4<- aov(sleepDaytime ~ NBT *location, data = data0)
summary(fit4)
##4.6、Statistical description
mean(data$sleepDaytime)
sd(data$sleepDaytime)
describeBy(data$sleepDaytime, group = data$location)
##4.7post hoc
fit4_2=aov(sleepDaytime~NBT+location,data=data0)
summary(fit4_2)


##5、response:STNT
#5.1 Outlier determination
boxplot(data0$res5~data0$location,
        xlab = c("location"), ylab = expression("res5"))
##Remove outliers
quartiles <- quantile(data3$STNT, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data3$STNT)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
data3_no_outlier <- subset(data3,data3$STNT > Lower & data3$STNT < Upper)
data0_no_outlier<-rbind(data1,data2,data3_no_outlier)
#After checking for outliers
data0_no_outlier$location=factor(data0_no_outlier$location)
boxplot(data0_no_outlier$res5~data0_no_outlier$location,
        xlab = c("location"), ylab = expression("res5"))
#5.2 Normality check
shapiro.test(data0_no_outlier$res5)
#5.3Chi-squared test
leveneTest(res5~location,data = data0_no_outlier)
##5.4Linear relationship test
ggplot(data=data0_no_outlier, aes(x=NBT, y=STNT,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$STNT,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$STNT,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$STNT,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0_no_outlier$STNT,threedata0_no_outlier$NBT,method = "pearson",alternative = "two.sided")
##5.5 Parallelism test
fit5<- aov(STNT ~ NBT *location, data = data0_no_outlier)
summary(fit5)
##5.6、Statistical description
mean(data0_no_outlier$STNT)
sd(data0_no_outlier$STNT)
describeBy(data0_no_outlier$STNT, group = data0_no_outlier$location)
##5.7 ANCOVA
fit5_2=aov(STNT~NBT+location,data=data0_no_outlier)
summary(fit5_2)
##5.8事后分析
library(emmeans)
model=lm(STNT~ factor(NBT)+location, data = data0_no_outlier)
modelemm=emmeans(model,"location")
pairs(modelemm)

##6、response:Nightwake
#6.1 outlier analysis
boxplot(data0$res6~threedata0$location,
        xlab = c("location"), ylab = expression("res6"))
#6.2 Normality test
shapiro.test(data0$res6)
#6.3 Chi-squared test
leveneTest(res6~location,data = data0)
##6.4 Linear relationship test
ggplot(data=data0, aes(x=NBT, y=Nightwake,color=location))+geom_point()+stat_smooth(method="lm",se=TRUE)
cor.test(data1$Nightwake,data1$NBT,method = "pearson",alternative = "two.sided")
cor.test(data2$Nightwake,data2$NBT,method = "pearson",alternative = "two.sided")
cor.test(data3$Nightwake,data3$NBT,method = "pearson",alternative = "two.sided")
cor.test(data0$Nightwake,data0$NBT,method = "pearson",alternative = "two.sided")
##6.5 Parallelism test
fit6<- aov(Nightwake ~ NBT *location, data = data0)
summary(fit6)#p=0.0.107 交互项不显著
##6.6、Statistical description
mean(data$Nightwake)
sd(data$Nightwake)
describeBy(data$Nightwake, group = data$location)
##6.7Analysis of covariance
fit6_2=aov(Nightwake~NBT+location,data=data0)
summary(fit6_2)



####Maternal ancova code
library(readxl)
mother_sleep<- read_excel("dissertation/mother sleep.xlsx")
names(mother_sleep)
####3(2-4) month mother sleep outcome analysis (anova)
data=subset(mother_sleep,mother_sleep$babyAge==3|mother_sleep$babyAge==2|mother_sleep$babyAge==4)
names(data)
#1、SleeptotalC
#1.1 描述分析
#总体均值方差
mean(data$SleeptotalC)
sd(data$SleeptotalC)
#每组的平均数方差
library(psych)
describeBy(data$SleeptotalC, group = data$location)
#1.2 Anova
data$location=factor(data$location)
library(mvtnorm)
library(survival)
library(TH.data)
library(MASS)
library(TH.data)
library(multcomp)
fit1<-aov(SleeptotalC~factor(location), data = data)
summary(fit1)
#F:3.139 P:0.0451 *
#2.3 post hoc 
library(DescTools)
DunnettTest(x=data$SleeptotalC, g=data$location)
#SS-BS -0.5000862 -0.9933325 -0.006839868 0.0463 *  

#2.4 Check ANOVA assumptions: test validity
#2.4.1同方差性
library(car)
leveneTest(SleeptotalC ~location, data = data)
#F:0.4027 P:0.6689
#0.6689>0.1
#2.4.2正态性
# Extract the residuals
residuals <- residuals(object =fit1)
#W = 0.98785, p-value = 0.03767
# Run Shapiro-Wilk test
shapiro.test(x = residuals )
plot(fit1, 2)
#W = 0.98785, p-value = 0.03767

#2、Nightwake
#2.1 描述分析
#总体均值方差
mean(data$Nightwake)
sd(data$Nightwake)
#每组的平均数方差
describeBy(data$Nightwake, group = data$location)
#2.2 Anova
fit2<- aov(Nightwake ~ factor(location), data = data)
summary(fit2)
#F:4.1 P:0.0178 *
#2.3 post hoc 
library(DescTools)
DunnettTest(x=data$Nightwake, g=data$location)
#RS-BS -0.8907563 -1.746518 -0.0349951 0.0397 *  
#SS-BS -1.1516914 -2.263534 -0.0398491 0.0408 * 
#2.4 Check ANOVA assumptions: test validity
#2.4.1同方差性
library(car)
leveneTest(Nightwake ~location, data = data)
#F:1.7791  p:0.171
#0.171>0.1
#2.4.2正态性
# Extract the residuals
residuals <- residuals(object =fit2)
# Run Shapiro-Wilk test
shapiro.test(x = residuals )
#W = 0.85531, p-value = 2.476e-14
plot(fit2, 2)
#3、GSDS
#3.1 描述分析
#总体均值方差
mean(data$GSDS)
sd(data$GSDS)
#每组的平均数方差
describeBy(data$GSDS, group = data$location)
#3.2 Anova
fit3<-aov(GSDS~factor(location), data = data)
summary(fit3)
#F:0.138  0.871
#3.3 post hoc 
#library(DescTools)
#DunnettTest(x=data$Nightwake, g=data$location)
#3.4 Check ANOVA assumptions: test validity
#3.4.1同方差性
library(car)
leveneTest(GSDS ~factor(location), data = data)
#F:0.3266 p:0.7217
#3.4.2正态性
# Extract the residuals
residuals <- residuals(object =fit3)
# Run Shapiro-Wilk test
shapiro.test(x = residuals )#W = 0.93213, p-value = 3.183e-11
plot(fit3, 2)

#4、day function
#4.1 描述分析
#总体均值方差
mean(data$dayfunction)
sd(data$dayfunction)
#每组的平均数方差
describeBy(data$dayfunction, group = data$location)
#4.2 Anova
fit4<- aov(dayfunction ~factor(location), data = data)
summary(fit4)
#F:0.01 P：0.99
#4.3 post hoc 
#library(DescTools)
#DunnettTest(x=data$Nightwake, g=data$location)
#4.4 Check ANOVA assumptions: test validity
#4.4.1同方差性
library(car)
leveneTest(dayfunction ~location, data = data)
#F:0.4878 P:0.6146
#4.4.2正态性
# Extract the residuals
residuals <- residuals(object =fit4)
# Run Shapiro-Wilk test
shapiro.test(x = residuals )#W = 0.97018, p-value = 5.593e-05
plot(fit4, 2)

#5、sleeptimedD
#delete null value
data5=subset(data,data$SleeptotalD>0)
#5.1 描述分析
#总体均值方差
mean(data5$SleeptotalD)
sd(data5$SleeptotalD)
#每组的平均数方差
describeBy(data5$SleeptotalD, group = data5$location)
#5.2ANOVA
fit5<- aov(SleeptotalD ~factor(location), data = data5)
summary(fit5)
#F:4.918 P:0.00968 **
#5.3 post hoc
library(DescTools)
DunnettTest(x=data5$SleeptotalD, g=data5$location)
TukeyHSD(fit5)
#SS-BS 1.1365854  0.19415034 2.079020 0.0139755
#5.4 Check ANOVA assumptions: test validity
#5.4.1同方差性
library(car)
leveneTest(SleeptotalD ~location, data = data5)
#F:0.4821 p:0.6193
#5.4.2正态性
# Extract the residuals
residuals <- residuals(object =fit5)
# Run Shapiro-Wilk test
shapiro.test(x = residuals )
#W = 0.97538, p-value = 0.1115
plot(fit5, 2)

#6、Longest period
#5.1 描述分析
#总体均值方差
mean(data5$LongestPeroid)
sd(data5$LongestPeroid)
#每组的平均数方差
describeBy(data5$LongestPeroid,group= data5$location)
#6.2ANOVA
fit6<- aov(log(LongestPeroid)~factor(location), data = data5)
summary(fit6)
#F:3.654 P:0.0303 *
#6.3 post hoc
library(DescTools)
DunnettTest(x=log(data5$LongestPeroid), g=data5$location)
#SS-BS 0.2677341  0.003435726 0.5320325 0.0465 *  

#6.4 Check ANOVA assumptions: test validity
#6.4.1同方差性
library(car)
leveneTest(LongestPeroid ~location, data = data5)
#F:2.2364 p:0.1135
#6.4.2正态性
# Extract the residuals
residuals <- residuals(object =fit6)
# Run Shapiro-Wilk test
shapiro.test(x = residuals )#log:0.0224 no_log:1.874e-07
plot(fit6, 2)
#W = 0.96475, p-value = 0.0224

####6(5-7) month mother sleep outcome analysis (anova)
data=subset(mother_sleep,mother_sleep$babyAge==6|mother_sleep$babyAge==5|mother_sleep$babyAge==7)
names(data)
data$location=factor(data$location)
#1、SleeptotalC
#1.1 Check ANOVA assumptions: test validity
#1.1.1 Homoscedasticity
library(car)
leveneTest(SleeptotalC ~location, data = data)
#F:0.8222 p=0.4405
#1.1.2 Normality test
# Extract the residuals
lm1=lm(SleeptotalC ~factor(location), data = data)
residuals <- residuals(object =lm1)
# Run Shapiro-Wilk test
plot(lm1, 2)
shapiro.test(x = residuals)
#W = 0.94086, p-value = 2.645e-10
#删除离群值
quartiles <- quantile(data$SleeptotalC, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$SleeptotalC)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
data1_no_outlier <- subset(data,data$SleeptotalC > Lower & data$SleeptotalC < Upper)
#重新测试
lm1=lm(SleeptotalC ~factor(location), data = data1_no_outlier)
residuals <- residuals(object =lm1)
plot(lm1, 2)
shapiro.test(x = residuals)
#W = 0.98434, p-value = 0.003552
#1.2 描述分析
#总体均值方差
mean(data$SleeptotalC)
sd(data$SleeptotalC)
#每组的平均数方差
library(psych)
describeBy(data$SleeptotalC, group = data$location)
#1.3 Anova
library(mvtnorm)
library(survival)
library(TH.data)
library(MASS)
library(TH.data)
library(multcomp)
fit1<-aov(SleeptotalC~factor(location), data = data)
summary(fit1)
#F:0.891  p:0.411
#2、Nightwake
#2.1 Check ANOVA assumptions: test validity
#2.1.1 Homoscedasticity
library(car)
leveneTest(Nightwake ~location, data = data)
#F: 4.1269 p=0.0171 *
#2.1.2 Normality test
# Extract the residuals
lm2=lm(Nightwake ~factor(location), data = data)
residuals <- residuals(object =lm2)
# Run Shapiro-Wilk test
plot(lm2, 2)
shapiro.test(x = residuals)
#W = 0.81812, p-value < 2.2e-16
#调整正态分布
lm2=lm(log(Nightwake+0.00001)~factor(location), data = data)
residuals <- residuals(object =lm2)
plot(lm2, 2)
shapiro.test(x = residuals)
#W = 0.5011, p-value < 2.2e-16
#2.2 描述分析
#总体均值方差
mean(data$Nightwake)
sd(data$Nightwake)
#每组的平均数方差
describeBy(data$Nightwake, group = data$location)
#2.3 Anova
fit2<- aov(Nightwake ~ factor(location), data = data)
summary(fit2)
#F:4.36 p=0.0136 *
#2.4 post hoc 
TukeyHSD(fit2)
#SS-BS -0.9777550 -1.828282 -0.1272279 0.0195656

#3、GSDS
#3.1 Check ANOVA assumptions: test validity
#3.1.1 Homoscedasticity
library(car)
leveneTest(GSDS ~factor(location), data = data)
#F:2.2647 0.1057
#3.1.2 Normality test
# Extract the residuals
lm3=lm(GSDS ~factor(location), data = data)
residuals <- residuals(object =lm3)
# Run Shapiro-Wilk test
plot(lm3, 2)
shapiro.test(x = residuals)
#W = 0.94008, p-value = 1.838e-09
#3.2 描述分析
#总体均值方差
mean(data$GSDS)
sd(data$GSDS)
#每组的平均数方差
describeBy(data$GSDS, group = data$location)
#3.3 Anova
fit3<-aov(GSDS~factor(location), data = data)
summary(fit3)
#F:0.921  0.399
#4、day function
#4.1 Check ANOVA assumptions: test validity
#4.1.1 Homoscedasticity
library(car)
leveneTest(dayfunction ~location, data = data)
#F:0.4307 p:0.6505
#4.1.2 Normality test
# Extract the residuals
lm4=lm(dayfunction ~factor(location), data = data)
residuals <- residuals(object =lm4)
# Run Shapiro-Wilk test
plot(lm4, 2)
shapiro.test(x = residuals)
#W = 0.98406, p-value = 0.002592

#删除离群值
quartiles <- quantile(data$dayfunction, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$dayfunction)
Lower <- quartiles[1] - 0.5*IQR
Upper <- quartiles[2] + 0.5*IQR 
data4_no_outlier <- subset(data,data$dayfunction > Lower & data$dayfunction < Upper)
#重新测试
lm4=lm(dayfunction ~factor(location), data = data)
residuals <- residuals(object =lm4)
plot(lm4, 2)
shapiro.test(x = residuals)
#W = 0.98406, p-value = 0.002592
#4.2 描述分析
#总体均值方差
mean(data$dayfunction)
sd(data$dayfunction)
#每组的平均数方差
describeBy(data$dayfunction, group = data$location)
#4.3 Anova
fit4<- aov(dayfunction~factor(location), data = data4_no_outlier)
summary(fit4)
#F=0.306,p=0.737

#5.1 SleeptotalD
#Delete Null Value
data5=subset(data,data$SleeptotalD>0)
#5.1 Check ANOVA assumptions: test validity
#5.1.1 Homoscedasticity
library(car)
leveneTest(SleeptotalD ~location, data = data5)
#F=0.7035 p=0.4967
#5.1.2 Normality test
# Extract the residuals
lm5=lm(SleeptotalD~factor(location), data = data5)
residuals <- residuals(object =lm5)
# Run Shapiro-Wilk test
plot(lm5, 2)#有些离群点
shapiro.test(x = residuals)
#W = 0.94096, p-value = 1.845e-05

#删除离群点
quartiles <- quantile(data5$SleeptotalD, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data5$SleeptotalD)
Lower <- quartiles[1] - 1.65*IQR
Upper <- quartiles[2] + 1.65*IQR 
data5_no_outlier <- subset(data5,data5$SleeptotalD > Lower & data5$SleeptotalD < Upper)
#重新测试
lm5=lm(SleeptotalD~factor(location), data = data5_no_outlier)
residuals <- residuals(object =lm5)
plot(lm5, 2)
shapiro.test(x = residuals)
#W = 0.98869, p-value = 0.3716
leveneTest(SleeptotalD ~location, data = data5_no_outlier)
#F:1.3832 p:0.2546
#5.2 描述分析
#总体均值方差
mean(data5_no_outlier$SleeptotalD)
sd(data5_no_outlier$SleeptotalD)
#每组的平均数方差
library(psych)
describeBy(data5_no_outlier$SleeptotalD, group = data5_no_outlier$location)
#5.3ANOVA
fit5<- aov(SleeptotalD~location, data = data5_no_outlier)
summary(fit5)#F:0.749  p:0.475

#6、Longest Period
#6.1 Check ANOVA assumptions: test validity
#6.1.1 Homoscedasticity
library(car)
leveneTest(LongestPeroid ~location, data = data5)
#F:1.4759 p:0.2324
#6.1.2 Normality test
# Extract the residuals
lm6=lm(LongestPeroid~factor(location), data = data5)
residuals <- residuals(object =lm6)
# Run Shapiro-Wilk test
plot(lm6, 2)#很多离群点
shapiro.test(x = residuals)
#W = 0.83374, p-value = 7.37e-11
#删除离群点
quartiles <- quantile(data5$LongestPeroid, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data5$LongestPeroid)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
data6_no_outlier <- subset(data5,data5$LongestPeroid > Lower & data5$LongestPeroid < Upper)
#重新测试
lm6=lm(LongestPeroid~factor(location), data = data6_no_outlier)
residuals <- residuals(object =lm6)
plot(lm6, 2)
shapiro.test(x = residuals)
#W = 0.97234, p-value = 0.01356
leveneTest(LongestPeroid ~location, data = data6_no_outlier)
#F:0.8178 p:0.4439
#6.2 描述分析
#总体均值方差
mean(data5$LongestPeroid)
sd(data5$LongestPeroid)
#每组的平均数方差
describeBy(data5$LongestPeroid, group = data5$location)
#6.3ANOVA
fit6<- aov(LongestPeroid~factor(location), data = data5)
summary(fit6)
#F:0.484  p:0.618

####9(8-10) month mother sleep outcome analysis (anova)
data=subset(mother_sleep,mother_sleep$babyAge==9|mother_sleep$babyAge==8|mother_sleep$babyAge==10)
data$location=factor(data$location)
#1、SleeptotalC
#1.1 Check ANOVA assumptions: test validity
#1.1.1 Homoscedasticity
library(car)
leveneTest(SleeptotalC ~location, data = data)
#F:5.8438 p=0.003255 **
#1.1.2 Normality test
# Extract the residuals
lm1=lm(SleeptotalC ~factor(location), data = data)
residuals <- residuals(object =lm1)
# Run Shapiro-Wilk test
plot(lm1, 2)
shapiro.test(x = residuals)
#W = 0.90447, p-value = 1.448e-12

#删除离群值
quartiles <- quantile(data$SleeptotalC, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data$SleeptotalC)
Lower <- quartiles[1] - 1.65*IQR
Upper <- quartiles[2] + 1.65*IQR 
data1_no_outlier <- subset(data,data$SleeptotalC > Lower & data$SleeptotalC < Upper)
#重新测试
lm1=lm(SleeptotalC ~factor(location), data = data1_no_outlier)
residuals <- residuals(object =lm1)
plot(lm1, 2)
shapiro.test(x = residuals)
#W = 0.99131, p-value = 0.08976
#2.2 描述分析
#总体均值方差
mean(data$SleeptotalC)
sd(data$SleeptotalC)
#每组的平均数方差
library(psych)
describeBy(data$SleeptotalC, group = data$location)
#1.3 Anova
library(mvtnorm)
library(survival)
library(TH.data)
library(MASS)
library(TH.data)
library(multcomp)
fit1<-aov(SleeptotalC~factor(location), data = data)
summary(fit1)
#F:2.715 p:0.0679 
#2、Nightwake
#2.1 Check ANOVA assumptions: test validity
#2.1.1 Homoscedasticity
library(car)
leveneTest(Nightwake ~location, data = data)
#F:3.7307 p=0.02515 *
#2.1.2 Normality test
# Extract the residuals
lm2=lm(Nightwake ~factor(location), data = data)
residuals <- residuals(object =lm2)
# Run Shapiro-Wilk test
plot(lm2, 2)
shapiro.test(x = residuals)
#W = 0.84709, p-value = 3.11e-16
#重新测试
lm2=lm(log(Nightwake+0.01) ~factor(location), data = data)
residuals <- residuals(object =lm2)
plot(lm2, 2)
shapiro.test(x = residuals)
#W = 0.62588, p-value < 2.2e-16
#2.2 描述分析
#总体均值方差
mean(data$Nightwake)
sd(data$Nightwake)
#每组的平均数方差
describeBy(data$Nightwake, group = data$location)
#2.3 Anova
fit2<- aov(log(Nightwake+0.1) ~ factor(location), data = data)
summary(fit2)
#F:4.446 p=0.0125 *
#2.4 post hoc 
TukeyHSD(fit2)
#RS-BS -0.6936736 -1.24181813 -0.1455291 0.0087231

#3、GSDS
#3.1 Check ANOVA assumptions: test validity
#3.1.1 Homoscedasticity
library(car)
leveneTest(Minsomnia ~factor(location), data = data)
#F:3.9309 p=0.02069 *
#3.1.2 Normality test
# Extract the residuals
lm3=lm(Minsomnia ~factor(location), data = data)
residuals <- residuals(object =lm3)
# Run Shapiro-Wilk test
plot(lm3, 2)
shapiro.test(x = residuals)
#W = 0.94444, p-value = 5.548e-09
#3.2 描述分析
#总体均值方差
mean(data$GSDS)
sd(data$GSDS)
#每组的平均数方差
describeBy(data$GSDS, group = data$location)
#3.3 Anova
fit3<-aov(GSDS~factor(location), data = data)
summary(fit3)
#F:1.028  0.359

#4、day function
#4.1 Check ANOVA assumptions: test validity
#4.1.1 Homoscedasticity
library(car)
leveneTest(dayfunction ~location, data = data)
#F:0.8153 p=0.4435
#4.1.2 Normality test
# Extract the residuals
lm4=lm(dayfunction ~factor(location), data = data)
residuals <- residuals(object =lm4)
# Run Shapiro-Wilk test
plot(lm4, 2)
shapiro.test(x = residuals)
#W = 0.97734, p-value = 0.0001521
#4.2 描述分析
#总体均值方差
mean(data$dayfunction)
sd(data$dayfunction)
#每组的平均数方差
describeBy(data$dayfunction, group = data$location)
#4.3 Anova
fit4<- aov(dayfunction ~factor(location), data = data)
summary(fit4)
#F=2.419 p=0.0908
#5.1 SleeptotalD
#Delete Null Value
data5=subset(data,data$SleeptotalD>0)
#5.1 Check ANOVA assumptions: test validity
#5.1.1 Homoscedasticity
library(car)
leveneTest(SleeptotalD ~location, data = data5)
#F:0.5753 P:0.5642
#5.1.2 Normality test
# Extract the residuals
lm5=lm(SleeptotalD~factor(location), data = data5)
residuals <- residuals(object =lm5)
# Run Shapiro-Wilk test
plot(lm5, 2)#有很多离群点
shapiro.test(x = residuals)
#W = 0.92417, p-value = 5.917e-06
#删除离群点
quartiles <- quantile(data5$SleeptotalD, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data5$SleeptotalD)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
data5_no_outlier <- subset(data5,data5$SleeptotalD > Lower & data5$SleeptotalD < Upper)
#重新测试
lm5=lm(SleeptotalD~factor(location), data = data5_no_outlier)
residuals <- residuals(object =lm5)
plot(lm5, 2)
shapiro.test(x = residuals)#W = 0.98552, p-value = 0.2336
#5.2 描述分析
#总体均值方差
mean(data5_no_outlier$SleeptotalD)
sd(data5_no_outlier$SleeptotalD)
#每组的平均数方差
library(psych)
describeBy(data5$SleeptotalD, group = data5$location)
#5.3ANOVA
fit5<- aov(SleeptotalD~factor(location), data = data5_no_outlier)
summary(fit5)
#F:4.208 p=0.0174 *
#5.4 post hoc
TukeyHSD(fit5)
#SS-RS  1.3089286  0.2269142 2.39094296 0.0134007

#6、Longest Period
#6.1 Check ANOVA assumptions: test validity
#6.1.1 Homoscedasticity
library(car)
leveneTest(LongestPeroid ~location, data = data5)
#F:1.1355 p:0.3249

#6.1.2 Normality test
# Extract the residuals
lm6=lm(LongestPeroid~factor(location), data = data5)
residuals <- residuals(object =lm6)
# Run Shapiro-Wilk test
plot(lm6, 2)#很多离群点
shapiro.test(x = residuals)
#W = 0.79105, p-value = 1.492e-11

#删除离群点
quartiles <- quantile(data5$LongestPeroid, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(data5$LongestPeroid)
Lower <- quartiles[1] - 1.75*IQR
Upper <- quartiles[2] + 1.75*IQR 
data6_no_outlier <- subset(data5,data5$LongestPeroid > Lower & data5$LongestPeroid < Upper)
#重新测试
lm6=lm(LongestPeroid~factor(location), data = data6_no_outlier)
residuals <- residuals(object =lm6)
plot(lm6, 2)
shapiro.test(x = residuals)
#W = 0.9842, p-value = 0.2652

#6.2 描述分析
#总体均值方差
mean(data6_no_outlier$LongestPeroid)
sd(data6_no_outlier$LongestPeroid)
#每组的平均数方差
describeBy(data6_no_outlier$LongestPeroid, group = data6_no_outlier$location)
#6.3ANOVA
fit6<- aov(LongestPeroid~factor(location), data = data6_no_outlier)
summary(fit6)#F: 5.173 p:0.0073 **
#6.4 post hoc
TukeyHSD(fit6)
#SS-BS  0.4026163  0.06234391 0.7428886 0.0160681


