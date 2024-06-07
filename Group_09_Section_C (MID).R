install.packages("ggplot2")
install.packages("naniar")
library(ggplot2)
library(naniar)

data<-read.csv("D:/midproject.csv",header=TRUE,sep=",")
data
summary(data)
str(data)
sum(is.na(data))
gg_miss_upset(data)


class(data$Age)
any(!as.numeric(data$Age))
data<-data[!is.na(data$Age),]
data
boxplot(data$Age,main="Boxplot of Age",ylab="Values")
boxplot.stats(data$Age)$out
q1<-quantile(data$Age,0.25)
q3<-quantile(data$Age,0.75)
iqr<-q3-q1
threshold<-1.5
outliersAge<-data$Age<(q1-threshold*iqr)|data$Age>(q3+threshold*iqr)
mean_age<-round(mean(data$Age,na.rm=TRUE),digits=0)
data$Age[outliersAge]<-mean_age
data

unique(data$Infection)
data$Infection[data$Infection=="yes"]<- 1
data$Infection[data$Infection=="marginal"]<- 2
data$Infection[data$Infection=="no"]<- 3
data$Infection[!(data$Infection%in%c(1,2,3))]<-NA
data$Infection<-as.numeric(data$Infection)
sum(is.na(data$Infection))
names(sort(table(data$Infection),decreasing=TRUE))[1]
data$Infection[is.na(data$Infection)]<-names(sort(table(data$Infection),decreasing=TRUE))[1]
data$Infection<-as.character(data$Infection)
data$Infection[data$Infection=="1"]<- "yes"
data$Infection[data$Infection=="2"]<- "marginal"
data$Infection[data$Infection=="3"]<- "no"
data

class(data$Smoking)
unique(data$Smoking)
sum(is.na(data$Smoking))
data$Smoking<-as.integer(data$Smoking)
smoking_mode<-names(sort(table(data$Smoking),decreasing=TRUE))[1]
smoking_mode
data$Smoking[is.na(data$Smoking)]<-smoking_mode
sum(is.na(data$Smoking))
data$Smoking<-as.character(data$Smoking)
data$Smoking[data$Smoking=="1"]<-"yes"
data$Smoking[data$Smoking=="2"]<-"sometimes"
data$Smoking[data$Smoking=="3"]<-"no"
data

class(data$SystolicBP)
sum(is.na(data$SystolicBP))
boxplot(data$SystolicBP,main="Boxplot of systolicBP",ylab="values")
any(is.na(as.numeric(data$SystolicBP)))


sum(is.na(data$DiastolicBP))
mean_diasolic<-round(mean(data$DiastolicBP,na.rm=TRUE),digits=0)
mean_diastolic
data$DiastolicBP[is.na(data$DiastolicBP)]<-mean_diastolic
any(is.na(as.numeric(data$DiastolicBP)))
boxplot(data$DiastolicBP,main="Boxplot of DiastolicBP",ylab="Values")

class(data$BS)
sum(is.na(data$BS))
any(is.na(as.numeric(data$BS)))
range(data$BS)
boxplot(data$BS,main="Boxplot of BS",ylab="Values")
min_BS<-min(data$BS)
max_BS<-max(data$BS)
custom_min<-0.1
custom_max<-0.9
data$BS<-0.1+(0.9-0.1)*(data$BS-min_BS)/(max_BS-min_BS)
data$BS<-round(data$BS,3)
data


class(data$BodyTemp)
sum(is.na(data$BodyTemp))
boxplot(data$BodyTemp,main="Boxplot of BodyTemp",ylab="Values")
mean_bodytemp<-round(mean(data$BodyTemp[data$BodyTemp>=0]))
mean_bodytemp
data$BodyTemp[data$BodyTemp<0]<-mean_bodytemp
boxplot(data$BodyTemp)


class(data$HeartRate)
sum(is.na(data$HeartRate))
any(is.na(as.numeric(data$HeartRate)))
boxplot(data$HeartRate,main="Boxplot of HeartRate",ylab="Values")

class(data$RiskLevel)
unique(data$RiskLevel)
sum(is.na(data$RiskLevel))



ggplot(data,aes(x=Smoking))+geom_bar()
ggplot(data,aes(x=Infection))+geom_bar()
ggplot(data,aes(x=RiskLevel))+geom_bar()



for(col in names(data)[sapply(data,is.numeric)] ){
  hist(data[[col]], main=paste(col,"Distribution"),xlab=col)
}

