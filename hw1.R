library(tidyverse)
library(mosaic)
ABIA <- read.csv("C:/Users/yaint/OneDrive/desktop/data mining/exercises/hw1/hw1/ABIA.csv")

ABIA2 = ABIA
USairports <- subset(airports,(airports$iso_country =="US"))

# Q1: What is the best time of day to fly to minimize delays?
# Calculate delaying departure time:
ABIA2$TWindow = floor(ABIA2$DepTime/100)
ABIA2$CRSTWindow_D = floor(ABIA2$CRSDepTime/100)
ABIA2$CRSTWindow_A = floor(ABIA2$CRSArrTime/100)
ABIA2$INorOUT[ABIA2$Dest == "AUS"] = "Arrival"
ABIA2$INorOUT[ABIA2$Origin == "AUS"] = "Departure"
ABIA2_fly <- subset(ABIA2,(ABIA2$Cancelled == "0"))
ABIA2_fly <- subset(ABIA2_fly,!(ABIA2_fly$Diverted == 1))
ABIA2_cancel <- subset(ABIA2,(ABIA2$Cancelled == "1"))

# plot the summ for all the airline companies 
ABIA2_CRSsumm_D_total <- ABIA2_fly %>%
  group_by(CRSTWindow_D,INorOUT) %>%
  summarise(DepDelay_mean = mean(DepDelay))

ABIA2_CRSsumm_A_total <- ABIA2_fly %>%
  group_by(CRSTWindow_A,INorOUT) %>%
  summarise(ArrDelay_mean = mean(ArrDelay))

p1 = ggplot(data = subset(ABIA2_CRSsumm_D_total,(ABIA2_CRSsumm_D_total$INorOUT == "Departure")))+
  geom_bar(aes(x = CRSTWindow_D, y = DepDelay_mean),stat='identity',position='dodge')+
  labs(title = "Scheduled Time vs Average Departure Delay", x = "Scheduled Time", y = "Departure Delay")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
p1
p2 = ggplot(data = subset(ABIA2_CRSsumm_A_total,(ABIA2_CRSsumm_A_total$INorOUT == "Arrival")))+
  geom_bar(aes(x = CRSTWindow_A, y = ArrDelay_mean),stat='identity',position='dodge')+
  labs(title = "Scheduled Time vs Average Arrival Delay", x = "Scheduled Time", y = "Arrival Delay")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
p2

# Q2: When is the best month to fly to minimize delays?
# plot the summ for all the airline companies 
ABIA2_summ_Month <- ABIA2_fly %>%
  group_by(Month,INorOUT) %>%
  summarise(DepDelay_mean = mean(DepDelay),ArrDelay_mean = mean(ArrDelay))

p3 = ggplot(data = subset(ABIA2_summ_Month,(ABIA2_summ_Month$INorOUT == "Departure")))+
  geom_bar(aes(x = Month, y = DepDelay_mean),stat='identity',position='dodge')+
  labs(title = "Month vs Average Departure Delay", x = "Month", y = "Departure Delay")+
  theme_bw()+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"))+
  theme(plot.title = element_text(hjust = 0.5))
p3
p4 = ggplot(data = subset(ABIA2_summ_Month,(ABIA2_summ_Month$INorOUT == "Arrival")))+
  geom_bar(aes(x = Month, y = ArrDelay_mean),stat='identity',position='dodge')+
  labs(title = "Month vs Average Arrival Delay", x = "Month", y = "Arrival Delay")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May", "6" = "Jun", "7" = "Jul", "8" = "Aug", "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
p4
mean(DepDelay~Month, data=ABIA2_fly)
#Q3. We can check it by days(Mon~Sun)


p5 = ggplot(ABIA, aes(Month))+geom_bar()
p5
count(ABIA2$Month==12)
# How can I split it into Arrival vs Departure? Why are there errors whenever I try to put in JAN to DEC?
# Why are there so many delays in December although there are not much traffic?


#Q4. Which airplane company makes delays most often?
#Delays by Aircraft companies 
favstats(DepDelay~UniqueCarrier, data=ABIA2_fly)
bwplot(DepDelay~UniqueCarrier, data=ABIA2_fly)
p6= bwplot(DepDelay~UniqueCarrier, data=ABIA2_fly, ylim=c(-20,30))
p6
# seems like EV is the worst
AvgDepDelay_Unique=ABIA2_fly%>% group_by(UniqueCarrier) %>% summarize(AvgDepDelay= mean(DepDelay) )

mean.ABIA2_fly<-as.data.frame(tapply(ABIA2_fly$DepDelay, ABIA2_fly$UniqueCarrier, mean))
mean.ABIA2_fly$UniqueCarrier<-rownames(mean.ABIA2_fly)
names(mean.ABIA2_fly)<-c("DepDelay","UniqueCarrier")
mean.ABIA2_fly
ggplot(mean.ABIA2_fly, aes(UniqueCarrier, DepDelay))+geom_bar(stat="identity")
ggplot(mean.ABIA2_fly, aes(reorder(UniqueCarrier, -DepDelay, sum), DepDelay))+geom_bar(stat="identity")


favstats(ArrDelay~UniqueCarrier, data=ABIA2_fly)
p7= bwplot(ArrDelay~UniqueCarrier, data=ABIA2_fly, ylim=c(-20,30))
p7

# seems like OH is the worst

# Do they stay longer at the airport when they arrive earlier than scheduled time?
plot(TaxiIn~ArrDelay, data=ABIA, xlim=c(-100,0))

EarlyArrival= subset(ABIA, ABIA$ArrDelay<0)
EarlyArrival
lm(TaxiIn~EarlyArrival, data=ABIA)
# How can I show it? Anyway my assumption was wrong!
