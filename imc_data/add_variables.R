########################### 시계열 변수 생성 ##############################

library(xts)
library(reshape2)
library(tidyverse)

##22656 = 944*24 ##22776 = 949*24
time_index=seq.POSIXt(ISOdate(2017, 05, 27, hour = 0, min = 0, tz = "GMT"),by="hour" , length.out=22776) 

# DB_code, Apt 변수 사용 안함
data_s<-data[,-c(1,2)]
data_s<-spread(data_s,key="House",value="KW")
data_s$Hour<-as.numeric(data_s$Hour)

year<-month<-day<-index<-c() 
for(i in 1:nrow(data_s)) {
  year[i]<-substr(data_s$Date[i],1,4) ; month[i]<-substr(data_s$Date[i],5,6) ; day[i]<-substr(data_s$Date[i],7,8)
  # year<-as.numeric(year) ; month<-as.numeric(month) ; day<-as.numeric(day)
  index[i]<-ISOdate(year[i], month[i], day[i], hour = data_s$Hour[i], tz = "GMT") 
}

index<-as.character(index) ; time_index_s<-as.character(time_index) 
data_s$iso<-index 
time<-data.frame(time_index_s,X2=rnorm(22776))

test<-left_join(time,data_s,by=c("time_index_s"="iso"))
test2<-test[which(!duplicated(test$time_index_s)),]
test2<-test2[,-c(1:4)]

full_data_with_time=xts(test2, time_index) 
full_data_with_time<-as.data.frame(full_data_with_time)

save(full_data_with_time,file="C:\\Users\\이지선\\Desktop\\에기평\\샘플자료_new\\imcnew.RData")

### full_data_time 시계열 변수 추가  

full_data_with_time$day<-weekdays(as.Date(substr(rownames(full_data_with_time),1,10)))
full_data_with_time$hour<-substr(rownames(full_data_with_time),12,13)
full_data_with_time$dh<-paste(full_data_with_time$day,full_data_with_time$hour,sep="")

