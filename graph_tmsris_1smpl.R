
# data : 4 column data('House', 'Date', 'Hour', 'W')
# hname : random하게 선택된 1개 가구 
hname<-sample(unique(data$House),1)
# charge_hour : data + 시간단위 전력 요금 데이터
rhouse<-subset(charge_hour,House == hname)
rhouse$Day<-factor(rhouse$Day,
                   levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

############################################### Graph ###########################################
par(mfrow=c(2,2))

### Graph 1 : 월별 총 전력 사용량
sum_m<-rhouse %>% group_by(Month) %>% summarise(usage=sum(KW,na.rm=TRUE)) #Monthly/kw
plot(usage~Month,data=sum_m,type="b",main="Monthly power usage graph/per KW",
     ylab="Electricity Load",ylim=c(0,600))
axis(side=1,at=seq(1,12,by=1))

### Graph 2 : 일별 총 전력 사용량
sum_d<-rhouse %>% group_by(Month,Date2,Day) %>% summarise(KW=sum(KW,na.rm=TRUE))
sum_d1<-sum_d %>% group_by(Date2) %>% summarise(usage=mean(KW,na.rm=TRUE)) #daily/W
plot(usage~Date2,data=sum_d1,type="b",main="Daily power usage graph/per KW",
     xlab="Date",ylab="Electricity Load",ylim=c(0,15))
axis(side=1,at=seq(1,31,by=1))

### Graph 3 : 요일별 평균 전력 사용량
sum_d2<-sum_d %>% group_by(Day) %>% summarise(usage=mean(KW,na.rm=TRUE)) 
barplot(usage~Day,data=sum_d2,main="Power usage graph by day of the week/per KW",
        ylab="Electricity Load",ylim=c(0,15)) #day of the week

### Graph 4 : 시간별 평균 전력 사용량
sum_h<-rhouse %>% group_by(Hour) %>% summarise(usage=mean(W,na.rm=TRUE)) #Hourly/W
plot(usage~Hour,data=sum_h,type="b",main="Hourly power usage graph/per W",ylim=c(0,800))
axis(side=1,at=seq(0,23,by=1))

### Graph 5 : 요일별, 시간별 평균 전력 사용량
da = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
color = c("Deep sky blue", "light green", "yellow", "Gold", "Dark orange", "Light coral", "orange red")

par(mfrow=c(1,1))
sum_h2<-rhouse %>% group_by(Hour,Day) %>% summarise(usage=mean(W,na.rm=TRUE))

for(i in 1:7){
  a<-sum_h2 %>% filter(Day==da[i])
  # Monday 먼저 그리기
  if(i==1){
    plot(a$Hour, a$usage, type="b", col=color[i],xlab="Hour(0~23)",ylab="Electricity Load",main="Hourly power usage graph/per W",ylim=c(0,800))
  }
  # 다른 요일 그리기
  else{
    par(new=TRUE)
    plot(a$Hour, a$usage, type="b",xlab="", ylab="", xaxt="n", yaxt="n", col=color[i],ylim=c(0,800))
    axis(side=1,at=seq(0,23,by=1))
  }
}
legend("bottomright",c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
       col = c("Deep sky blue", "light green", "yellow", "Gold", "Dark orange", "Light coral", "orange red"),
       lty = c(1,1,1,1,1,1,1), cex=0.5)
