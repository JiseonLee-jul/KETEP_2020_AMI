### plot 1 : one sample plot during whole timepoints
index = sample(c(1:4051) ,1)  
plot(full_data_with_time[,index], main=paste('Sample', index))


###### data for plot
data_plot<-aggregate(full_data_with_time[,-c(4052:4054)],by=list(full_data_with_time$dh),FUN=mean,na.rm=T)
data_plot$day<-substr(data_plot$Group.1,1,3)
data_plot$hour<-substr(data_plot$Group.1,4,5)
temp<-apply(data_plot[,-c(1,4052:4054)],1,mean)

data_plot2<-data.frame(day=data_plot$day,hour=data_plot$hour,value=temp)
data_plot2$hour<-as.integer(data_plot2$hour)

##### plot 2 : 요일별 시간당 전력사용량 평균
da<-c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일")
color<-c("black", "green", "yellow", "blue", "orange", "pink", "red")

for(i in 1:7){
  a<-data_plot2 %>% filter(day==da[i])
  if(i==1){
    plot(a$hour, a$value, type="l", col=color[i],xlab="Hour(0~23)",ylab="Electricity Load",main="B단지(2629가구) 요일별 전기사용량 평균")
    points(a$hour,a$value,pch=i,col=color[i])
  }
  else{
    par(new = TRUE)
    plot(a$hour, a$value, type="l",xlab="", ylab="", xaxt="n", yaxt="n", col=color[i])
    points(a$hour,a$value,pch=i,col=color[i])
  }
}
legend("topleft",c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일"),
       col=c("black", "green", "yellow", "blue", "orange", "pink", "red"),
       lty=c(1,1,1,1,1,1,1),pch=c(1:7),cex=0.8)


##### plot 3 : 평일 시간당 전력사용량 평균
data_weekday<-subset(data_plot2,day=="월요일"|day=="화요일"|day=="수요일"|day=="목요일"|day=="금요일")
data_weekday2<-aggregate(data_weekday,by=list(data_weekday$hour),FUN=mean,na.rm=T)

plot(data_weekday2$hour,data_weekday2$value,type="l",xlab="Hour(0~23)",ylab="Electricity Load",main="B단지(2629가구) 평일 전기사용량 평균")
points(data_weekday2$hour,data_weekday2$value)

##### plot 4 : 주말 시간당 전력사용량 평균
data_weekend<-subset(data_plot2,day=="토요일"|day=="일요일")
data_weekend
data_weekend2<-aggregate(data_weekend,by=list(data_weekend$hour),FUN=mean,na.rm=T)

plot(data_weekend2$hour,data_weekend2$value,type="l",xlab="Hour(0~23)",ylab="Electricity Load",main="B단지(2629가구) 주말 전기사용량 평균")
points(data_weekend2$hour,data_weekend2$value)


##### plot 5 : sample가구 시간별 전력 사용량

par(mfrow=c(1,2))

c1<-aggregate(data_plot[,50],by=list(data_plot$hour),FUN=mean,na.rm=T)
plot(c1$Group.1,c1$x,type="o",xlab="Hour(0~23)",ylab="Electricity Load",main="B단지 가구 c의 시간별 전력사용량",ylim=c(0.2,0.8),axes=F)
axis(1,at=0:23)
axis(2,ylim=c(0.2,0.8))


c2<-aggregate(data_plot[,2000],by=list(data_plot$hour),FUN=mean,na.rm=T)
plot(c2$Group.1,c2$x,type="o",xlab="Hour(0~23)",ylab="Electricity Load",main="B단지 가구 d의 시간별 전력사용량",ylim=c(0.2,0.8),axes=F)
axis(1,at=0:23)
axis(2,ylim=c(0.2,0.8))


#### plot 6 : sample가구 요일별 전력 사용량

c1<-aggregate(data_plot[,50],by=list(data_plot$day),FUN=mean,na.rm=T)
c1<-c1[c(4,7,3,2,1,6,5),] ; rownames(c1)<-NULL
plot(c1$x,type="o",xlab="Day(Mon~Sun)",ylab="Electricity Load",main="B단지 가구 c의 요일별 전력사용량",ylim=c(0.25,0.6),axes=F)
axis(1,at=1:7,labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
axis(2,ylim=c(0.37,0.6))

c2<-aggregate(data_plot[,2000],by=list(data_plot$day),FUN=mean,na.rm=T)
c2<-c2[c(4,7,3,2,1,6,5),] ; rownames(c2)<-NULL
plot(c2$x,type="o",xlab="Day(Mon~Sun)",ylab="Electricity Load",main="D단지 가구 d의 요일별 전력사용량",ylim=c(0.25,0.60),axes=F)
axis(1,at=1:7,labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
axis(2,ylim=c(0.37,0.60))


### plot 7 : sample가구 월별 전력 사용량

full_data_with_time$month<-substr(rownames(full_data_with_time),6,7)

data_plotm<-aggregate(full_data_with_time[,-c(2630:2632)],by=list(full_data_with_time$month),FUN=mean,na.rm=T)
plot(data_plotm[,50],type="o",xlab="Month(1~12)",ylab="Electricity Load",main="A단지 가구 a의 월별 전력사용량",ylim=c(0.25,0.7),axes=F)
axis(1,at=1:12)
axis(2,ylim=c(0.25,0.7))

plot(data_plotm[,2000],type="o",xlab="Month(1~12)",ylab="Electricity Load",main="A단지 가구 b의 월별 전력사용량",ylim=c(0.25,0.7),axes=F)
axis(1,at=1:12)
axis(2,ylim=c(0.25,0.7))
