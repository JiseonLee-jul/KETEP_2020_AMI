##############################################################################
############################ Graph : Outlier detection #######################
##############################################################################
#install.packages('tidyverse')
library(tidyverse)
#install.packages('lubridate')
library(lubridate)
library(zoo)
library(dplyr)
data <- complex2_long
### IQR method
# data$사용량[which(data$사용량 > fivenum(data$사용량)[4] + 1.5*IQR(data$사용량))]=NA
# boxplot(data$사용량)
# summary(data$사용량)
data$사용량=na.approx(data$사용량,na.rm=F)

### 시계열 데이터 그리기 위한 준비
date=as.Date(ymd_hms(data$시간))
data=cbind(data,date)
names(data)[7]='date'
#wday(very_first_game_of_kbo, label = TRUE)

### directory 생성 후 그래프 이미지 넣기
dir.create('수원권선그린_plot')
serial=unique(data[,"serial_number"])
Dmean=data %>% 
  group_by(serial_number,date) %>% 
  summarise(mean_day = mean(사용량))
Dmean=as.data.frame(Dmean)

for(i in 1:length(serial)){
  Dmean_2=Dmean[Dmean$serial_number==serial[i],]
  ### png 생성
  png(paste('수원권선그린_plot',paste(paste(serial[i],'day',sep='_'),'png',sep='.'),sep='/'),
      width=800)
  par(mfrow=c(1,2))
  ### graph 1 : 전체 time points 동안 평균 전력 소비량
  plot(1:dim(Dmean_2)[1],Dmean_2[,3],type='l',axes=F,xlab='날짜',ylab='평균전력소비량')
  axis(1,at=c(1,round(dim(Dmean_2)[1]/2),dim(Dmean_2)[1]),lab=Dmean_2[c(1,round(dim(Dmean_2)[1]/2),dim(Dmean_2)[1]),2])
  axis(2,ylim=c(0,1))
  ### graph 2 : 월별 평균 전력소비량
  Dmean_2[,4]=month(Dmean_2[,2])
  names(Dmean_2)[4]='month'
  Mmean=Dmean_2 %>% 
    group_by(month) %>% 
    summarise(mean_month = mean(mean_day))
  Mmean=as.data.frame(Mmean)
  barplot(Mmean[,2],names=c(1:12),xlab='월',ylab='평균전력소비량')
  dev.off()
}

### mean/max/min
summary1=Dmean %>% 
  group_by(serial_number) %>% 
  summarise(mean = mean(mean_day),
            max=max(mean_day),
            min=min(mean_day))
write.csv(summary1, "./수원권선꿈에그린_기초분석.csv",row.names=F)
