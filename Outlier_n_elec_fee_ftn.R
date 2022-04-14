############################################################################
######################### Outlier Detection_ Z-score #######################
############################################################################

### data : 4 column data('House', 'Date', 'Hour', 'W')
names(data)[4]<-'W'
data_out<-data
data_out[which(data_out$W<0),"W"]<-NA

### 정규화 : Yeo-johnson Transformation
library(VGAM)
library(car)
library(bestNormalize)

data_trans<-yeojohnson(data_out$W,eps=0.01,standardize=TRUE)
str(data_trans)

### Z-score method(threshold = 3)
library(outliers)

outnum_z<-c(which(data_trans$x.t>3))
data_out$W[c(outnum_z)]<-NA
summary(data_out)

data_sp<-data_out


########################################################################
##################### 요금제 변수 추가 #################################
########################################################################

str(data_sp)

names(data_sp)[3]<-"Time" # 변수명 변경 : Hour -> Time
data_sp$Date<-as.character(data_sp$Date)

### 새로운 시계열 변수 생성
data_sp$KW_d<-data_sp$W/1000
data_sp$Hour<-as.integer(substr(data_sp$Time,1,2))
data_sp$Min<-as.integer(substr(data_sp$Time,4,5))
data_sp$Month<-as.integer(substr(data_sp$Date,6,7))
data_sp$Date2<-as.integer(substr(data_sp$Date,9,10))
data_sp$Year<-as.integer(substr(data_sp$Date,1,4))
data_sp$Day<-weekdays(as.Date(data_sp$Date))

### 요금 계산
library(dplyr)

### charge1 : 월별 전력사용량 데이터
charge1<-data_sp %>% group_by(House,Month,Year) %>% summarise(KW_diff=sum(W,na.rm=TRUE))
# KW_diff : W -> KW 단위 변경
charge1$KW_diff<-charge1$KW_diff/1000

### sys 1 : 누진 요금제 
sys1<-function(data=charge1,month=Month,elec=KW_diff){
  
  summer<-subset(charge1,Month=="7" |Month=="8")
  summer$charge<-ifelse(summer$KW_diff<=300,78.3*summer$KW_diff,
                        ifelse(summer$KW_diff<=450,78.3*300+147.3*(summer$KW_diff-300),
                               78.3*300+147.3*150+215.6*(summer$KW_diff-450)))
  etc<-subset(charge1,Month!="7" & Month!="8")
  etc$charge<-ifelse(etc$KW_diff<=200,78.3*etc$KW_diff,
                     ifelse(etc$KW_diff<=400,78.3*200+147.3*(etc$KW_diff-200),
                            78.3*200+147.3*200+215.6*(etc$KW_diff-400)))
  charge<-rbind(summer,etc)
  return(charge)
}

charge_ex<-sys1(data_sp,Month,KW_diff)
charge_ex$charge<-floor(charge_ex$charge)
# charge_ex : 누진 요금 계산 데이터 프레임
charge_ex<-as.data.frame(charge_ex)

### sys 2 : 계시별 요금제(일반형) 

sys2<-function(data=data_sp,month=Month,hour=Hour,elec=KW_d){
  
  summer<-subset(data_sp,Month=="6"|Month=="7"|Month=="8")
  summer$charge_new1<-ifelse(summer$Hour>=13 & summer$Hour<=17,188*summer$KW_d,
                             ifelse(summer$Hour>=0 & summer$Hour<=9 | summer$Hour==23,82*summer$KW_d,155*summer$KW_d))
  
  winter<-subset(data_sp,Month=="1"|Month=="2"|Month=="11"|Month=="12")
  winter$charge_new1<-ifelse(winter$Hour>=9 & winter$Hour<=12,159*winter$KW_d,
                             ifelse(winter$Hour>=0 & winter$Hour<=9 | winter$Hour==23,95*winter$KW_d,138*winter$KW_d))
  
  etc<-subset(data_sp,Month=="3"|Month=="4"|Month=="5"|Month=="9"|Month=="10")
  etc$charge_new1<-ifelse(etc$Hour>=9 & etc$Hour<=23,109*etc$KW_d,82*etc$KW_d)
  
  charge<-rbind(summer,winter,etc)
  return(charge)
}


charge_data_sp<-sys2(data_sp,Month,Hour,KW_d)

### sys 3 : 계시별 요금제(집중형)

sys3<-function(data=charge_data_sp,month=Month,hour=Hour,elec=KW_d){
  
  summer<-subset(charge_data_sp,Month=="6"|Month=="7"|Month=="8")
  summer$charge_new2<-ifelse(summer$Hour>=15 & summer$Hour<=17,316*summer$KW_d,
                             ifelse(summer$Hour>=0 & summer$Hour<=9 & summer$Hour==23,73*summer$KW_d,155*summer$KW_d))
  winter<-subset(charge_data_sp,Month=="1"|Month=="2"|Month=="11"|Month=="12")
  winter$charge_new2<-ifelse(winter$Hour>=9 & winter$Hour<=11,258*winter$KW_d,
                             ifelse(winter$Hour>=0 & winter$Hour<=9 & winter$Hour==23,94*winter$KW_d,138*winter$KW_d))
  etc<-subset(charge_data_sp,Month=="3"|Month=="4"|Month=="5"|Month=="9"|Month=="10")
  etc$charge_new2<-ifelse(etc$Hour>=9 & etc$Hour<=23,109*etc$KW_d,73*etc$KW_d)
  
  charge<-rbind(summer,winter,etc)
  return(charge)
}

charge_new<-sys3(data_sp,Month,Hour,KW_d)

### charge_new data에 누진 요금제 열 추가

charge_new$charge_ex<-if(charge_new$Month=="7"){
  ifelse(charge_new$KW_d<=300,78.3*charge_new$KW_d,
         ifelse(charge_new$KW_d<=450,78.3*300+147.3*(charge_new$KW_d-300),
                78.3*300+147.3*150+215.6*(charge_new$KW_d-450)))
} else if(charge_new$Month=="8"){
  ifelse(charge_new$KW_d<=300,78.3*charge_new$KW_d,
         ifelse(charge_new$KW_d<=450,78.3*300+147.3*(charge_new$KW_d-300),
                78.3*300+147.3*150+215.6*(charge_new$KW_d-450)))
}else {
  ifelse(charge_new$KW_d<=200,78.3*charge_new$KW_d,
         ifelse(charge_new$KW_d<=400,78.3*200+147.3*(charge_new$KW_d-200),
                78.3*200+147.3*200+215.6*(charge_new$KW_d-400)))
}

write.csv(charge_new,"charge_new.csv",row.names=F)  

### charge_new의 불필요한 시계열 변수 제거
t<-charge_new[,-c(3,7,8,9,10,11)]

## 시간별 전력 사용량
charge_hour_W<-t %>% group_by(House,Date,Hour) %>% 
  summarise(W=sum(W,na.rm=TRUE))
## 시간별 sys2 요금
charge_hour_c1<-t %>% group_by(House,Date,Hour) %>% 
  summarise(c1=sum(charge_new1,na.rm=TRUE))
## 시간별 sys3 요금
charge_hour_c2<-t %>% group_by(House,Date,Hour) %>% 
  summarise(c2=sum(charge_new2,na.rm=TRUE))
## 시간별 sys1 요금
charge_hour_ce<-t %>% group_by(House,Date,Hour) %>% 
  summarise(c_ex=sum(charge_ex,na.rm=TRUE))

## 일별 전력사용량, 3가지 요금제의 요금
charge_date<-t %>% group_by(House,Date) %>% 
  summarise(W=sum(W,na.rm=TRUE),KW=sum(KW_d,na.rm=TRUE),c1=sum(charge_new1,na.rm=TRUE),c2=sum(charge_new2,na.rm=TRUE),c_ex=sum(charge_ex,na.rm=TRUE))
#,KW=sum(KW_d,na.rm=TRUE)
