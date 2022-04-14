##############################################################################
############################ Outlier #########################################
##############################################################################
#### data merge
data_long<-rbind(complex1_long,complex2_long) # sum(is.na) = 4416
####기간 조정 2018-05-01 00:00:00 ~
data_long<-arrange(data_long,시간)
num<-which(data_long$시간==ymd_hms("18-05-01 00:00:00"))[1]
data_long<-data_long[c(num:dim(data_long)[1]),] # sum(is.na) = 0

#### Outlier - z-score
library(VGAM)
library(car)
library(bestNormalize)
library(outliers)

data_out<-data_long # long form data 넣기
data_trans<-yeojohnson(data_out$사용량,eps=0.01,standardize=TRUE)

outnum_z<-c(which(data_trans$x.t>2))
data_out$사용량[c(outnum_z)]<-NA
summary(data_out)

#### Outlier - IQR method
library(lubridate)
library(zoo)

data_out<-data_long # long form data 넣기
data_out$사용량[which(data_out$사용량 
                   > fivenum(data_out$사용량)[4] + 1.5*IQR(data_out$사용량,na.rm=T))] = NA
#boxplot(data_out$사용량)
#summary(data$사용량)

##############################################################################
############################ cumulative sum ##################################
##############################################################################
#### long to wide form
# after detecting outlier & long form data
data_wide<-data_out[,-1] %>% 
  pivot_wider(id_cols=c(동,호,serial_number),
              names_from=시간,values_from=사용량) 

#### NA 처리 : linear interpolation
library(zoo)
data_wide[,-c(1:3)] <- t(apply(data_wide[,-c(1:3)],1,na.approx,na.rm=F))
data_wide[,-c(1:3)] <- t(apply(data_wide[,-c(1:3)],1,na.locf,na.rm=F))
data_wide[,-c(1:3)] <- t(apply(data_wide[,-c(1:3)],1,na.locf,fromLast=T))

#### wide to long(NA 처리 후)
data_long<-data_wide %>% 
  pivot_longer(cols=-c(동,호,serial_number),
               names_to="시간", values_to="사용량")

#### cumulative sum
data_cum<-data_wide
data_cum[,-c(1:3)]<-t(apply(data_cum[,-c(1:3)],1,cumsum))

### graph : original data와 cumulative sum data 비교 
par(mfcol=c(2,4))
for (i in c(1,5,14,23)){
  ### original value
  plot(t(complex2_wide_day[i,-c(1:3)]),type="l",xaxt="n",ylim=c(0,0.9),
       xlab="Time",ylab="AMI value",
       main=paste(complex2_wide_day[i,1],"동 ",complex2_wide_day[i,2],"호"))
  axis(1,at=c(1,8017,16801,25609),lab=c("2018 1","2018 12","2019 12","2020 12"))
  ### cumsum value
  plot(t(data_cum[i,-c(1:3)]),type="l",xaxt="n",ylim=c(0,11000),
       xlab="Time",ylab="AMI value")
  axis(1,at=c(1,8017,16801,25609),lab=c("2018 1","2018 12","2019 12","2020 12"))
}
