### Outlier Detection
### Quantile method
Q1<-Q3<-LC<-UC<-c() ; full_data_with_time2<-data.frame()

for(i in 1:4051) {
  # 1분위수 계산
  Q1[i] = quantile(full_data_with_time[,i],probs = c(0.25),na.rm = TRUE) 
  # 3분위수 계산
  Q3[i] = quantile(full_data_with_time[,i],probs = c(0.75),na.rm = TRUE) 
  
  LC[i] = Q1[i] - 1.5 * (Q3[i] - Q1[i]) # 아래 울타리
  UC[i] = Q3[i] + 1.5 * (Q3[i] - Q1[i]) # 위 울타리
  
  full_data_with_time2[,i] = subset(full_data_with_time,full_data_with_time[,i] >  LC[i] & full_data_with_time[,i] < UC[i])
}

### boxplot
boxplot(full_data_with_time$V1)
