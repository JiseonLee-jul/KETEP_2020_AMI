library(data.table)
library(doParallel)
library(foreach)
library(dplyr)

library(bit64)

# getwd()

n.cores<-detectCores() - 1 ; s.cluster<-makeCluster(n.cores)    #### 코어 사용 개수 지정, 메모리 리밋 풀기

registerDoParallel(s.cluster)

memory.limit(50000000000) ; gc()


##################### load the data  ##############################
i=7 ; j=5


dir<-("D:/에기평/jbj_data/AMI_data")

folder_list<-list.files(dir) ; folder_num<-length(folder_list)
file_list<-c() ; apt_list<-list() ; dup<-list() ; t<-c()
#dup_list<-list()

for(i in 1:33)  {  
  
  fdir<-(paste(dir,folder_list[i],sep="/"))
  file_list<-list.files(fdir)
  f.num<-length(file_list)
  
  tmp<-foreach(j = 1:f.num,.packages = "data.table",.combine = "rbind") %dopar% {    ### doparallel
    
    temp<-fread(paste(fdir,file_list[j],sep="/"),sep=",",header=FALSE,encoding="UTF-8")
    
    t[j]<-strsplit(file_list[j],"_")[[1]][2]
    temp$V1<-paste(gsub(".txt","",t[j]),temp$V1,sep="")
    
    return(temp)
  }
  
  
  ############################## Deduplication ################################
  
  names(tmp)<-c("House","Date","Hour","KW","V5")   ### tmp variable name 변경
  
  dup[[i]]<-unique(tmp[duplicated(tmp[,c(1,2,3)]),1]) ### 중복 가구 찾기
  
  #dup_list[[i]]<-subset(tmp,tmp$House %in% dup[[i]]$House)  ### 중복 데이터만 저장 
  
  tmp<-tmp[!(tmp$House %in% dup[[i]]$House),] ### 중복데이터 제거
  
  ############################## apt_list ################################ 
  
  ### tmp$diff = 시간당 전력사용량 (KW : 누적 전력 사용량)
  tmp[,diff:=KW-shift(KW),by=House] 
  
  ### date_diff -> testdate : 전체 time point 개수
  date_diff<-as.Date(sub('.*:', '', summary(tmp)[6,2]))-as.Date(sub('.*:', '', summary(tmp)[1,2]))   
  testdate<-as.numeric(gsub("Time difference of ", "", date_diff))
  testn<-(testdate+1)*24*4
  
  tmp2 <- tmp %>% 
    group_by(House) %>% 
    summarize(counts=n(), sdate=min(Date), edate=max(Date), minKW=min(diff,na.rm=T), 
              maxKW=max(diff,na.rm=T), zero_num=sum(diff==0,na.rm=T)) %>% 
    data.frame()             
  tmp2$zero_per <- tmp2$zero_num/nrow(tmp2)*100
  tmp2$na_num <- testn-tmp2$counts ; tmp2$na_per<-tmp2$na_num/testn*100
  
  apt_list[[i]] <- tmp2
  rm(tmp) ; rm(tmp2)
  print(i)
  
}



############################### summary #################################

house_n<-n<-startdate<-enddate<-zero_np<-zero_min<-zero_max<-na_np<-na_min<-na_max<-min<-max<-c() 
apt_list_new<-data.frame()


for(k in 1:33) {
  
  ### 가구수
  house_n[k]<-length(unique(apt_list[[k]]$House)) 
  
  ###관측자료수
  n[k]<-sum(apt_list[[k]]$counts) 
  
  ### 기록시작시점
  startdate[k]<-as.character(sub('.*:', '', summary(apt_list[[k]])[1,3]))
  ### 기록종료시점
  enddate[k]<-as.character(sub('.*:', '', summary(apt_list[[k]])[6,4]))  
  
  ### 0값 개수(비율)
  zero_np[k]<-paste(sum(apt_list[[k]]$zero_num),"(",round(sum(apt_list[[k]]$zero_per),3),"%",")") 
  ### 0값 개수 최대인 가구의 0값 비율
  zero_max[k]<-paste(round(apt_list[[k]][which.max(apt_list[[k]]$zero_per),8],3),"%")  
  ### 0값 개수 최소인 가구의 0값 비율
  zero_min[k]<-paste(round(apt_list[[k]][which.min(apt_list[[k]]$zero_per),8],3),"%")  
  
  ### NA 개수(비율)
  na_np[k]<-paste(sum(apt_list[[k]]$na_num),"(",as.numeric(sub('.*:', '', summary(apt_list[[k]])[4,9])),"%",")")
  ### NA 개수 최소인 가구의 NA 비율
  na_max[k]<-paste(round(apt_list[[k]][which.max(apt_list[[k]]$na_per),10],3),"%")
  ### NA 개수 최대인 가구의 NA 비율
  na_min[k]<-paste(round(apt_list[[k]][which.min(apt_list[[k]]$na_per),10],3),"%") 
  
  ### 최대 전력사용량
  max[k]<-as.numeric(sub('.*:', '', summary(apt_list[[k]])[6,6]))
  ### 최소 전력사용량
  min[k]<-as.numeric(sub('.*:', '', summary(apt_list[[k]])[1,5]))  
  
}

apt_list_new<-data.frame(house_n,n,startdate,enddate,zero_np,zero_max,zero_min,na_np,na_max,na_min,max,min)

write.csv(apt_list_new, file="/home/cps/Desktop/STAT/SGT/apt_list_new.csv")








