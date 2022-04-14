library(readr)
library(doParallel)
library(foreach)
library(dplyr)

#### 코어 사용 개수 지정, 메모리 리밋 풀기
n.cores<-detectCores() - 1 ; s.cluster<-makeCluster(n.cores)      
registerDoParallel(s.cluster)
memory.limit(50000000000) ; gc()


##################### load the data for NA counting #######################

dir<-("C:\\Users\\LYJ_lab\\Desktop\\이지선\\에기평\\imcdata_v2")
folder_list<-list.files(dir) ; folder_num<-length(folder_list)
file_list<-c() ; na_num<-c() ; na_per<-c() ; apt_list<-list()

for(i in 1:folder_num)  {  
  
  fdir<-(paste(dir,folder_list[i],sep="/"))
  file_list<-list.files(fdir)
  f.num<-length(file_list)
  
  tmp<-foreach(j = 1:f.num,.packages = "readr",.combine = "rbind") %dopar% {
    
    temp<-read_csv(paste(fdir,file_list[j],sep="/"),col_names=TRUE)
    return(temp)
  }
  
  ############################### NA count 변수 추가 ##############################
  ## 총 Timepoint = 1217일(29208 시간)
  
  tmp2 <- tmp %>% group_by(House) %>% count() %>% data.frame()             
  tmp2$na_num <- 29208-tmp2$n ; tmp2$na_per<-tmp2$na_num/29208
  apt_list[[i]] <- tmp2
  rm(tmp) ; rm(tmp2)
  
  print(i)
}

save(apt_list,file="C:\\Users\\LYJ_lab\\Desktop\\이지선\\에기평\\apt_list.RData")


################### 전체 데이터에서 100개 가구 이름 샘플링 ########################
# load(file="C:\\Users\\LYJ_lab\\Desktop\\이지선\\에기평\\apt_list.RData")
set.seed(2020120110)
temp<-list()

for(i in 1:206) {
  temp[[i]]<-unique(apt_list[[i]]$House)           
}

temp<-unlist(temp) ; r.house<-sample(temp,100)

################ 가구 샘플링 토대로 파일 불러오기 #######################


dir<-("C:\\Users\\LYJ_lab\\Desktop\\이지선\\에기평\\imcdata_v2")
folder_list<-list.files(dir) ; folder_num<-length(folder_list)  
testdata<-list() ; temp<-temp2<-tmp<-data.frame()


for(i in 1:folder_num)  {  
  
  fdir<-(paste(dir,folder_list[i],sep="\\"))
  file_list<-list.files(fdir)
  f.num<-length(file_list)
  
  tmp<-foreach(j = 1:f.num,.packages = "readr",.combine = "rbind") %dopar% {
    
    temp<-read_csv(paste(fdir,file_list[j],sep="\\"),col_names=TRUE)
    temp2<-subset(temp,House %in% r.house)
    tmp<-rbind(tmp,temp2) ; return(tmp)
  }      
  
  tmp<-as.data.frame(tmp)
  testdata[[i]]<-tmp ; tmp<-c()
  print(i)
}

################ NULL list 정리 ########################

for(i in 1:206){
  if(nrow(testdata[[i]])==0){
    testdata[[i]]<-NULL
  }
}

############### 가구 수 100개 확인 #####################

a<-0 ; s<-0
for(i in 1:58) {
  a<-length(unique(testdata[[i]]$House))
  s<-a+s ; a<-0
  print(s)
}

save(testdata,file="C:\\Users\\LYJ_lab\\Desktop\\이지선\\에기평\\샘플링\\test100.RData")
