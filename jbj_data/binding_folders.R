library(data.table)
library(doParallel)
library(foreach)
library(dplyr)

# getwd()
##### 코어 사용 개수 지정, 메모리 리밋 풀기
n.cores<-detectCores() - 1 ; s.cluster<-makeCluster(n.cores)    

registerDoParallel(s.cluster)

memory.limit(500000000000000) ; gc()

##### 데이터 디렉토리 지정
dir<-("D:\\에기평\\jbj_data\\AMI_data")

folder_list<-list.files(dir) ; folder_num<-length(folder_list) 
file_list<-t<-c() ; apt_list<-dup<-list()
data<-data.frame() ; data_list<-list()

# dup_list<-list() : 중복데이터 저장할 때 생성 필요한 리스트

for(i in 1:33){
  
  fdir<-(paste(dir,folder_list[i],sep="/"))
  file_list<-list.files(fdir)
  f.num<-length(file_list)
  aptnum<-strsplit(folder_list[i],". ")[[1]][1]
  
  # 각 폴더의 데이터 하나의 데이터 프레임으로 병합(rbind)
  data<-foreach(j = 1:f.num,.packages = "data.table",.combine = "rbind") %dopar% {    ### doparallel ###readr :: read_csv
    
    temp<-fread(paste(fdir,file_list[j],sep="/"),sep=",",header=FALSE,encoding="UTF-8")
    
    t[j]<-strsplit(file_list[j],"_")[[1]][2]
    temp$V1<-paste(aptnum,gsub(".txt","",t[j]),temp$V1,sep="_")
    
    return(temp)
  }
  
  
  ############################## Deduplication ################################
  ### tmp variable name 변경
  names(data)<-c("House","Date","Hour","KW","V5") 
  
  ### 중복 가구 찾기
  dup[[i]]<-unique(data[duplicated(data[,c(1,2,3)]),1]) 
  
  ### 중복 데이터만 저장
  #dup_list[[i]]<-subset(tmp,tmp$House %in% dup[[i]]$House)   
  
  ### 중복데이터 제거
  data<-data[!(data$House %in% dup[[i]]$House),] 
  
  ############################## apt_list ################################ 
  
  ### 그룹별 전력사용량 : KW가 누적 전력 사용량이기 때문에 각 시간별 전력사용량 데이터로 바꿔주기 위해
  data[,diff:=KW-shift(KW),by=House]   
  data_list[[i]]<-data
  print(i)
}  
  

#### list to dataframe
temp <- data <- data.frame()

for(i in 1:length(data_list)){
  temp<-data_list[[i]]
  # 'KW', 'V5' 변수 분석에 사용 안 해서 용량을 위해 제거
  data<-rbind(temp[,-c(4,5)],data)
  print(i)
}
