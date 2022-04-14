##############################################################################
############################ preprocessing ###################################
##############################################################################
library(doParallel)
library(foreach)
library(tidyverse)
library(doBy)
library(lubridate)

#### 코어 사용 개수 지정, 메모리 리밋 풀기
n.cores<-detectCores() - 1 ; s.cluster<-makeCluster(n.cores)
registerDoParallel(s.cluster)
memory.limit(500000000000000) ; gc()

##############################################################################
##############################################################################
#### 데이터 합치기 - 단지 1
dir<-("D:\\에기평\\MONGODB DATA\\Project\\data\\다산자연앤이편한세상")
file_list<-list.files(dir) ; file_num<-length(file_list) 
data<-data.frame() ; data_list<-list() ; n<-length(file_list)
tmp<-foreach(j = 1:file_num,.packages = "data.table",.combine = "rbind") %dopar% {    ### doparallel ###readr :: read_csv
  temp<-fread(paste(dir,file_list[j],sep="\\"),sep=",",header=TRUE,encoding="UTF-8")
  print(j)
  return(temp)
}

#### 필요없는 열 제거
complex1<-tmp[,-c(25:26,28)] 
names(complex1)[25]<-"id"
names(complex1)[29]<-"serial_number"

#### 날짜 맞추기
complex1<-complex1 %>%
  unite(dongho,dong,ho,sep="-")

##5110-1601 2017-10-02 ~ 2018-01-01 공백
with(complex1,tapply(date,dongho,summary))

### 중복 가구 제거
complex1<-complex1[!duplicated(complex1[,c(26,27)]),]

### 공백 채우기
d<-seq(as.Date("2017-10-02"),as.Date("2018-01-01"),by=1)
t<-data.frame(matrix(NA,nrow=length(d),ncol=25),
              date=d,
              dongho=rep("5110-1601",length(d)),
              serial_number=rep("19028",length(d)))
names(t)[1:25]<-c(seq(0,23,1),"id")

complex1<-rbind(t,complex1)
complex1<-orderBy(~dongho,complex1)
rownames(complex1)=NULL

#### wide to long form
complex1_long<-complex1 %>% 
  pivot_longer(cols=-c(id,date,dongho,serial_number),
               names_to="시간", values_to="사용량")
complex1_long<-complex1_long %>%
  separate(dongho,sep="-",into=c("동","호"))

complex1_long<-complex1_long %>% 
  unite(시간, date, 시간,sep=" ")
complex1_long$시간<-ymd_h(complex1_long$시간)

#### write.csv - 다산 자연앤 이편한세상
write.csv(complex1_long,file="./다산자연앤이편한세상_long.csv",
          row.names=FALSE)

##############################################################################
##############################################################################
#### 데이터 합치기 - 수원권선꿈에그린
dir<-("D:\\에기평\\MONGODB DATA\\Project\\data\\수원권선꿈에그린")
file_list<-list.files(dir) ; file_num<-length(file_list) 
data<-dup<-data.frame() ; data_list<-list() ; n<-length(file_list)
tmp<-foreach(j = 1:file_num,.packages = "data.table",.combine = "rbind") %dopar% {    ### doparallel ###readr :: read_csv
  temp<-fread(paste(dir,file_list[j],sep="\\"),sep=",",header=TRUE,encoding="UTF-8")
  print(j)
  return(temp)
}

#필요없는 열 제거
complex2<-tmp[,-c(25:26,28)] 
names(complex2)[25]<-"id"
names(complex2)[29]<-"serial_number"

#### 날짜 맞추기
## 그룹별 summary를 위해 dongho를 factor형태로 변경
complex2<-complex2 %>%
  unite(dongho,dong,ho,sep="-")

# 공백 없음
with(complex2,tapply(date,dongho,summary))

### 중복 가구 제거
complex2<-complex2[!duplicated(complex2[,c(26,27)]),]

#### wide to long form
complex2_long<-complex2 %>% 
  pivot_longer(cols=-c(id,date,dongho,serial_number),
               names_to="시간", values_to="사용량")
complex2_long<-complex2_long %>%
  separate(dongho,sep="-",into=c("동","호"))

complex2_long<-complex2_long %>% 
  unite(시간, date, 시간,sep=" ")
complex2_long$시간<-ymd_h(complex2_long$시간)


#### write.csv - 수원권선꿈에그린
write.csv(complex2_long,file="./수원권선꿈에그린_long.csv",
          row.names=FALSE)
