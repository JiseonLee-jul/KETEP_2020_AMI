##############################################################################
################## Survey Summary - K-Means ##################################
##############################################################################
####
library(dplyr)
library(reshape2)
library(ggplot2)

survey<-read.csv(choose.files(),header=TRUE)

#### data processing
## 가구원수
survey$가구원수<-apply(select(survey,cols=paste("DQ1",c(1:9),sep="")),1,
                   function(x) sum(!is.na(x)))
## 보유대수
q_num<-c(5,4,8,9) ; n<-dim(survey)[2] # n =194
for (i in 1:4){
  survey[,n+i]<-apply(select(survey,cols=paste("Q",i,"A1",(1:q_num[i]),sep="")),1,
                    sum,na.rm=TRUE)
}
names(survey)[c(195:198)]<-c("계절가전_보유대수","영상음향가전_보유대수","주방가전_보유대수","생활가전_보유대수")
## 일별 사용시간
# nan to 0
survey[-c(1:3)]<-mutate_all(survey[-c(1:3)], ~replace(.,is.nan(.),0))

q_num<-c(5,4,8,9) ; n<-dim(survey)[2] # n = 198
for (i in 1:4){
  for (j in 1:q_num[i]){
    prid<-select(survey,cols=paste("Q",i,"A2",j,sep=""))
    
    for (k in 1:nrow(prid)){
      if(prid[k,]==1){
        survey[k,n+j]<-select(survey[k,],cols=paste("Q",i,"A3",j,sep=""))
      } else if(prid[k,]==2){
        survey[k,n+j]<-round(select(survey[k,],cols=paste("Q",i,"A3",j,sep=""))/7,3)
      } else if(prid[k,]==3){
        survey[k,n+j]<-round(select(survey[k,],cols=paste("Q",i,"A3",j,sep=""))/30,3)
      } else{
        survey[k,n+j]<-data.frame(0)
      }
    }
  }
  n<-n+j
}

col_name<-c() ; n<-0
  for (i in 1:4){
    for(j in 1:q_num[i]){
        col_name[n+j]<-paste("Q",i,j,"_일별사용시간",sep="")
    }
    n<-n+j
  }
names(survey)[199:224]<-col_name
names(survey)[3]<-"serial_number"
### data merge
survey<-merge(survey,cluster_wide[c(3,23428)],by="serial_number")
# 0 to nan
survey[-c(1:3)]<-mutate_all(survey[-c(1:3)], ~replace(.,which(.==0),NA))

################################################################################
######################### Statistical Analysis - Q1~Q4 #########################
################################################################################
#####table Q1
#보유대수
round(with(survey,tapply(계절가전_보유대수,cluster,mean)),2)

#평균사용시간
for (i in 1:4){
  for(j in 1:q_num[i]){
    survey[,which(colnames(survey)==paste("Q",i,j,"_일별사용시간",sep=""))]=
      survey[,which(colnames(survey)==paste("Q",i,j,"_일별사용시간",sep=""))]*survey[,which(colnames(survey)==paste('Q',i,'A1',j,sep=''))]
  }
}

n_clus<-which(names(survey)=="cluster")
q1<-survey[c(n_clus,which(grepl("^Q1", names(survey)) &
                   grepl("_일별사용시간$", names(survey))))]
survey_gr<-q1 %>% 
  group_by(cluster) %>%
  summarise("에어컨"=mean(Q11_일별사용시간,na.rm=T),
            "가습기"=mean(Q12_일별사용시간,na.rm=T),
            "제습기"=mean(Q13_일별사용시간,na.rm=T),
            "전기장판/매트/담요"=mean(Q14_일별사용시간,na.rm=T),
            "선풍기/에어서큐레이터"=mean(Q15_일별사용시간,na.rm=T))
round(t(as.matrix(survey_gr[2:6])),2)

#####graph Q1
library(tidyr)
survey_gr<-survey_gr %>% 
  pivot_longer(cols=-cluster,names_to="q_name",values_to="value")

ggplot(survey_gr,aes(x=cluster,y=value,group=cluster)) +
  facet_grid(~q_name) +
  geom_bar(stat="identity",aes(fill=factor(cluster),color=factor(cluster))) +
  labs(title = "Average daily usage time for seasonal appliances by cluster")+
  theme_minimal()+
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

#####table Q2
#보유대수
round(with(survey,tapply(영상음향가전_보유대수,cluster,mean)),2)

#평균사용시간
n_clus<-which(names(survey)=="cluster")
q2<-survey[c(n_clus,which(grepl("^Q2", names(survey)) &
                            grepl("_일별사용시간$", names(survey))))]
survey_gr<-q2 %>% 
  group_by(cluster) %>%
  summarise("TV"=mean(Q21_일별사용시간,na.rm=T),
            "모니터"=mean(Q22_일별사용시간,na.rm=T),
            "오디오"=mean(Q23_일별사용시간,na.rm=T),
            "DVD 플레이어"=mean(Q24_일별사용시간,na.rm=T))

round(t(as.matrix(survey_gr[2:5])),2)

#####graph Q2
library(tidyr)
survey_gr<-survey_gr %>% 
  pivot_longer(cols=-cluster,names_to="q_name",values_to="value")

ggplot(survey_gr,aes(x=cluster,y=value,group=cluster)) +
  facet_grid(~q_name) +
  geom_bar(stat="identity",aes(fill=factor(cluster),color=factor(cluster))) +
  labs(title = "Average daily usage time for video home appliances by cluster")+
  theme_minimal()+
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))


#####table Q3
#보유대수
round(with(survey,tapply(주방가전_보유대수,cluster,mean)),2)

#평균사용시간
n_clus<-which(names(survey)=="cluster")
q3<-survey[c(n_clus,which(grepl("^Q3", names(survey)) &
                            grepl("_일별사용시간$", names(survey))))]
survey_gr<-q3 %>% 
  group_by(cluster) %>%
  summarise("냉장고"=mean(Q31_일별사용시간,na.rm=T),
            "김치냉장고"=mean(Q32_일별사용시간,na.rm=T),
            "전기밥솥"=mean(Q33_일별사용시간,na.rm=T),
            "전자오븐"=mean(Q34_일별사용시간,na.rm=T),
            "에어프라이어"=mean(Q35_일별사용시간,na.rm=T),
            "식기세척기/식기건조기"=mean(Q36_일별사용시간,na.rm=T),
            "전자레인지"=mean(Q37_일별사용시간,na.rm=T),
            "인덕션/하이라이트/하이브리드"=mean(Q38_일별사용시간,na.rm=T))
round(t(as.matrix(survey_gr[2:9])),2)

#####graph Q3
library(tidyr)
survey_gr<-survey_gr %>% 
  pivot_longer(cols=-cluster,names_to="q_name",values_to="value")

ggplot(survey_gr,aes(x=cluster,y=value,group=cluster)) +
  facet_grid(~q_name) +
  geom_bar(stat="identity",aes(fill=factor(cluster),color=factor(cluster))) +
  labs(title = "Average daily usage time for kitchen appliances by cluster")+
  theme_minimal()+
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

#####table Q4
#보유대수
round(with(survey,tapply(생활가전_보유대수,cluster,mean)),2)

#평균사용시간
n_clus<-which(names(survey)=="cluster")
q4<-survey[c(n_clus,which(grepl("^Q4", names(survey)) &
                            grepl("_일별사용시간$", names(survey))))]
survey_gr<-q4 %>% 
  group_by(cluster) %>%
  summarise("공기청정기"=mean(Q41_일별사용시간,na.rm=T),
            "세탁기"=mean(Q42_일별사용시간,na.rm=T),
            "세탁건조기"=mean(Q43_일별사용시간,na.rm=T),
            "의류관리기"=mean(Q44_일별사용시간,na.rm=T),
            "진공청소기"=mean(Q45_일별사용시간,na.rm=T),
            "핸드청소기"=mean(Q46_일별사용시간,na.rm=T),
            "데스크탑"=mean(Q47_일별사용시간,na.rm=T),
            "프린터"=mean(Q48_일별사용시간,na.rm=T),
            "가스보일러"=mean(Q49_일별사용시간,na.rm=T))

round(t(as.matrix(survey_gr[2:10])),2)

#####graph Q4
library(tidyr)
survey_gr<-survey_gr %>% 
  pivot_longer(cols=-cluster,names_to="q_name",values_to="value")

ggplot(survey_gr,aes(x=cluster,y=value,group=cluster)) +
  facet_grid(~q_name) +
  geom_bar(stat="identity",aes(fill=factor(cluster),color=factor(cluster))) +
  labs(title = "Average daily usage time for seasonal appliances by cluster")+
  theme_minimal()+
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

######################################################################
####################### Statistical Analysis - DQ1 ###################
######################################################################
########가구구성원 수
survey %>%
  group_by(cluster) %>%
  summarise(평균_가구원수=mean(가구원수))

########연령대 비율
survey_gagu<-survey[,c(n_clus,
                    grep("DQ1A2", names(survey)))]
survey_gagu<-survey_gagu %>%
  pivot_longer(cols=-cluster,names_to="DQ1",values_to="age")

####graph
survey_gr<-survey_gagu %>% 
  group_by(cluster,age) %>% summarise(count=n())
survey_gr<-filter(survey_gr,age!=0)
survey_gr<-right_join(survey_gr,
                     data.frame(age=rep(c(1:7),5),
                                cluster=rep(c("cluster a1","cluster a2","cluster b1","cluster b2","cluster c"),each=7)),
                     by=c("cluster","age"))

survey_gr<-mutate_all(survey_gr, ~replace(.,is.na(.),0))
all_num<-as.vector(with(survey_gr,tapply(count,cluster,sum)))

survey_gr<-arrange(survey_gr,cluster)
survey_gr$per<-round(survey_gr$count/(rep(all_num,each=7)),3)*100
survey_gr$age<-factor(survey_gr$age)
levels(survey_gr$age)=c("10대 미만","10대","20대",
                               "30대","40대","50대","60대 이상")
survey_gr$cluster<-factor(survey_gr$cluster)

ggplot(survey_gr,aes(x=age,y=per,group=cluster)) +
  facet_grid(~cluster) +
  geom_bar(stat="identity",aes(fill=factor(age),color=factor(age))) +
  labs(title = "Percentage by age of household members by cluster")+
  theme_minimal()+
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))


########################################################################
########################### Statistical Analysis - DQ2 #################
########################################################################
DQ21=survey %>%
  group_by(cluster) %>%
  summarise(소득분위=mean(DQ2A1))

ggplot(DQ21,aes(x=cluster,y=소득분위)) +
  geom_bar(stat="identity",aes(fill=factor(cluster),color=factor(cluster))) +
  labs(title = "Average annual imcome  by cluster")+
  theme_minimal()+
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))


DQ22=survey %>%
  group_by(cluster) %>%
  summarise(거주기간=mean(DQ2A3))

ggplot(DQ22,aes(x=cluster,y=거주기간)) +
  geom_bar(stat="identity",aes(fill=factor(cluster),color=factor(cluster))) +
  labs(title = "Average period of residence by cluster")+
  theme_minimal()+
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))


#######################################################################
##################### Statistical Analysis - DQ3 ######################
#######################################################################
# DQ3  1: LED X  2: LED o  3: 모름
survey[which(survey$DQ3==3),colnames(survey)=='DQ3']=1.5
survey$DQ3
DQ3=survey %>%
  group_by(cluster) %>%
  summarise(LED=mean(DQ3))

ggplot(DQ3,aes(x=cluster,y=LED)) +
  geom_bar(stat="identity",aes(fill=factor(cluster),color=factor(cluster))) +
  labs(title = "Whether 'LED' is installed  by cluster")+
  theme_minimal()+
  theme(legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

###################################
jubu<-c(which(survey[,colnames(survey)=='DQ1A32']==5),which(survey[,colnames(survey)=='DQ1A31']==5))
survey[jubu,226]<-1
survey[-jubu,226]=0
colnames(survey)[226]='주부'
jubu_per=survey %>%
  group_by(cluster) %>%
  summarise(주부비율=mean(주부))
