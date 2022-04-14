##############################################################################
################## K-Means (first step) ######################################
##############################################################################
#### Determining the number of clusters
library(factoextra)
library(NbClust)
library(cluster)

### Elbow method
g1=fviz_nbclust(data_cum[,-c(1:3)], kmeans, method = "wss") +
  #geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")
### Silhouette method
g2=fviz_nbclust(data_cum[,-c(1:3)], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

library(gridExtra)
grid.arrange(g1,g2,nrow=1,ncol=2)

##### k-means clustering
set.seed(777)
clusters <- kmeans(select(data_cum, -c(동,호,serial_number)), centers = 3)
centers <- rownames_to_column(as.data.frame(clusters$centers), "cluster")

##### Visualization
graph_long <- data_wide %>% 
  mutate(cluster = clusters$cluster)  %>%
  pivot_longer(cols=-c(동,호,serial_number,cluster), names_to = "시간", values_to = "사용량")

# 시간 정보 저장
graph_long$시간<-as.POSIXlt(graph_long$시간)
# 일별 AMI데이터 합산
graph_long <- graph_long %>%
  group_by(동,호,
            day=unclass(graph_long$시간)$yday,cluster) %>%
  summarise(사용량=sum(사용량))
# 클러스터별 데이터로 변환 (AMI 평균)
graph_long<-graph_long %>%
  group_by(day,cluster) %>% summarise(사용량=mean(사용량))
# 클러스터 이름 변환
graph_long$cluster<-as.factor(graph_long$cluster)
levels(graph_long$cluster)<-c("cluster a","cluster b","cluster c")

# graph 
ggplot(graph_long,aes(x=day,y=사용량,group=factor(cluster))) +
  #facet_wrap(~cluster) +
  geom_line(aes(color=factor(cluster))) +
  labs(title = "K-means(k=3) in the first step")+
  theme_minimal()+
  #scale_x_date(breaks=datebreaks,labels=date_format("%Y %b"))+
  theme(legend.position=c(0.1,0.9),legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))



##############################################################################
################## K-Means (second step) #####################################
##############################################################################
#### 데이터 생성
cluster_wide<-data_wide %>% 
  mutate(cluster = clusters$cluster)
# 클러스터 이름 변환
cluster_wide$cluster<-as.factor(cluster_wide$cluster)
levels(cluster_wide$cluster)<-c("cluster a","cluster b","cluster c")

######################## cluster a - 2개로 분류 ##############################
a_data<-filter(cluster_wide,cluster=="cluster a") ; dim(a_data)[1]
#t<-apply(a_data[,-c(1:3,ncol(a_data))],1,max)
#a_clust<-a_data[,-c(1:3,ncol(a_data))]/t

#### Determining the number of clusters
g1=fviz_nbclust(a_data[,-c(1:3,ncol(a_data))], kmeans, method = "wss") +
  #geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method 
g2=fviz_nbclust(a_data[,-c(1:3,ncol(a_data))], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

library(gridExtra)
grid.arrange(g1,g2,nrow=1,ncol=2)

# 2개로 분류
set.seed(777)
clusters <- kmeans(a_data[,-c(1:3,ncol(a_data))], centers = 2)

# 분류 가구 수
length(which(clusters[["cluster"]] == 1))
length(which(clusters[["cluster"]] == 2))
##### Visualization
a_data<-a_data %>% 
  mutate(cluster = clusters$cluster)
a_data$cluster<-as.factor(a_data$cluster)
levels(a_data$cluster)<-c("cluster a1","cluster a2")
graph_long <- a_data %>%
  pivot_longer(cols=-c(동,호,serial_number,cluster), names_to = "시간", values_to = "사용량")

# 시간 정보 저장
graph_long$시간<-as.POSIXlt(graph_long$시간)
# 일별 AMI데이터 합산
graph_long <- graph_long %>%
  group_by(동,호,
            day=unclass(graph_long$시간)$year,
            day=unclass(graph_long$시간)$month,
            day=unclass(graph_long$시간)$yday,cluster) %>%
  summarise(사용량=sum(사용량))
# 클러스터별 데이터로 변환 (AMI 평균)
graph_long<-graph_long %>%
  group_by(day,cluster) %>% summarise(사용량=mean(사용량))
# 클러스터 이름 변환
graph_long$cluster<-as.factor(graph_long$cluster)
levels(graph_long$cluster)<-c("cluster a1","cluster a2")

#library(scales)
#datebreaks<- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="1 month")

g1=ggplot(graph_long,aes(x=day,y=사용량,group=factor(cluster))) +
  #facet_wrap(~cluster) +
  geom_line(aes(color=factor(cluster))) +
  labs(title = "K-means(k=2) for Cluster a")+
  theme_minimal()+
  #scale_x_date(breaks=datebreaks,labels=date_format("%Y %b"))+
  theme(legend.position=c(0.1,0.95),legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

################시간 단위 그래프
graph_long <- a_data %>%
  pivot_longer(cols=-c(동,호,serial_number,cluster), names_to = "시간", values_to = "사용량")

# 시간 정보 저장
graph_long$시간<-as.POSIXlt(graph_long$시간)
# 일별 AMI데이터 합산
graph_long <- graph_long %>%
  group_by(동,호,
            day=unclass(graph_long$시간)$yday,cluster,
            hour=unclass(graph_long$시간)$hour) %>%
  summarise(사용량=sum(사용량))
# 클러스터별 데이터로 변환 (AMI 평균)
graph_long<-graph_long %>%
  group_by(cluster,hour) %>% summarise(사용량=mean(사용량))
# 클러스터 이름 변환
graph_long$cluster<-as.factor(graph_long$cluster)
levels(graph_long$cluster)<-c("cluster a1","cluster a2")

#library(scales)
#datebreaks<- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="1 month")

g2=ggplot(graph_long,aes(x=hour,y=사용량,group=factor(cluster))) +
  #facet_wrap(~cluster) +
  geom_line(aes(color=factor(cluster))) +
  labs(title = "K-means(k=2) for Cluster a")+
  theme_minimal()+
  #scale_x_date(breaks=datebreaks,labels=date_format("%Y %b"))+
  theme(legend.position=c(0.1,0.95),legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

grid.arrange(g1,g2,nrow=1,ncol=2)

######################### cluster b - 2개로 분류 #############################
# 클러스터 이름 변환
b_data<-filter(cluster_wide,cluster=="cluster b") ; dim(b_data)[1]
#### Determining the number of clusters
g1=fviz_nbclust(b_data[,-c(1:3,ncol(b_data))], kmeans, method = "wss") +
  #geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
g2=fviz_nbclust(b_data[,-c(1:3,ncol(b_data))], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

library(gridExtra)
grid.arrange(g1,g2,nrow=1,ncol=2)

# 2개로 분류
set.seed(777)
clusters <- kmeans(b_data[,-c(1:3,ncol(b_data))], centers = 2)

length(which(clusters[["cluster"]] == 1))
length(which(clusters[["cluster"]] == 2))
##### Visualization
b_data<-b_data %>% 
  mutate(cluster = clusters$cluster)
b_data$cluster<-as.factor(b_data$cluster)
levels(b_data$cluster)<-c("cluster b1","cluster b2")
graph_long <- b_data %>%
  pivot_longer(cols=-c(동,호,serial_number,cluster), names_to = "시간", values_to = "사용량")

# 시간 정보 저장
graph_long$시간<-as.POSIXlt(graph_long$시간)
# 일별 AMI데이터 합산
graph_long <- graph_long %>%
  group_by(동,호,
            day=unclass(graph_long$시간)$yday,cluster) %>%
  summarise(사용량=sum(사용량))
# 클러스터별 데이터로 변환 (AMI 평균)
graph_long<-graph_long %>%
  group_by(day,cluster) %>% summarise(사용량=mean(사용량))
# 클러스터 이름 변환
graph_long$cluster<-as.factor(graph_long$cluster)
levels(graph_long$cluster)<-c("cluster b1","cluster b2")

#library(scales)
#datebreaks<- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="1 month")

g1=ggplot(graph_long,aes(x=day,y=사용량,group=factor(cluster))) +
  #facet_wrap(~cluster) +
  geom_line(aes(color=factor(cluster))) +
  labs(title = "K-means(k=2) for Cluster b")+
  theme_minimal()+
  #scale_x_date(breaks=datebreaks,labels=date_format("%Y %b"))+
  theme(legend.position=c(0.1,0.95),legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

################시간 단위 그래프
graph_long <- b_data %>%
  pivot_longer(cols=-c(동,호,serial_number,cluster), names_to = "시간", values_to = "사용량")

# 시간 정보 저장
graph_long$시간<-as.POSIXlt(graph_long$시간)
# 일별 AMI데이터 합산
graph_long <- graph_long %>%
  group_by(동,호,
            day=unclass(graph_long$시간)$yday,cluster,
            hour=unclass(graph_long$시간)$hour) %>%
  summarise(사용량=sum(사용량))
# 클러스터별 데이터로 변환 (AMI 평균)
graph_long<-graph_long %>%
  group_by(cluster,hour) %>% summarise(사용량=mean(사용량))
# 클러스터 이름 변환
graph_long$cluster<-as.factor(graph_long$cluster)
levels(graph_long$cluster)<-c("cluster b1","cluster b2")

#library(scales)
#datebreaks<- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="1 month")

g2=ggplot(graph_long,aes(x=hour,y=사용량,group=factor(cluster))) +
  #facet_wrap(~cluster) +
  geom_line(aes(color=factor(cluster))) +
  labs(title = "K-means(k=2) for Cluster b")+
  theme_minimal()+
  #scale_x_date(breaks=datebreaks,labels=date_format("%Y %b"))+
  theme(legend.position=c(0.1,0.95),legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

grid.arrange(g1,g2,nrow=1,ncol=2)


########################### cluster c - 분류x ###############################
# 클러스터 이름 변환
c_data<-filter(cluster_wide,cluster=="cluster c") ; dim(c_data)[1]

#### Determining the number of clusters
g1=fviz_nbclust(c_data[,-c(1:3,ncol(c_data))], kmeans, method = "wss") +
  #geom_vline(xintercept = 2, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
g2=fviz_nbclust(c_data[,-c(1:3,ncol(c_data))], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

library(gridExtra)
grid.arrange(g1,g2,nrow=1,ncol=2)

###############################################################################
####################### cluster 전체 묶은 데이터 ##############################
cluster_wide<-rbind(a_data,b_data,c_data)
library(gdata)
cluster_wide$cluster<-drop.levels(cluster_wide$cluster)

graph_long <- cluster_wide %>%
  pivot_longer(cols=-c(동,호,serial_number,cluster), names_to = "시간", values_to = "사용량")

# 시간 정보 저장
graph_long$시간<-as.POSIXlt(graph_long$시간)
# 일별 AMI데이터 합산
graph_long <- graph_long %>%
  group_by(동,호,
            day=unclass(graph_long$시간)$yday,cluster) %>%
  summarise(사용량=sum(사용량))
# 클러스터별 데이터로 변환 (AMI 평균)
graph_long<-graph_long %>%
  group_by(day,cluster) %>% summarise(사용량=mean(사용량))
# 클러스터 이름 변환
graph_long$cluster<-as.factor(graph_long$cluster)

#library(scales)
#datebreaks<- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="1 month")

g1=ggplot(graph_long,aes(x=day,y=사용량,group=factor(cluster))) +
  #facet_wrap(~cluster) +
  geom_line(aes(color=factor(cluster))) +
  labs(title = "Two-step K-means")+
  theme_minimal()+
  #scale_x_date(breaks=datebreaks,labels=date_format("%Y %b"))+
  theme(legend.position=c(0.1,0.85),legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

################시간 단위 그래프
graph_long <- cluster_wide %>%
  pivot_longer(cols=-c(동,호,serial_number,cluster), names_to = "시간", values_to = "사용량")

# 시간 정보 저장
graph_long$시간<-as.POSIXlt(graph_long$시간)
# 일별 AMI데이터 합산
graph_long <- graph_long %>%
  group_by(동,호,
            day=unclass(graph_long$시간)$yday,cluster,
            hour=unclass(graph_long$시간)$hour) %>%
  summarise(사용량=sum(사용량))
# 클러스터별 데이터로 변환 (AMI 평균)
graph_long<-graph_long %>%
  group_by(hour,cluster) %>% summarise(사용량=mean(사용량))
# 클러스터 이름 변환
graph_long$cluster<-as.factor(graph_long$cluster)


#library(scales)
#datebreaks<- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="1 month")

g2=ggplot(graph_long,aes(x=hour,y=사용량,group=factor(cluster))) +
  #facet_wrap(~cluster) +
  geom_line(aes(color=factor(cluster))) +
  labs(title = "Two step K-means")+
  theme_minimal()+
  #scale_x_date(breaks=datebreaks,labels=date_format("%Y %b"))+
  theme(legend.position=c(0.1,0.95),legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=30, hjust=1))

library(gridExtra)
grid.arrange(g1,g2,nrow=1,ncol=2)












