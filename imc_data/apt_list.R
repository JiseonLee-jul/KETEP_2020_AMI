
start.time <- Sys.time()

dir<-("D:\\1-50")

folder_list<-list.files(dir) ; folder_num<-length(folder_list)
file_list<-c() ; na_num<-c() ; na_per<-c() ; apt_list<-list()
# zero_num<-c() ; 
zero_per<-c()
# kwmin<-c() ; kwmax<-c()
# startdate<-c() ; enddate<-c()

for(i in 1:folder_num)  {  
  fdir<-(paste(dir,folder_list[i],sep="/"))
  file_list<-list.files(fdir)
  f.num<-length(file_list)
  tmp<-foreach(j = 1:f.num,.packages = "readr",.combine = "rbind") %dopar% {
    temp<-read_csv(paste(fdir,file_list[j],sep="/"),col_names=TRUE)
    return(temp)
  }
  ##################### NA ########################1217일 ##29208 시간
  # a<-summary(tmp)
  sdate<-as.numeric(sub('.*:', '', summary(tmp)[1,4]))  # 20181015 in first sample
  edate<-as.numeric(sub('.*:', '', summary(tmp)[6,4]))  # 20190730 in first sample
  survey <- data.frame(tx_start=sdate, date=edate)
  survey$date_diff <- as.Date(as.character(survey$date), format="%Y%m%d")-
    as.Date(as.character(survey$tx_start), format="%Y%m%d")
  b<-survey$date_diff[1]
  testdate<-as.numeric(gsub("Time difference of ", "", b))
  testn<-testdate*24
  
  dim(tmp)[1]   # 4718 in first sample
  testsum<-sum(tmp$KW==0)
  
  testmin<-as.numeric(sub('.*:', '', summary(tmp)[1,6]))
  testmax<-as.numeric(sub('.*:', '', summary(tmp)[6,6]))
  
  tmp2<-tmp %>% group_by(House) %>% summarize(counts=n(), sdate=min(Date), edate=max(Date), minKW=min(KW), maxKW=max(KW), zero_num=sum(KW==0)) %>% data.frame()             
  tmp2$zero_per <- tmp2$zero_num/dim(tmp)[1]*100
  tmp2$na_num <- testn-tmp2$counts ; tmp2$na_per<-tmp2$na_num/testn*100
  
  # tmp2<-tmp %>% group_by(House) %>% count() %>% data.frame()             
  # tmp2$startdate <- sdate ; tmp2$enddate <- edate
  # tmp2$zero_num <- testsum ; tmp2$zero_per <- testsum/dim(tmp)[1]*100
  # tmp2$na_num <- testn-tmp2$n ; tmp2$na_per<-tmp2$na_num/testn*100
  # tmp2$kwmin <- testmin ; tmp2$kwmax <- testmax
  
  apt_list[[i]]<-tmp2
  rm(tmp) ; rm(tmp2)
  print(i)
}
# apt_list

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# for(i in 1:folder_num)  {  
#   fdir<-(paste(dir,folder_list[i],sep="/"))
#   file_list<-list.files(fdir)
#   f.num<-length(file_list)
#   tmp<-foreach(j = 1:f.num,.packages = "readr",.combine = "rbind") %dopar% {
#     temp<-read_csv(paste(fdir,file_list[j],sep="/"),col_names=TRUE)
#     return(temp)
#   }
#   ##################### NA ########################1217일 ##29208 시간
#   tmp2<-tmp %>% group_by(House) %>% count() %>% data.frame()             
#   tmp2$na_num <- 29208-tmp2$n ; tmp2$na_per<-tmp2$na_num/29208
#   apt_list[[i]]<-tmp2
#   rm(tmp) ; rm(tmp2)
#   print(i)
# }

start.time <- Sys.time()
save(apt_list,file="C:/Users/Chan.DESKTOP-FVSMR1F/Desktop/Project/aptlist.RData")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

load(file="C:/Users/Chan.DESKTOP-FVSMR1F/Desktop/Project/aptlist.RData")

# ======================== summary ========================== #
k=2
apt_list[[k]]
#summary(apt_list[[k]])

house_n<-n<-startdate<-enddate<-zero_np<-zero_min<-zero_max<-na_np<-na_min<-na_max<-min<-max<-c() 
apt_list_new<-data.frame()

for(k in 1:50) {
  
  house_n[k]<-length(unique(apt_list[[k]]$House))
  n[k]<-sum(apt_list[[k]]$counts)
  startdate[k]<-as.numeric(sub('.*:', '', summary(apt_list[[k]])[1,3]))
  enddate[k]<-as.numeric(sub('.*:', '', summary(apt_list[[k]])[6,4]))
  zero_np[k]<-paste(sum(apt_list[[k]]$zero_num),"(",round(sum(apt_list[[k]]$zero_per),2),"%",")")
  #zero_p[k]<-round(sum(apt_list[[k]]$zero_per),2)
  # which.max(apt_list[[k]]$zero_per)   # 1 693
  # which.min(apt_list[[k]]$zero_per)   # 1 723
  zero_max[k]<-paste(round(apt_list[[k]][which.max(apt_list[[k]]$zero_per),c(8)],2),"%")
  zero_min[k]<-paste(round(apt_list[[k]][which.min(apt_list[[k]]$zero_per),c(8)],3),"%")
  na_np[k]<-paste(sum(apt_list[[k]]$na_num),"(",as.numeric(sub('.*:', '', summary(apt_list[[k]])[4,10])),"%",")")
  # na_p[k]<-as.numeric(sub('.*:', '', summary(apt_list[[k]])[4,10])) # mean of na_per
  # mean(apt_list[[k]]$na_per)
  # apt_list[[k]]$na_per
  # which.max(apt_list[[k]]$na_per)   # 7 772
  # which.min(apt_list[[k]]$na_per)   # 6 483
  na_max[k]<-paste(round(apt_list[[k]][which.max(apt_list[[k]]$na_per),c(10)],2),"%")
  na_min[k]<-paste(round(apt_list[[k]][which.min(apt_list[[k]]$na_per),c(10)],3),"%")
  max[k]<-as.numeric(sub('.*:', '', summary(apt_list[[k]])[6,6]))
  min[k]<-as.numeric(sub('.*:', '', summary(apt_list[[k]])[1,5]))
  
  
}


apt_list_new<-data.frame(house_n,n,startdate,enddate,zero_np,zero_max,zero_min,na_np,na_max,na_min,max,min)
write.csv(apt_list_new,file="C:\\Users\\이지선\\Desktop\\에기평\\샘플자료_new\\apt_list_new.csv")
